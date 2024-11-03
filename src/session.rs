use std::cell::{Cell, OnceCell, RefCell, UnsafeCell};
use std::collections::HashMap;
use std::mem;
use std::mem::ManuallyDrop;
use std::pin::Pin;
use std::sync::atomic::{AtomicU64, Ordering};
use std::thread::LocalKey;

/// A Cell type that only allows exclusive access to the contained value.
struct ExclusiveCell<T> {
    inner: UnsafeCell<T>,
    in_use: Cell<bool>,
}

impl<T> ExclusiveCell<T> {
    pub const fn new(value: T) -> ExclusiveCell<T> {
        ExclusiveCell {
            inner: UnsafeCell::new(value),
            in_use: Cell::new(false),
        }
    }

    /// Provides exclusive access to the contained value.
    pub fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut T) -> R,
    {
        assert!(!self.in_use.replace(true));

        // Ensures that `in_use` is reset even if `f` panics.
        struct Guard<'a>(&'a Cell<bool>);

        impl<'a> Drop for Guard<'a> {
            fn drop(&mut self) {
                self.0.set(false);
            }
        }

        let _guard = Guard(&self.in_use);

        // SAFETY: The `self.in_use` flag prevents reentrancy
        // and thus ensures exclusive access to the contained value.
        let value = unsafe { &mut *self.inner.get() };
        f(value)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct Key(u64);

impl Key {
    fn new() -> Key {
        static NEXT: AtomicU64 = AtomicU64::new(1);
        Key(NEXT.fetch_add(1, Ordering::Relaxed))
    }
}

struct Session {
    on_destroy: HashMap<Key, Box<dyn FnOnce()>>,
}

impl Session {
    fn new() -> Session {
        Session {
            on_destroy: HashMap::new(),
        }
    }

    fn add_on_destroy_callback(&mut self, key: Key, f: impl FnOnce() + 'static) {
        self.on_destroy.insert(key, Box::new(f));
    }

    fn remove_on_destroy_callback(&mut self, key: Key) {
        self.on_destroy.remove(&key);
    }
}

impl Drop for Session {
    fn drop(&mut self) {
        let on_destroy = mem::take(&mut self.on_destroy);
        on_destroy.into_values().for_each(|f| f());
    }
}

// `ManuallyDrop` ensures that `on_destroy` callbacks have exclusive
// access to their corresponding session-scoped values.
// This is not an issue, since all values will be dropped
// at the end of the thread's lifetime.
struct SessionSlot(ManuallyDrop<ExclusiveCell<Session>>);

impl SessionSlot {
    fn new() -> SessionSlot {
        SessionSlot(ManuallyDrop::new(ExclusiveCell::new(Session::new())))
    }

    fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut Session) -> R,
    {
        self.0.with(f)
    }

    fn reset(&self) {
        self.0.with(|session| *session = Session::new());
    }
}

thread_local! {
    static SESSION_SLOT: SessionSlot = SessionSlot::new();
}

pub fn reset_session() {
    SESSION_SLOT.with(|slot| slot.reset());
}

thread_local! {
    // Invariant: `0` only when there are no active borrows
    // of session-scoped variables.
    static REFS: Cell<usize> = const { Cell::new(0) };
}

// Safely increments the `REFS` counter.
// After calling this function, `REFS` is guaranteed to be greater than 0.
#[inline(always)]
fn inc_refs() {
    let mut refs = REFS.get();
    refs = refs.wrapping_add(1);
    if refs == 0 {
        unlikely_panic_on_overflow();
    }
    REFS.set(refs);
}

// Safely decrements the `REFS` counter.
#[inline(always)]
fn dec_refs() {
    // `REFS` must not be `0` here.
    debug_assert!(REFS.get() != 0);
    REFS.set(REFS.get() - 1);
}

// Simulates `unlikely(...)` on stable.
#[cold]
#[inline(always)]
fn unlikely_panic_on_overflow() {
    panic!();
}

/// A value tied to the session lifetime.
///
/// Every time the session is destroyed, it returns to the initialized state.
pub struct SessionScoped<T> {
    inner: Pin<Box<UnsafeCell<OnceCell<T>>>>,
    init: RefCell<Box<dyn FnMut() -> T>>,
    key: Key,
}

impl<T> SessionScoped<T> {
    #[doc(hidden)]
    pub fn new(init: impl FnMut() -> T + 'static) -> SessionScoped<T> {
        SessionScoped {
            inner: Box::pin(UnsafeCell::new(OnceCell::new())),
            init: RefCell::new(Box::new(init)),
            key: Key::new(),
        }
    }

    #[doc(hidden)]
    pub fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&T) -> R,
    {
        inc_refs();

        // Ensures that `dec_refs` is called even if `f` panics.
        struct Guard;

        impl Drop for Guard {
            fn drop(&mut self) {
                dec_refs();
            }
        }

        let _guard = Guard;

        // SAFETY:
        // * `self.inner` is used here and in `Self::reset`.
        // * `self.inner` is mutated only in `reset_session_scoped_value`.
        // * `reset_session_scoped_value` checks that `REFS` == 0.
        // * `inc_refs` ensures that `REFS` > 0.
        // * Hence, `REFS` > 0 excludes the possibility of having both
        //   mutable and immutable borrows of the value contained in
        //   `self.inner`, which is the case here.
        let inner = unsafe { &*self.inner.get() };

        f(inner.get_or_init(|| {
            // This will only be called once per session,
            // so we do not need to optimize it as much.
            let mut init = self.init.borrow_mut();
            init()
        }))
    }
}

impl<T: 'static> SessionScoped<T> {
    pub fn attach_to_session(&self) {
        let inner_ptr = self.inner.as_ref().get_ref() as *const UnsafeCell<OnceCell<T>>;

        SESSION_SLOT.with(|slot| {
            slot.with(|session| {
                session.add_on_destroy_callback(self.key, move || {
                    // SAFETY:
                    // * `Pin` ensures that `self.inner` will not be moved.
                    // * `SessionScoped::drop` prevents this callback from being called.
                    let inner = unsafe { &*inner_ptr };
                    Self::reset(inner);
                })
            });
        })
    }

    fn reset(inner: &UnsafeCell<OnceCell<T>>) {
        assert_eq!(REFS.get(), 0);
        // SAFETY: `REFS` == 0 ensures that we have exclusive access to `self.inner`.
        let inner = unsafe { &mut *inner.get() };
        inner.take();
    }
}

impl<T> Drop for SessionScoped<T> {
    fn drop(&mut self) {
        SESSION_SLOT.with(|slot| {
            slot.with(|session| session.remove_on_destroy_callback(self.key));
        })
    }
}

pub fn with_session<T, F, R>(value: &'static LocalKey<SessionScoped<T>>, f: F) -> R
where
    F: FnOnce(&T) -> R,
{
    value.with(|value| value.with(f))
}

#[macro_export]
macro_rules! session_scoped {
    () => {};
    ($vis:vis static $name:ident: $t:ty = $init:expr; $($rest:tt)*) => {
        thread_local! {
            $vis static $name: $crate::session::SessionScoped<$t> = {
                let value = $crate::session::SessionScoped::new(|| $init);
                value.attach_to_session();
                value
            }
        }

        session_scoped!($($rest)*);
    };
}

#[cfg(test)]
mod test {
    use std::cell::Cell;

    use crate::session::{reset_session, with_session};

    #[test]
    fn session_scoped_create() {
        session_scoped! {
            static VALUE: Box<str> = "string!".to_owned().into_boxed_str();
        }

        with_session(&VALUE, |v| assert_eq!(v.as_ref(), "string!"));
    }

    #[test]
    fn session_scoped_mutate() {
        session_scoped! {
            static VALUE: Cell<i32> = Cell::new(11);
        }

        with_session(&VALUE, |v| assert_eq!(v.get(), 11));

        with_session(&VALUE, |v| v.set(23));
        with_session(&VALUE, |v| assert_eq!(v.get(), 23));
    }

    #[test]
    fn session_scoped_reset() {
        thread_local! {
            static FLAG: Cell<bool> = const { Cell::new(false) };
        }

        struct MyCustomType(Cell<i32>);

        impl Drop for MyCustomType {
            fn drop(&mut self) {
                FLAG.set(true);
            }
        }

        session_scoped! {
            static VALUE: MyCustomType = MyCustomType(Cell::new(11));
        }

        with_session(&VALUE, |v| assert_eq!(v.0.get(), 11));

        with_session(&VALUE, |v| v.0.set(23));
        with_session(&VALUE, |v| assert_eq!(v.0.get(), 23));

        reset_session();

        assert!(FLAG.get(), "destructor has not run");
        with_session(&VALUE, |v| assert_eq!(v.0.get(), 11));
    }
}
