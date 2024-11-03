use bumpalo::Bump;
use elsa::FrozenIndexSet;
use std::fmt::{Display, Formatter};

use crate::session::with_session;
use crate::session_scoped;

session_scoped! {
    static INTERNER: Interner = Interner::new();
}

type SymbolIndex = u32;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct Symbol {
    index: SymbolIndex,
}

impl Symbol {
    fn new(index: SymbolIndex) -> Self {
        Self { index }
    }

    pub fn from_str(value: &str) -> Symbol {
        with_session(&INTERNER, |interner| interner.intern(value))
    }

    pub fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&str) -> R,
    {
        with_session(&INTERNER, |interner| f(interner.get(*self).unwrap()))
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.with(|value| f.write_str(value))
    }
}

struct Interner {
    bump: Bump,
    lookup: FrozenIndexSet<&'static str>,
}

impl Interner {
    fn new() -> Self {
        Self {
            bump: Bump::new(),
            lookup: FrozenIndexSet::new(),
        }
    }

    fn intern(&self, str: &str) -> Symbol {
        if let Some(index) = self.lookup.get_index_of(str) {
            return Symbol::new(index as SymbolIndex);
        }

        // Allocate a new string slice
        let str: &str = self.bump.alloc_str(str);

        // SAFETY: the value pointed by `str` is owned by the arena and
        // is only accessed while the arena is active.
        let str: &'static str = unsafe { &*(str as *const str) };

        // Register the newly created string with the interner.
        let (index, _) = self.lookup.insert_full(str);

        Symbol::new(index as SymbolIndex)
    }

    fn get(&self, symbol: Symbol) -> Option<&str> {
        self.lookup.get_index(symbol.index as usize)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn symbols_eq() {
        let strings = dummy_strings();
        let symbols = strings.map(Symbol::from_str);

        for i in 0..strings.len() {
            assert_eq!(symbols[i], Symbol::from_str(strings[i]));
        }
    }

    #[test]
    fn symbols_neq() {
        let strings = dummy_strings();
        let symbols = strings.map(Symbol::from_str);

        for i in 0..strings.len() {
            for j in 0..strings.len() {
                if i != j {
                    assert_ne!(symbols[i], symbols[j]);
                }
            }
        }
    }

    fn dummy_strings() -> [&'static str; 4] {
        ["Banana", "Apple", "Orange", "pumpkin"]
    }
}
