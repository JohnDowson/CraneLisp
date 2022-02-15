use cl_alloc::{Mark, Ref};
use somok::Somok;

use crate::symbol::SymId;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct FuncId(pub usize);
pub type AtomRef<const ID: u32> = Ref<Atom<ID>, ID>;

#[derive(PartialEq, Clone, Copy)]
pub enum Atom<const ID: u32> {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    Pair(AtomRef<ID>, AtomRef<ID>),
    Symbol(SymId),
    Func(u64),
}

#[derive(Debug)]
pub struct FromBytesError;

impl<const ID: u32> Atom<ID> {
    pub fn from_bytes(bytes: [u8; 16]) -> Result<Self, FromBytesError> {
        match bytes[0] {
            0 => Self::Null,
            1 => {
                let b = bytes[15] == 1;
                Self::Bool(b)
            }
            2 => {
                let i = i64::from_be_bytes(bytes[8..].try_into().map_err(|_| FromBytesError)?);
                Self::Int(i)
            }
            3 => {
                let f = f64::from_be_bytes(bytes[8..].try_into().map_err(|_| FromBytesError)?);
                Self::Float(f)
            }
            4 => {
                let cadr = usize::from_be_bytes(bytes[8..].try_into().map_err(|_| FromBytesError)?);
                let id = u32::from_be_bytes(bytes[4..8].try_into().map_err(|_| FromBytesError)?);
                if ID != id {
                    return FromBytesError.error();
                }
                let car = cadr >> 4;
                let cdr = cadr & 0x00FF;
                unsafe {
                    Self::Pair(
                        Ref::<Self, ID>::from_bytes(car),
                        Ref::<Self, ID>::from_bytes(cdr),
                    )
                }
            }
            5 => todo!(),
            6 => {
                let func = u64::from_be_bytes(bytes[8..].try_into().map_err(|_| FromBytesError)?);
                Self::Func(func)
            }
            7.. => return FromBytesError.error(),
        }
        .okay()
    }

    pub fn to_bytes(&self) -> [u8; 16] {
        let mut bytes = [0; 16];
        for (byte, idbyte) in bytes[4..].iter_mut().zip(ID.to_be_bytes()) {
            *byte = idbyte;
        }
        match self {
            Atom::Null => (),
            Atom::Bool(b) => {
                bytes[0] = 1;
                bytes[15] = *b as u8;
            }
            Atom::Int(i) => {
                bytes[0] = 2;
                for (byte, ibyte) in bytes[8..].iter_mut().zip(i.to_be_bytes()) {
                    *byte = ibyte
                }
            }
            Atom::Float(f) => {
                bytes[0] = 3;
                for (byte, fbyte) in bytes[8..].iter_mut().zip(f.to_be_bytes()) {
                    *byte = fbyte
                }
            }
            Atom::Pair(car, cdr) => {
                bytes[0] = 4;
                let cadr = (car.as_usize() << 4) | cdr.as_usize();
                for (byte, cadrbyte) in bytes[8..].iter_mut().zip(cadr.to_be_bytes()) {
                    *byte = cadrbyte
                }
            }
            Atom::Symbol(SymId(s)) => {
                bytes[0] = 5;
                for (byte, sbyte) in bytes[8..].iter_mut().zip(s.to_be_bytes()) {
                    *byte = sbyte
                }
            }
            Atom::Func(f) => {
                bytes[0] = 5;
                for (byte, fbyte) in bytes[8..].iter_mut().zip(f.to_be_bytes()) {
                    *byte = fbyte
                }
            }
        }
        bytes
    }
}

#[test]
fn to_and_from_bytes() {
    use cl_alloc::Heap;
    use std::assert_matches::assert_matches;
    let atom = Atom::<0>::Int(69);
    let bytes = atom.to_bytes();

    assert_matches!(bytes, [2, .., 69]);
    assert_eq!(atom, Atom::from_bytes(bytes).unwrap());

    let mut heap = Heap::default();

    let r1 = heap.alloc(Atom::Int(42));
    let r2 = heap.alloc(Atom::Int(69));
    let atom = Atom::<0>::Pair(r1, r2);

    let bytes = atom.to_bytes();
    assert_matches!(bytes, [4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,]);
    assert_eq!(atom, Atom::from_bytes(bytes).unwrap());
}

impl<const ID: u32> std::fmt::Debug for Atom<ID> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "Null"),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Int(arg0) => f.debug_tuple("Int").field(arg0).finish(),
            Self::Float(arg0) => f.debug_tuple("Float").field(arg0).finish(),
            Self::Pair(_, _) => f.debug_tuple("Pair").finish(),
            Self::Symbol(arg0) => f.debug_tuple("Symbol").field(arg0).finish(),
            Self::Func(arg0) => f.debug_tuple("Func").field(arg0).finish(),
        }
    }
}

impl<const ID: u32> Mark<ID> for Atom<ID> {
    fn refs(&self) -> Vec<Ref<Self, ID>> {
        if let Self::Pair(car, cdr) = self {
            vec![*car, *cdr]
        } else {
            Vec::new()
        }
    }
}
