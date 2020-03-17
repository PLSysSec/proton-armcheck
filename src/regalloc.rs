use std::collections::HashSet;
use std::collections::HashMap;

    
// for now
type Register = u8;
type StackSlot = u8;
type ArgumentIndex = u8;
type VirtualRegister = u8;

enum Location {
    Reg { reg : Register },
    SSlot { sslot : StackSlot },
    ASlot { aslot : ArgumentIndex },
}

enum RegisterState {
    Start,
    Error ( String ) ,
    RegMap ( HashMap<Location, HashSet<VirtualRegister>> ),
}

impl PartialEq for RegisterState {
    fn eq(&self, other: &Self) -> bool {
	use RegisterState::*;
	match (self, other) {
	    (&Start, _)                            => false,
	    (_, &Start)                            => false,
	    (&Error ( ref m1 ), &Error ( ref m2 )) => m1 == m2,
//	    (&RegMap (ref h1 ), &RegMap (ref h2))  => h1 == h2,
	    _                                      => false,
	}
    }
}

fn main () {
      println!("Hello, world!");
}

