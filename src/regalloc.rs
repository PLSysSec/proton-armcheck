use std::collections::HashSet;
use std::collections::HashMap;
use crate::kildall::{Checkable, Node};
    
// for now
type Register = u8;
type StackSlot = u8;
type ArgumentIndex = u8;
type VirtualRegister = u8;

#[derive(Eq, PartialEq, Hash, Clone)]
enum Location {
    Reg ( Register ),
    SSlot ( StackSlot ),
    ASlot ( ArgumentIndex ),
}

#[derive(Clone)]
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
	    (&RegMap (ref h1 ), &RegMap (ref h2))  => h1 == h2,
	    _                                      => false,
	}
    }
}

impl Eq for RegisterState {}

impl Checkable for RegisterState {
    fn meet(fst : &RegisterState, snd : &RegisterState) -> RegisterState {
	panic!();
    }
    fn transfer(fst : &Node<RegisterState>) -> RegisterState {
	panic!();
    }
}

fn transfer_phi(node : &Node<RegisterState>) -> RegisterState { panic!(); }

fn transfer_move(node : &Node<RegisterState>) -> RegisterState { panic!(); }

fn transfer_other(node : &Node<RegisterState>) -> RegisterState { panic!(); }

fn main () {
      println!("Hello, world!");
}

