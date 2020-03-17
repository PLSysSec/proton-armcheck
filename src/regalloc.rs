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
    Error { message : String },
    RegMap { regmap : HashMap<Location, HashSet<VirtualRegister>> },
}


fn main () {
      println!("Hello, world!");
}

