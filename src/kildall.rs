use std::collections::HashMap;
use std::collections::VecDeque;

// for now 
type LIR = u8;

type NodeId = (u8, u8);

pub struct Node<T> {
    nodeId : NodeId,
    nodeState : T,
}

pub struct Store<T> {
    storeMap : HashMap<NodeId, T>, 
}

pub trait Checkable
where
    Self: Sized + PartialEq
{
    fn meet(fst: Self, snd: Self) -> Self; 
    fn transfer(fst: Node<Self>) -> Node<Self>; 
} 

fn getElem<'a, T : Checkable> (node : &Node<T>, store : &'a Store<T>) -> &'a T {
    let id = &node.nodeId;
    return &store.storeMap[id]; 
}

fn successors<T> (node : &Node<T>) -> &[Node<T>] {
    panic!();
}

fn kildall<T : Checkable> (mut worklist : VecDeque<Node<T>>, store : Store<T>) -> Store<T> {
    if worklist.len() == 0 { return store; }
    let node = worklist.pop_front().unwrap();
    let incomingState = &node.nodeState;
    let currentState = getElem(&node, &store);
    if *incomingState == *currentState { return kildall(worklist, store); }
    let succs = successors(&node);
    panic!();
}


fn main () {
      println!("Hello, world!");
}
