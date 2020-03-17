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
    fn meet(fst: &Self, snd: &Self) -> Self; 
    fn transfer(fst: &Node<Self>) -> Self; 
} 

fn getElem<'a, T : Checkable> (node : &Node<T>, store : &'a Store<T>) -> &'a T {
    let id = &node.nodeId;
    return &store.storeMap[id]; 
}

fn setElem<'a, T : Checkable> (node : &Node<T>, state : T, store : &'a Store<T>) {
    // let id = &node.nodeId;
    // store.storeMap[id] = state;
    panic!();
}

fn makeAndAddSuccessors<T> (node : &Node<T>, state : T, worklist : &VecDeque<Node<T>>) {
    // Get the node's successor ids
    // Add the given state to make new nodes
    // update the vector to include them
    panic!();
}

fn kildall<T : Checkable> (mut worklist : VecDeque<Node<T>>, store : Store<T>) -> Store<T> {
    if worklist.len() == 0 { return store; }
    let node = worklist.pop_front().unwrap();
    // See if the state has reached a fixed point yet
    let incomingState = &node.nodeState;
    let currentState = getElem(&node, &store);
    let newState = T::meet(incomingState, currentState);
    if newState == *currentState { return kildall(worklist, store); }
    // Update the information at this node in the state
    setElem(&node, newState, &store);
    // Get the information about this node using the transfer function,
    // and propagate it to all the node's successors
    let transferredState = T::transfer(&node);
    makeAndAddSuccessors(&node, transferredState, &worklist);
    return kildall(worklist, store);
}


fn main () {
      println!("Hello, world!");
}
