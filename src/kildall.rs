use std::collections::HashMap;
use std::collections::VecDeque;

// for now 
type LIR = u8;

type NodeId = (u8, u8);

pub struct Node<T> {
    node_id : NodeId,
    node_state : T,
}

pub struct Store<T> {
    store_map : HashMap<NodeId, T>, 
}

pub trait Checkable
where
    Self: Sized + PartialEq + Copy
{
    fn meet(fst: &Self, snd: &Self) -> Self; 
    fn transfer(fst: &Node<Self>) -> Self; 
} 

fn get_elem<'a, T : Checkable> (node : &Node<T>, store : &'a Store<T>) -> &'a T {
    let id = &node.node_id;
    return &store.store_map[id]; 
}

fn set_elem<'a, T : Checkable> (node : &Node<T>, state : T, store : &'a Store<T>) {
    let id = &node.node_id;
//    store.storeMap[id] = state;
    panic!();
}

fn make_and_add_successors<T : Checkable> (node : &Node<T>, state : T, worklist : &VecDeque<Node<T>>) {
    // Get the node's successor ids
    let new_ids = [(6,5)];
    for new_id in new_ids.iter() {
	let new_node = Node { node_id : *new_id, node_state : state };
    // 	worklist.push_back(Node { nodeId : *newId, nodeState : state}); 
    }
    // add the given state to make new nodes
    // update the vector to include them
    panic!();
}

fn kildall<T : Checkable> (mut worklist : VecDeque<Node<T>>, store : Store<T>) -> Store<T> {
    if worklist.len() == 0 { return store; }
    let node = worklist.pop_front().unwrap();
    // See if the state has reached a fixed point yet
    let incoming_state = &node.node_state;
    let current_state = get_elem(&node, &store);
    let new_state = T::meet(incoming_state, current_state);
    if new_state == *current_state { return kildall(worklist, store); }
    // Update the information at this node in the state
    set_elem(&node, new_state, &store);
    // Get the information about this node using the transfer function,
    // and propagate it to all the node's successors
    let transferred_state = T::transfer(&node);
    make_and_add_successors(&node, transferred_state, &worklist);
    return kildall(worklist, store);
}


fn main () {
      println!("Hello, world!");
}
