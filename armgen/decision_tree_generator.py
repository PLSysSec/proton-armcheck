#!/usr/bin/python
#
# generate an optimized? decision tree given a list of instructions

from collections import deque

# decision tree node / leaf object
class TreeNode(object):
    values = []
    posn = None
    left = None
    right = None
    up = None

    def is_leaf(self):
        return self.left is None and self.right is None and self.posn is None

    def is_root(self):
        return self.up is None

    def pretty_print(self, indent=0, value='x'):
        offset = 0
        if not self.is_root():
            print(' ' * indent, "<%s> = %s:" % (str(self.up.posn), str(value)), sep='- ', end='')
            offset = 2

        if self.is_leaf():
            print(" ", self.values, sep='')
            return
        print()

        if self.left is not None:
            self.left.pretty_print(indent + offset, 0)
        else:
            print("%s- <%s> = 0: DEAD_END" % (" " * (indent + offset), str(self.posn)))

        if self.right is not None:
            self.right.pretty_print(indent + offset, 1)
        else:
            print("%s- <%s> = 1: DEAD_END" % (" " * (indent + offset), str(self.posn)))

# read in instructions file
def read_instrs(infile):
    with open(infile, 'r') as f:
        return [ l.strip() for l in f.readlines() ]

# how many different values are there across all instructions at a given position?
def num_opts(instrs, posn):
    vals = {}
    for instr in instrs:
        vals[instr[posn]] = 1 + vals.get(instr[posn], 0)
    return list(vals.keys())

# return a list of all of the positions we have to consider in decision tree
def get_posns(instrs):
    # walrus operator!!!
    #return [ (p, n) for p in range(0, len(instrs[0])) if (n := num_opts(instrs, p)) > 1 ]
    nx2 = []
    x2 = []
    x3 = []
    for p in range(0, len(instrs[0])):
        opts = num_opts(instrs, p)
        if len(opts) == 1 and opts[0] == 'x':
            # ignore bits that are always dont-care
            continue
        if 'x' in opts:
            if len(opts) == 2:
                #x2.append((p, opts))
                x2.append(p)
            else:
                #x3.append((p, opts))
                x3.append(p)
        elif len(opts) == 2:
            #nx2.append((p, opts))
            nx2.append(p)
        else:
            raise ValueError("unexpected: posn was none of nx2, x2, x3")
    return nx2 + x3 + x2

# get the values that go on the left or right side
def get_lr(instrs, posn):
    l0 = []
    l1 = []
    for instr in instrs:
        b = instr[posn]
        if b != '1':
            # '0' or 'x'
            l0.append(instr)
        if b != '0':
            # '1' or 'x'
            l1.append(instr)
    return (l0, l1)

def make_decision_tree(instrs):
    posns = get_posns(instrs)

    # root of tree
    root = TreeNode()
    root.values = list(instrs)

    # work queue
    wnext = deque()
    wnext.append(root)

    for p in posns:
        # fresh wnext queue
        work = wnext
        wnext = deque()
        while work:
            node = work.popleft()

            # no need to do anything if there's only one instruction here
            if len(node.values) < 2:
                continue

            # check if current position applies to this node
            (v0, v1) = get_lr(node.values, p)
            if set(v0) == set(v1):
                # if not, push to next position
                wnext.append(node)
                continue

            # otherwise, need to make children for this node
            left = right = None
            if v0:
                left = TreeNode()
                left.up = node
                left.values = v0
                wnext.append(left)
            if v1:
                right = TreeNode()
                right.up = node
                right.values = v1
                wnext.append(right)

            # update this node's info
            (node.left, node.right) = (left, right)
            node.posn = p
            node.values = []

    return root
