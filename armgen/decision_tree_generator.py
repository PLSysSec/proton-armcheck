#!/usr/bin/python
#
# generate an optimized? decision tree given a list of instructions

from cffi import FFI
from collections import deque
from random import choice, getrandbits

# decision tree node / leaf object
class TreeNode(object):
    names = {}
    values = []
    instrs = []
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
            print(" ", [ TreeNode.names[v] for v in self.values ], sep='')
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

    def to_code(self, indent=0):
        (ostr, posns) = self._to_code(indent)
        indent_str = " " * indent
        pstr = indent_str + "int DEAD_END = -1;\n"
        for p in posns:
            px = 31 - p
            pstr += indent_str + "int instr_bit_%d = (instr & (1 << %d)) != 0;\n" % (p, px)
        return pstr + ostr

    def _to_code(self, indent=0):
        indent_str = " " * indent
        posns = set()
        if self.is_leaf():
            #return (indent_str + "printf(\"%s\\n\");\n" % str(self.values), posns)
            assert len(self.values) == 1
            return (indent_str + "return %d;\n" % self.instrs.index(self.values[0]), posns)

        ostr = ""
        posns.add(self.posn)
        #ostr += indent_str + "int test_bit = (instr & (1 << %d)) == 1;\n" % self.posn
        ostr += indent_str + "if (instr_bit_%d == 0) {\n" % self.posn
        if self.left is None:
            ostr += indent_str + "  return DEAD_END;\n"
        else:
            (lstr, lpos) = self.left._to_code(indent + 2)
            ostr += lstr
            posns.update(lpos)

        ostr += indent_str + "} else {\n"
        if self.right is None:
            ostr += indent_str + "  return DEAD_END;\n"
        else:
            (rstr, rpos) = self.right._to_code(indent + 2)
            ostr += rstr
            posns.update(rpos)
        ostr += indent_str + "}\n"
        return (ostr, posns)

# read in instructions file
def read_instrs(infile):
    with open(infile, 'r') as f:
        ni = [ l.strip().split() for l in f.readlines() ]
    ret = []
    TreeNode.names.clear()
    for (n, i) in ni:
        TreeNode.names[i] = n
        ret.append(i)
    TreeNode.instrs = ret
    return ret

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
    nx1 = []
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
                x2.append(p)
            else:
                x3.append(p)
        elif len(opts) == 2:
            nx2.append(p)
        elif len(opts) == 1:
            nx1.append(p)
        else:
            raise ValueError("unexpected: posn was none of nx2, x2, x3: %s" % str(opts))
    return nx1 + nx2 + x3 + x2

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

def cleanup_dead_ends(root):
    work = deque()
    work.append(root)
    while work:
        node = work.popleft()
        if node is None:
            continue

        if not node.is_leaf():
            work.append(node.left)
            work.append(node.right)

        if node.up is None:
            continue

        (sibling, node_is_Left) = (node.up.right, True) if node.up.left is node else (node.up.left, False)
        if sibling is None or sibling.is_leaf():
            continue
        if node.posn != sibling.posn:
            continue
        if node.left is not None and node.right is not None:
            continue
        if sibling.left is not None and sibling.right is not None:
            continue

        nn = None
        if node.left is None and sibling.left is None:
            nn = TreeNode()
            (nn.left, nn.right) = (node.right, sibling.right) if node_is_Left else (sibling.right, node.right)
            (node.up.left, node.up.right) = (None, nn)
        if node.right is None and sibling.right is None:
            nn = TreeNode()
            (nn.left, nn.right) = (node.left, sibling.left) if node_is_Left else (sibling.left, node.left)
            (node.up.left, node.up.right) = (nn, None)
        if nn is None:
            continue
        nn.posn = node.up.posn
        node.up.posn = node.posn
        nn.up = node.up
        nn.left.up = nn.right.up = nn
        work.append(node.up)

def cleanup_1111(root):
    work = deque()
    work.append(root)
    while work:
        node = work.popleft()
        if node is None:
            continue

        if not node.is_leaf():
            work.append(node.left)
            work.append(node.right)
            continue

        if node.up is None:
            continue
        (sibling, node_is_Left) = (node.up.right, True) if node.up.left is node else (node.up.left, False)
        if sibling is None or not sibling.is_leaf() or sibling.values is None or len(sibling.values) != 1:
            continue
        if sibling.values[0] in node.values:
            node.values.remove(sibling.values[0])

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

    cleanup_dead_ends(root)
    cleanup_1111(root)
    return root

def to_bits(instr):
    ret = bin(instr)[2:]
    ret = ('0' * (32 - len(ret))) + ret
    return ret

def match_instr(template, instr):
    for (t, i) in zip(template, to_bits(instr)):
        if t != 'x' and t != i:
            return False
    return True

def fix_instr(template, instr):
    maxint = (1 << 32) - 1
    for (idx, b) in enumerate(template):
        bidx = 31 - idx
        if b == 'x':
            continue
        if b == '0':
            mask = maxint ^ (1 << bidx)
            instr = instr & mask
        else:
            assert b == '1'
            mask = (1 << bidx)
            instr = instr | mask
    assert match_instr(template, instr)
    return instr

def test(instr_file):
    # read in file and generate decision tree
    instrs = read_instrs(instr_file)
    root = make_decision_tree(instrs)

    # build the resulting code
    cstr = "int check_instr(uint32_t instr) {\n" + root.to_code(indent=2) + "  return 0;\n}\n"
    ffibuilder = FFI()
    ffibuilder.cdef("int check_instr(uint32_t instr);")
    ffibuilder.set_source("_ckinstr", cstr)
    ffibuilder.compile(verbose=True)
    from _ckinstr import ffi, lib

    # first, test random values that match known instructions
    for _ in range(0, 256):
        target_instr = choice(instrs)
        instr = fix_instr(target_instr, getrandbits(32))
        ret = lib.check_instr(instr)
        if ret != TreeNode.instrs.index(target_instr):
            print(TreeNode.names[target_instr], to_bits(instr))
