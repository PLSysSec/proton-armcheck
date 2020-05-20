#!/usr/bin/env python

# generate an optimized? decision tree given a list of instructions

from __future__ import absolute_import, print_function

from collections import deque
from random import choice, getrandbits
import os
from string import Template
import sys

script_dir_path = os.path.dirname(os.path.realpath(__file__))

# info about instructions
class Instrs(object):
    dummy = None
    instrs = []
    names = {}
    tnames = []
    tests = ""

    # read in instructions file
    @classmethod
    def from_file(cls, infile):
        with open(infile, "r") as f:
            ni = [ l.strip().split(' ', 1) for l in f.readlines() ]

        del cls.instrs[:]
        del cls.tnames[:]
        cls.names.clear()
        tests = {}
        tnames = set()

        # parse instructions
        in_instr = False
        current_instr = None
        for n in ni:
            if n == ['']:
                in_instr = False
                continue

            if not in_instr:
                (current_name, current_instr) = n
                if current_name == "dummy":
                    cls.dummy = current_instr
                else:
                    cls.instrs.append(current_instr)
                    cls.names[current_instr] = current_name
                    tests[current_instr] = []
                    in_instr = True
            else:
                tests[current_instr].append(n)
                tnames.add(n[0])

        # test names
        cls.tnames = ["OK"] + list(sorted(tnames))
        tstr = "const char *test_names[] = {"
        for tname in cls.tnames:
            tstr += "\"%s\"," % tname
        tstr += "};\n\n";

        # instruction names
        istr = "const char *instr_names[] = {\"_\","
        for instr in cls.instrs:
            name = cls.names[instr]
            istr += "\"%s\"," % name
        istr += "};\n\n";

        # check functions
        fstr = ""
        for tt in tests:
            iidx = 1 + cls.instrs.index(tt)
            fstr += "static uint32_t check_instr_%d(uint32_t instr) {\n" % iidx
            for t in tests[tt]:
                tidx = cls.tnames.index(t[0])
                fstr += "  if ((%s) != 0) { return (%d << 16) | %d; }\n" % (t[1], tidx, iidx)
            if not tests[tt]:
                fstr += "  (void)instr;\n"
            fstr += "  return %d;\n}\n\n" % iidx

        cls.tests = tstr + istr + fstr

    # generate early return for dummy instr
    @classmethod
    def dummy_code(cls, indent=2):
        if cls.dummy is None:
            return ""
        dmask = int(''.join( '0' if c == 'x' else '1' for c in cls.dummy ), 2)
        dpatt = int(''.join( '0' if c == 'x' else c for c in cls.dummy ), 2)
        return (' ' * indent) + "if ((instr & 0x%x) == 0x%x) { return DEAD_END; }\n" % (dmask, dpatt)

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
            print(" ", [ Instrs.names[v] for v in self.values ], sep='')
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
        pstr = indent_str + "const int DEAD_END = 0;\n" + Instrs.dummy_code(indent)
        for p in posns:
            px = 31 - p
            pstr += indent_str + "int instr_bit_%d = (instr & (1 << %d)) != 0;\n" % (p, px)
        return pstr + ostr

    def _to_code(self, indent=0):
        indent_str = " " * indent
        ostr = ""
        posns = set()
        if self.is_leaf():
            assert len(self.values) == 1
            iidx = 1 + Instrs.instrs.index(self.values[0])
            ostr += indent_str + "return check_instr_%d(instr);\n" % iidx
            return (ostr, posns)

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

# how many different values are there across all instructions at a given position?
def num_opts(posn):
    vals = {}
    for instr in Instrs.instrs:
        vals[instr[posn]] = 1 + vals.get(instr[posn], 0)
    return list(vals.keys())

# return a list of all of the positions we have to consider in decision tree
def get_posns():
    nx1 = []
    nx2 = []
    x2 = []
    x3 = []
    for p in range(0, len(Instrs.instrs[0])):
        opts = num_opts(p)
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

def make_decision_tree():
    posns = get_posns()

    # root of tree
    root = TreeNode()
    root.values = list(Instrs.instrs)

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
    if Instrs.dummy is not None:
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

def make_code(bits, static=False):
    # read in file and generate decision tree
    Instrs.from_file(os.path.join(script_dir_path, "instrs_%d" % bits))
    root = make_decision_tree()
    static_str = "static " if static else ""
    proto = "%suint32_t check_instr(uint32_t instr)" % static_str
    cstr = ("%s {\n" % proto) + root.to_code(indent=2) + "  return DEAD_END;\n}\n"
    return ("%s;" % proto, Instrs.tests + cstr)

def write_code(bits):
    (_, code) = make_code(bits, static=True)

    with open(os.path.join(script_dir_path, "implck_%d" % bits), "r") as f:
        tmpl = Template(f.read())
    with open(os.path.join(script_dir_path, "armgen_%d.cpp" % bits), "w") as f:
        f.write("// autogenerated. do not edit.\n\n" + tmpl.substitute(code=code))

def test(bits):
    (proto, code) = make_code(bits, static=False)

    # build the resulting code
    from cffi import FFI
    ffibuilder = FFI()
    ffibuilder.cdef(proto)
    ffibuilder.set_source("_ckinstr", code)
    ffibuilder.compile(verbose=True)
    from _ckinstr import ffi, lib

    # first, test random values that match known instructions
    for _ in range(0, 8192):
        target_instr = choice(Instrs.instrs)
        instr = fix_instr(target_instr, getrandbits(32))
        ret = lib.check_instr(instr)
        if ret != 1 + Instrs.instrs.index(target_instr):
            print(Instrs.names[target_instr], to_bits(instr), ((1 << 16) - 1) & ret, ret >> 16)

    # next, generate random instructions, match them manually, then make sure the tree agrees
    for _ in range(0, 8192):
        if Instrs.dummy is not None:
            # leftmost bit is 0, guaranteeing that we don't get something rejected by dummy filter
            # XXX this is a hard-coded hack
            instr = getrandbits(31)
        else:
            instr = getrandbits(32)
        matches = [ tmpl for tmpl in Instrs.instrs if match_instr(tmpl, instr) ]
        ret = lib.check_instr(instr)
        if not matches:
            assert ret == 0
        if matches:
            ret = ((1 << 16) - 1) & ret
            assert Instrs.instrs[ret - 1] in matches
            if (ret >> 16) != 0:
                print(ret >> 16)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        test(32)
        test(64)
    else:
        write_code(32)
        write_code(64)
