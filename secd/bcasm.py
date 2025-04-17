#!/usr/bin/python3
# Bytecode Assembler

# Syntax:
# PROG      ::= <COMPONENT>*
# COMPONENT ::= <STATIC> | <BYTECODES>
# STATIC    ::= LABEL ":" <OBJECT>
# OBJECT    ::= ".object" INT {LABEL,","}   # tag, list, of, fields
#             | ".closure" LABEL INT        # code pointer, arity
#             | ".int" INT                  # integer object
#             | ".char" "'" CHAR "'"        # char object
#             | ".string" "\"" CHAR* "\""   # string object
# BYTECODES ::= <any bytecode as specified in BYTECODE.md>
#             | LABEL ":"

# Labels must be the first token on their line.
# Other whitespace is not significant except where necessary to separate tokens.

# The label _start is special and is the entrypoint of the program.
# A few labels are builtin, they are: static_neg1, static_0, static_1, ... static_9,
# static_true, static_false, and nil. These builtin labels can be used only
# in static data, and any data that uses them will be moved to the relocatable
# segment.

# Bytecodes with arguments are followed immediately by their arguments.
# Arguments representing code offsets must be labels. Arguments representing
# integral values must be integer tokens.
# The <BYTECODES> in the program will be kept in the same order in which they
# were written, and contiguous sequences will stay contiguous,
# which is important for fall-through codepaths. Objects between sequences may
# be moved, however, and in particular static objects containing labels will
# be moved to the beginning of the program for relocation.
# The LDGW and LDGD bytecodes should not be used explicitly; rather use LDG
# and let the assembler decide if LDGW or LDGD should be used. (LDGW will be
# used as long as it is possible to place the named object in the first 64KB
# of the program.) If you explicitly use LDGD and the object could be placed
# in the first 64KB, you are wasting space; if you explicitly use LDGW with
# more objects than can be placed in the first 64KB, assembly will fail with
# an error.
# Similarly, use LD instead of LDH, LDW, or LDD. The assembler will choose the
# smallest valid representation. Similarly for LFLD and SFLD.
# Example:

# _start: LDH 2 LDG global_list FASTAP index HLT
# index: OVER IF_EQ index_base 
#        DUP IF_NIL index_crash
#        UNPCK DROP SWAP I1 SUB SWAP FASTTL index
# index_base: UNPCK RET
# index_crash: ...
# global_list: .object 16, static_0, list1
# list1:       .object 16, static_1, list2
# list2:       .object 16, static_2, list3
# list3:       .object 16, static_3, nil

# All of the objects in this program will be moved above the bytecodes.
# The LDG will be assembled as an LDGW. Note that this code implements
# the following ML program:
# let index n xs = match n,xs with
#   | 0, x::_  -> x
#   | _, _::xs -> index (n-1) xs;;
# let global_list = [0; 1; 2; 3];;
# index 2 global_list;;

# There is no reason to use a bytecode like `EAP 1` instead of `EAP1`,
# so such bytecodes are replaced automatically.
# At the moment, this does not apply to `LDG `

# For now, an error is raised if a branch target is out of range.
# In future, an attempt may be made to replace an out-of-range branch
# target with an inverted branch over a GOTO, but for now, don't produce
# bytecode jumps that might need to jump over 64KB of bytecode.
# (See e.g. https://reviews.llvm.org/D108961 for why this relaxation can be bad.)

from ast import literal_eval
from dataclasses import dataclass
from enum import Enum
from lark import Lark, Transformer
import lark
from typing import List, Dict, Set, Union

# Implementation notes:
# To avoid infinite loops, we will not shorten a relaxable instruction
# after it has been relaxed to be longer. It is possible for situations
# where this might be possible to arise, because the aligning aspect of
# the MATCH instruction family's jump tables can move a jump closer to
# its target in situations like `LD...; IFX...;MATCH...;tgt:`.
# Relaxing the `LD` can move the `IFX` closer to the tgt, if the jump
# table doesn't cross an alignment boundary.

# When we are updating fragment offsets after a relaxation,
# we could either update match padding on the way, or do a separate
# pass later after possibly more relocations have occurred.
# For now, we do it on the way, anticipating that this will
# not cause problems.

# The key functions, are 'parse', 'fragmentize', 'relax_fragments',
# 'assemble_fragments', and 'drive'.

grammar = r"""
  _list1{x,sep}: x (sep x)*

  start: labeled *
  labeled: LABEL ":" _component
  _component: object | bytecode *
  ?object: dotobj | dotclo | dotint | dotchar | dotstr
  dotobj: ".object" INT [ "," _list1{LABEL,","} ]
  dotclo: ".closure" LABEL INT
  dotint: ".int" SIGNED_INT
  dotchar: ".char" "'" ( /[^\\]|\\\d+/ ) "'"
  dotstr: ".string" ESCAPED_STRING

  ?bytecode: nullary
    | tjump
    | int_bc | ld_bc | chr
    | branch
    | jump
    | ldg
    | mkobj
    | mkclo
    | match 

  nullary: NULLARY_BC
  tjump: TJUMP INT LABEL
  int_bc: INT_BC INT
  ld_bc: LD_BC SIGNED_INT
  branch: BRANCH LABEL
  jump: JUMP LABEL
  ldg: LDG LABEL
  chr: /CHR/ "'" /[^\\]|\\\d+/ "'" // very awk but wcyd
  mkobj: /MKOBJ/ INT INT
  mkclo: MKCLO INT LABEL INT
  match: MATCH  INT ","         _list1{LABEL,","}
       | MATCHD INT "," INT "," _list1{LABEL,","}
  
  NULLARY_BC: /NOP|HLT|NIL|I0|I1|BT|BF|LDE|POP|DUP|TUCK|TUCK2|SWAP|OVER|ADD|SUB|MUL|DIV|REM|NEG|SHL|SHR|ASR|INC|AND|OR|XOR|TAG|I2(B|C)|(B|C)2I|CLONE|UNPCK|LLCL[0-9]?|SLCL[0-9]?|LDCV[0-3]?|LLCV([1-2][0-3])?|EAP[0-5]|ETL[0-5]|CAP[1-6]|CTL[1-6]|UAP|UTL|RET/
  JUMP: /GOTO|FASTAP|FASTTL/
  BRANCH: /IFEQ|IFNE|IFLT|IFGT|IFLE|IFGE|IFF|IFNF|IFCEQ|IFCNE|IFCLT|IFCGT|IFCLE|IFCGE|IFSAME|IFDIFF|IFNIL|IFNNIL|IFFALSY|IFTRUTHY/
  TJUMP: /IFTEQ|IFTLT/
  INT_BC: /ALLOC|EAP|ETL|SYS|CHKTAG|STAG/
  LD_BC: /LD(H|W|D)?/
  LDG: /LDG(W|D)?/
  MKCLO: /MKCLO|MKLCLO/

  MATCH:  /MATCHN?/
  MATCHD: /MATCHN?D/

  %import common.INT    // this is actually a natural number
  %import common.SIGNED_INT // this is not
  %import common.ESCAPED_STRING
  %import common.CNAME  ->  LABEL
  COMMENT: ";" /[^\n]/*
  %ignore COMMENT
  %import common.WS
  %ignore WS
"""

parser = Lark(grammar)

example = """
_start: LDH 2 LDG global_list FASTAP index HLT
index: OVER IFEQ index_base DUP IFNIL index_base
       UNPCK POP SWAP I1 SUB SWAP FASTTL index
index_base: UNPCK RET
global_list: .object 16, static_0, list1
list1:       .object 16, static_1, list2
list2:       .object 16, static_2, list3
list3:       .object 16, static_3, nil
"""

class Segment(Enum):
  BUILTIN = 0
  RELOCATABLE = 1
  TEXT = 2

@dataclass(repr=False, unsafe_hash=True)
class Label():
  name: str
  segment: Segment
  containing_fragment: Union['Fragment',None] = None

  def __str__(self):
    return self.name
  def __repr__(self):
    return repr(self.__str__())

  def offset(self):
    if Label.memo[self.name] == Segment.BUILTIN:
      return None
    frag = self.containing_fragment
    return frag.offset + frag.labels[self.name]

  memo = {} # : Dict[str, 'Label'] but not dataclass member
  builtin_reloc = {} # : Dict[str, bytes] relocation bytesequences

  @staticmethod
  def get_label(name: str, segment: Segment = Segment.TEXT):
    try:
      return Label.memo[name]
    except KeyError:
      l = Label(name, segment)
      Label.memo[name] = l
      return l

def init_label_tables():
  bytesequences = {
    'nil':          bytes.fromhex('00 00 00 00'),
    'static_false': bytes.fromhex('00 00 00 ff'),
    'static_true':  bytes.fromhex('01 00 00 ff'),
    'static_neg1':  bytes.fromhex('02 00 00 ff'),
    'static_0':     bytes.fromhex('03 00 00 ff'),
    'static_1':     bytes.fromhex('04 00 00 ff'),
    'static_2':     bytes.fromhex('05 00 00 ff'),
    'static_3':     bytes.fromhex('06 00 00 ff'),
    'static_4':     bytes.fromhex('07 00 00 ff'),
    'static_5':     bytes.fromhex('08 00 00 ff'),
    'static_6':     bytes.fromhex('09 00 00 ff'),
    'static_7':     bytes.fromhex('0a 00 00 ff'),
    'static_8':     bytes.fromhex('0b 00 00 ff'),
    'static_9':     bytes.fromhex('0c 00 00 ff'),
  }

  for name in bytesequences.keys():
    Label.memo[name] = Label(name, Segment.BUILTIN, None)
    Label.builtin_reloc[name] = bytesequences[name]
init_label_tables()

@dataclass
class Static:
  label: Label
  length: int
@dataclass
class UserObj(Static):
  tag: int
  fields: List[str]
@dataclass
class Closure(Static):
  code_pointer: Label
  arity: int
@dataclass
class StaticInt(Static):
  value: int
@dataclass
class StaticChar(Static):
  value: str # must be single letter
@dataclass
class StaticString(Static):
  value: str

@dataclass
class Bytecode():
  name: str
  args: List[Label|int]

# Bytecodes that are not relaxable.
@dataclass
class FixedBytecode(Bytecode):
  length: int

# Types of relaxations
class RelaxType(Enum):
  LD = 1
  # these might change in the future, idk if this is the cleanest
  # way to do this...
  ABS_1_TO_2 = 2 # 1-byte values that can relax to 2-byte values
  ABS_2_TO_3 = 3 # 2-byte offsets that can relax to 3-byte offsets
  MATCH_PAD = 4

# Bytecodes that are relaxable.
@dataclass
class RelaxBytecode(Bytecode):
  current_length: int
  types = {
    'LD': RelaxType.LD,
    'LDG': RelaxType.ABS_2_TO_3,
    'LFLD': RelaxType.ABS_1_TO_2,
    'SFLD': RelaxType.ABS_1_TO_2,
    'MATCH'  : RelaxType.MATCH_PAD,
    'MATCHN' : RelaxType.MATCH_PAD,
    'MATCHD' : RelaxType.MATCH_PAD,
    'MATCHND': RelaxType.MATCH_PAD,
  }
  def relax_type(self):
    return RelaxBytecode.types[self.name]
  def freeze(self):
    return FixedBytecode(self.name, self.args, self.current_length)

# A sequence of bytecodes from the parser, mixing both
# fixed and relaxable bytecodes. Separated by labels
# according to the input program.
@dataclass
class InputByteCodeSequence():
  label: Label
  bytecodes: List[Bytecode]

Offset = int
# Fragments form a linked list of program components,
# which are each either a DataSegment,
# FixedBytecodeSequence or a RelaxBytecode. 
# Also records the current offset of this fragment in the
# overall program and the offsets (within this fragment)
# of any labels inside the fragment. Relaxable bytecodes
# will have labels only at offset 0.
@dataclass
class Fragment():
  offset: Offset
  length: int
  labels: Dict[str,Offset]
  next: Union['Fragment',None]

@dataclass
class RelaxFragment(Fragment):
  bc: RelaxBytecode
  initialized = False
  def append(self, bc: RelaxBytecode):
    assert not self.initialized
    self.bc = bc
    self.length = bc.current_length

# A sequence of data objects. Data objects have fixed
# lengths but might be rearranged. After rearrangement,
# adjacent data segments may be combined.
@dataclass
class DataFragment(Fragment):
  data: List[Static]

# A sequence of bytecodes, all of which are fixed.
@dataclass
class FixedFragment(Fragment):
  # store a list, not bytes, because we can't assemble every
  # bytecode until after labels are resolved, and we can't do
  # that until relaxation is done.
  seq: List[FixedBytecode]

  def append(self, bc: FixedBytecode):
    self.seq.append(bc)
    self.length += bc.length

def data_fragment_of(obj: Static, offset: int):
  return DataFragment(
    offset,
    obj.length,
    labels = { obj.label: 0 },
    next = None
  )

class BcAsm(Transformer):
  def __init__(self):
    self.stop_after_transform = False
    self.labels_requiring_relocation = set()
    # It's possible to map names to approximate values of moving them
    # but accurate approximations require noticing loops and we're
    # obviously not going to do that. A compiler that wants to do that
    # is free to promote the valuable objects itself.
    # Therefore, if the compiler emits 'LDGW', we should also trust
    # that this is done ALREADY, and not try to move the object again.
    # When we move data, we'll move it after things that require relocation
    # but before anything else that came before the code.
    # This gives the most naturally correct support for a future LDGR
    # that loads from relative offset.
    self.labels_desiring_low_offsets = set()

  def ESCAPED_STRING(self, s):
    return literal_eval(s)
  def LABEL(self, s: lark.Token):
    return Label.get_label(s.value)
  def INT(self, s):
    return int(s)
  def SIGNED_INT(self, s):
    return int(s)

  def labeled(self, children):
    label = children[0]
    if len(children) == 2 and isinstance(children[1], Static):
      obj = children[1]
      obj.label = label
      # If a UserObj contains fields, those fields need relocation.
      # That's not optional! Mark it as such.
      if isinstance(obj, UserObj) and len(obj.fields) > 0:
        self.labels_requiring_relocation.add(label.name)
      return obj
    elif not isinstance(children[1], Bytecode):
      raise ValueError(f'labeled thing {children[1]} is not static or bytecode?')
    return InputByteCodeSequence(label, children[1:])

  def dotobj(self, params):
    tag = params[0]
    fields = params[1:]
    return UserObj(None, (len(fields)+1)*4, tag, fields)
  def dotclo(self, params):
    codepointer, arity = params
    return Closure(None, 8, codepointer, arity)
  def dotint(self, params):
    [i] = params
    return StaticInt(None, 8, i)
  def dotchar(self, params):
    [c] = params
    return StaticChar(None, 8, c.value)
  def dotstr(self, content):
    [content] = content
    return StaticString(None, length=(len(content)+7)//4*4, value=content)

  _relax_bytecodes = set([
    'MATCH', 'MATCHD', 'MATCHN', 'MATCHND',
    'LD', 'LDG', 'LFLD', 'SFLD'
  ])

  def nullary(self, content):
    [bc] = content
    return FixedBytecode(bc.value, [], 1)
  
  def tjump(self, content):
    [bc, tag, target] = content
    return FixedBytecode(bc.value, [tag, target], 4)

  def int_bc(self, content):
    [bc, arg] = content
    name = bc.value
    if name in ['ALLOC', 'STAG', 'CHKTAG']:
      size = 3
    elif name in ['SYS']:
      size = 2
    elif name in ['EAP', 'ETL']:
      if arg < 6:
        return FixedBytecode(name + str(arg), [], 1)
      else:
        return FixedBytecode(name, [arg], 2)
    else:
      raise ValueError('Unknown INT_BC')

  def ld_bc(self, content):
    [bc, arg] = content
    name = bc.value
    if name == 'LD':
      return RelaxBytecode(name, [arg], 2) # assume LDH
    fixsizes = {'LDH': 2, 'LDW': 3, 'LDD': 5}
    return FixedBytecode(name, [arg], fixsizes[name])

  def ldg(self, content):
    [bc, arg] = content
    name = bc.value
    if name == 'LDG':
      self.labels_desiring_low_offsets.add(arg.name)
      return RelaxBytecode(name, [arg], 3)
    fixsizes = {'LDGW': 3, 'LDGD': 4}
    return FixedBytecode(name, [arg], fixsizes[name])

  def chr(self, content):
    [_chr_tok, arg_tok] = content
    char = arg_tok.value
    if char[0] == '\\':
      arg = int(char[1:])
    else:
      arg = ord(char)
    return FixedBytecode('CHR', [arg], 2)

  def branch(self, content):
    [branch, tgt] = content
    name = branch.value
    return FixedBytecode(name, [tgt], 3)
  
  def jump(self, content):
    [jump, tgt] = content
    name = jump.value
    return FixedBytecode(name, [tgt], 4)

  def mkobj(self, content):
    [_mkobj, n, tag] = content
    return FixedBytecode('MKOBJ', [n,tag], 4)

  def mkclo(self, content):
    [mkclo, n, cp, arity] = content
    name = mkclo.value
    return FixedBytecode(name, [n,cp,arity], 6)

  def match(self, content):
    bc = content[0]
    name = bc.value
    is_def = name.endswith('D')
    if is_def:
      lo = content[1]
      hi = content[2]
      n  = hi - lo + 2 # +1 for table, +1 for default
      targets = content[3:]
      size = 5 + 4*n # assumes no padding
    else:
      n = content[1]
      targets = content[2:]
      size = 3 + 4*n # assumes no padding
    if 'N' in name:
      n += 1
      size += 4
    if len(targets) != n:
      print(f"Error: number of labels in {name} at {bc.line}:{bc.column} inconsistent")
      self.stop_after_transform = True
    return RelaxBytecode(name, content[1:], size)
    
def parse(input: str):
  tree = parser.parse(input)
  transformer = BcAsm()
  components = transformer.transform(tree).children
  return {
    'components': components,
    'desires_low': transformer.labels_desiring_low_offsets,
    'needs_reloc': transformer.labels_requiring_relocation,
    'stop': transformer.stop_after_transform
  }

# Find all of the components requiring relocation and move them into
# the given DataFragment.
def build_reloc_seg(seg: DataFragment, needs_reloc: Set[str],
                    components: List[Static|InputByteCodeSequence]):
  data = seg.data
  lbls = seg.labels
  unused_components = []

  len = seg.length # bytes used by reloc segment so far

  for component in components:
    if component.label.name in needs_reloc:
      assert isinstance(component, UserObj)
      data.append(component)
      lbls[component.label.name] = len
      component.label.containing_fragment = seg
      component.label.segment = Segment.RELOCATABLE
      len += component.length
    else:
      unused_components.append(component)
  seg.length = len
  return seg, unused_components

# Try to gather data objects that want lower addresses from the remaining
# components into a new DataFragment which will be in the Text segment.
# If no objects could be reduced, None is returned. Otherwise, a fragment
# containing them all and the remaining unprocessed components are returned.
# An object is moved if it wants a lower address, as long as there is
# still space for it.
def try_reduce_offsets(offset: Offset, wants_reduction: Set[str],
                       components: List[Static|InputByteCodeSequence]):
  LIMIT = 65535 - offset
  frag = DataFragment(offset, 0, {}, None, [])
  len = 0
  data = frag.data
  lbls = frag.labels
  unused_components = []

  for component in components:
    handled = False
    if component.label.name in wants_reduction:
      # Do we have enough room left?
      if len + component.length <= LIMIT:
        data.append(component)
        lbls[component.label.name] = len
        component.label.containing_fragment = frag
        component.label.segment = Segment.TEXT
        len += component.length
        handled = True
    if not handled:
      unused_components.append(component)
  frag.length = len
  if len > 0:
    return frag, unused_components
  return None

def make_frag_if_needed(frag: Fragment, frag_type, bc: Bytecode):
  if isinstance(bc, FixedBytecode):
    if frag_type != FixedFragment:
      new_frag = FixedFragment(frag.offset + frag.length, 0, {}, None, [])
      frag.next = new_frag
      frag = new_frag
      frag_type = FixedFragment
    return frag, frag_type
  if isinstance(bc, RelaxBytecode):
    new_frag = RelaxFragment(frag.offset + frag.length, 0, {}, None, None)
    frag.next = new_frag
    frag = new_frag
    frag_type = RelaxFragment
    return frag, frag_type
  assert False

def build_text_fragments(frag: Fragment,
                         components: List[Static|InputByteCodeSequence]):
  frag_type = None
  unhandled_labels: List[Label] = []
  for component in components:
    unhandled_labels.append(component.label)
    if isinstance(component, Static):
      if frag_type != DataFragment:
        new_frag = DataFragment(frag.offset + frag.length, 0, {}, None, [])
        frag.next = new_frag
        frag = new_frag
        frag_type = DataFragment
      for label in unhandled_labels:
        frag.labels[label.name] = frag.length
        label.containing_fragment = frag
      unhandled_labels = []
      frag.data.append(component)
      frag.length += component.length
    if isinstance(component, InputByteCodeSequence):
      if len(component.bytecodes) == 0:
        continue

      frag, frag_type = make_frag_if_needed(frag, frag_type,
                                            component.bytecodes[0])
      for label in unhandled_labels:
        frag.labels[label.name] = frag.length
        label.containing_fragment = frag
      unhandled_labels = []
      
      frag.append(component.bytecodes[0])

      for bytecode in component.bytecodes[1:]:
        frag, frag_type = make_frag_if_needed(frag, frag_type, bytecode)
        frag.append(bytecode)

# Convert a dictionary from 'parse' into a linked list of Fragments
# which is initialized but not yet relaxed.
# Do not call if parsecbc['stop'].
def fragmentize(parsedbc):
  assert not parsedbc['stop']
  components = parsedbc['components']
  desires_low = parsedbc['desires_low']
  needs_reloc = parsedbc['needs_reloc']

  # construct the relocatable segment first. It will be the head of the
  # linked list of fragments.
  # 12 is the length of the bytecode header.
  reloc_seg = DataFragment(offset = 12, length = 0, labels = {},
                           next = None, data = [])
  _, components = build_reloc_seg(reloc_seg, needs_reloc, components)
  reduced = try_reduce_offsets(reloc_seg.offset + reloc_seg.length,
                               desires_low, components)
  if reduced is not None:
    low_offset_frag, components = reduced
    reloc_seg.next = low_offset_frag
    last_frag = low_offset_frag
  else:
    last_frag = reloc_seg

  build_text_fragments(last_frag, components)

  return reloc_seg

def print_fragments(frag: Fragment):
  def print_labels_here(labels, length):
    for name,off in labels.items():
      if off == length:
        print(f'  {name}:')
  def print_obj(obj):
    print(f'    {obj}')
  def print_bc(bc):
    print(f'    {bc}')

  while frag is not None:
    print(f'{type(frag).__name__}@{frag.offset} (len={frag.length})')
    length = 0
    if isinstance(frag, DataFragment):
      for obj in frag.data:
        print_labels_here(frag.labels, length)
        print_obj(obj)
        length += obj.length
    elif isinstance(frag, RelaxFragment):
      print_labels_here(frag.labels, length)
      print_bc(frag.bc)
    else:
      assert isinstance(frag, FixedFragment)
      for bc in frag.seq:
        print_labels_here(frag.labels, length)
        print_bc(bc)
        length += bc.length
    frag = frag.next

def relax_fragments(frag: Fragment):
  pass

def assemble_fragments(frag: Fragment):
  pass

def drive(str):
  print_fragments(fragmentize(parse(str)))
