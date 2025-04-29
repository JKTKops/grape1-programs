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
import sys
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
    | int_bc | ld_bc | llcv_bc | chr
    | branch
    | jump
    | ldg
    | mkobj
    | mkclo
    | match 

  nullary: NULLARY_BC
  tjump: TJUMP INT LABEL
  int_bc: INT_BC INT
  llcv_bc: /LLCV/ INT INT
  ld_bc: LD_BC SIGNED_INT
  branch: BRANCH LABEL
  jump: JUMP LABEL
  ldg: LDG LABEL
  chr: /CHR/ "'" /[^\\]|\\\d+/ "'" // very awk but wcyd
  mkobj: /MKOBJ/ INT INT
  mkclo: MKCLO INT LABEL INT
  match: /MATCH/  INT ","         _list1{LABEL,","}
       | /MATCHD/ INT "," INT "," _list1{LABEL,","}
  
  NULLARY_BC: /NOP|HLT|NIL|I0|I1|BT|BF|LDE|POP|DUP|TUCK|TUCK2|SWAP|OVER|ADD|SUB|MUL|DIV|REM|NEG|SHL|SHR|ASR|INC|AND|OR|XOR|TAG|I2(B|C)|(B|C)2I|CLONE|UNPCK|LLCL[0-9]|SLCL[0-9]|LDCV[0-3]|LLCV[1-2][0-3]|EAP[0-5]|ETL[0-5]|CAP[1-6]|CTL[1-6]|UAP|UTL|RET/
  JUMP:   /GOTO|FASTAP|FASTTL/
  BRANCH: /IFEQ|IFNE|IFLT|IFGT|IFLE|IFGE|IFF|IFNF|IFCEQ|IFCNE|IFCLT|IFCGT|IFCLE|IFCGE|IFSAME|IFDIFF|IFNIL|IFNNIL|IFFALSY|IFTRUTHY/
  TJUMP:  /IFTEQ|IFTLT/
  INT_BC: /LLCL|SLCL|LDCV|ALLOC|LFLDW?|SFLDW?|EAP|ETL|SYS|CHKTAG|STAG/
  LD_BC: /LD(H|W|D)?/
  LDG: /LDG(W|D)?/
  MKCLO: /MKCLO|MKLCLO/

  LABEL: /[$._A-Za-z][$._A-Za-z0-9]*/

  %import common.INT    // this is actually a natural number
  %import common.SIGNED_INT // this is not
  %import common.ESCAPED_STRING
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

  def builtin_relocation(self) -> bytes:
    assert self.segment == Segment.BUILTIN
    return Label.builtin_reloc[self.name]

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
  def get_tag(self) -> int:
    raise ValueError(f'Object {self} of type Static should not exist!')
@dataclass
class UserObj(Static):
  tag: int
  fields: List[Label]
  def get_tag(self):
    return self.tag
@dataclass
class Closure(Static):
  code_pointer: Label
  arity: int
  def get_tag(self):
    return 4
@dataclass
class StaticInt(Static):
  value: int
  def get_tag(self):
    return 0
@dataclass
class StaticChar(Static):
  value: str # must be single letter
  def get_tag(self):
    return 2
@dataclass
class StaticString(Static):
  value: str
  def get_tag(self):
    return 3

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

# A sequence of data objects. Data objects have fixed
# lengths but might be rearranged. After rearrangement,
# adjacent data segments may be combined.
@dataclass
class DataFragment(Fragment):
  data: List[Static]
  padding: int = 0

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

@dataclass
class RelaxFragment(Fragment):
  bc: RelaxBytecode
  initialized = False
  def append(self, bc: RelaxBytecode):
    assert not self.initialized
    self.bc = bc
    self.length = bc.current_length
  def relax_type(self):
    return self.bc.relax_type()
  def freeze(self) -> FixedFragment:
    frag = FixedFragment(
      self.offset,
      self.bc.current_length,
      self.labels,
      self.next,
      [self.bc.freeze()]
    )
    for name in self.labels.keys():
      label = Label.get_label(name)
      label.containing_fragment = frag
    return frag

def data_fragment_of(obj: Static, offset: int):
  return DataFragment(
    offset,
    obj.length,
    labels = { obj.label: 0 },
    next = None
  )

class BcParser(Transformer):
  def __init__(self, relocate_everything: bool = False):
    self.relocate_everything = relocate_everything
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
      # if we've been instructed to "relocate everything" then all
      # static data should be marked as needing relocation.
      if self.relocate_everything:
        self.labels_requiring_relocation.add(label.name)
      # If a UserObj contains fields, those fields need relocation.
      # That's not optional! Mark it as such.
      elif isinstance(obj, UserObj) and len(obj.fields) > 0:
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
    # len(content) + 8: we add 4 for the header,
    # 1 for the null terminator, and another 3 for rounding up.
    return StaticString(None, length=(len(content)+8)//4*4, value=content)

  _relax_bytecodes = set([
    'MATCH', 'MATCHD',
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
    if name in ['ALLOC', 'LFLDW', 'SFLDW', 'STAG', 'CHKTAG']:
      size = 3
    elif name in ['LLCL', 'SLCL', 'LDCV', 'SYS']:
      size = 2
    elif name in ['EAP', 'ETL']:
      if arg < 6:
        return FixedBytecode(name + str(arg), [], 1)
      else:
        return FixedBytecode(name, [arg], 2)
    elif name in ['LFLD', 'SFLD']:
      return RelaxBytecode(name, [arg], 2)
    else:
      raise ValueError(f'Unknown INT_BC {name}')
    return FixedBytecode(name, [arg], size)

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

  def llcv_bc(self, content):
    [bc, a1, a2] = content
    assert bc.name == 'LLCV'
    return FixedBytecode(bc.name, [a1,a2], 3)

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
    out_bc = RelaxBytecode(name, content[1:], size)
    out_bc.padding = 0
    return out_bc
    
def parse(input: str, relocate_everything: bool = False):
  tree = parser.parse(input)
  transformer = BcParser(relocate_everything=relocate_everything)
  components = transformer.transform(tree).children
  if transformer.stop_after_transform:
    sys.exit(1)
  return {
    'components': components,
    'desires_low': transformer.labels_desiring_low_offsets,
    'needs_reloc': transformer.labels_requiring_relocation,
    'stop': transformer.stop_after_transform
  }

# Find all of the components requiring relocation and move them into
# the given DataFragment.
def build_reloc_seg(seg: DataFragment, needs_reloc: Set[str],
                    components: List[Static|InputByteCodeSequence],
                    relocate_everything: bool = False):
  data = seg.data
  lbls = seg.labels
  unused_components = []

  len = seg.length # bytes used by reloc segment so far

  for component in components:
    if component.label.name in needs_reloc:
      assert isinstance(component, Static)
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
  _, components = build_reloc_seg(
    reloc_seg, needs_reloc, components,
    relocate_everything=relocate_everything
  )
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
        print(f'  {name}@{frag.offset+off}:')
  def print_obj(obj):
    print(f'    {obj}')
  def print_bc(bc):
    print(f'    {bc}')

  while frag is not None:
    print(f'{type(frag).__name__}@{frag.offset} (len={frag.length})')
    length = 0
    if isinstance(frag, DataFragment):
      length += frag.padding
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

class BcRelax():
  def __init__(self, head):
    self.head = head
    self.changed = False # tracks within a relax iteration
    self.stop_after_relax = False

  def lower_ld(self, prev: Fragment, frag: RelaxFragment):
    assert frag.relax_type() == RelaxType.LD
    value = frag.bc.args[0]
    if -128 <= value <= 127:
      frag.bc.name = 'LDH'
      frag = frag.freeze()
      prev.next = frag
      return frag
    if -32768 <= value <= 32767:
      self.changed = True # fragment is growing
      frag.bc.name, frag.bc.current_length = 'LDW', 3
      frag = frag.freeze()
      prev.next = frag
      return frag
    self.changed = True # fragment is growing
    frag.bc.name, frag.bc.current_length = 'LDD', 5
    frag = frag.freeze()
    prev.next = frag
    if not (-2147483648 <= value <= 2147483647):
      print(f"Error: LD arg {value} exceeds the range of integers")
      self.stop_after_relax = True
    return frag

  # Technically, something that works with LDGW now might not work later if
  # it wasn't moved to the low address section but happens to fit in a low
  # address anyway above something like a MATCH instruction. I think the way
  # we move things to low addresses prevents this from occurring in practice,
  # but to avoid relying on that property, we only fix LDG once it relaxes.
  # When we assemble, remaining LDGs are treated as LDGW.
  def relax_ldg(self, prev: Fragment, frag: RelaxFragment):
    assert frag.relax_type() == RelaxType.ABS_2_TO_3
    label: Label = frag.bc.args[0]
    label_frag = label.containing_fragment
    if label_frag is None:
      # specialize the error message if the label is builtin
      if label.segment == Segment.BUILTIN:
        print(f"Error: builtin label {label} cannot be used in LDG")
      else:
        print(f"Error: undefined label {label}")
      frag = frag.freeze()
      prev.next = frag
      return frag
    if label.offset() > 65535:
      self.changed = True
      frag.bc.name, frag.bc.current_length = 'LDGD', 4
      frag = frag.freeze()
      prev.next = frag
    # otherwise do nothing
    return frag

  # Like LD, LFLD and SFLD are fixed immediately.
  def relax_fld(self, prev: Fragment, frag: RelaxFragment):
    assert frag.relax_type() == RelaxType.ABS_1_TO_2
    arg = frag.bc.args[0]
    if arg <= 255:
      prev.next = frag.freeze()
      return prev.next
    else:
      self.changed = True
      frag.bc.name += 'W'
      frag.bc.current_length = 3
      prev.next = frag.freeze()
      if arg > 65535:
        print(f"Error: {arg} is too many fields")
        self.stop_after_relax = True
      return prev.next

  def align_frag(self, frag: DataFragment):
    # keep frag.offset where it is but make sure there is space in the
    # fragment for padding, and keep track of how much padding that is.
    # The fragment is properly aligned if frag.offset + frag.length
    # is a multiple of 4.
    if (frag.offset + frag.length) % 4 != 0:
      self.changed = True
      total = frag.offset + frag.length + 3
      total = (total // 4) * 4
      old_padding = frag.padding
      frag.padding = total - frag.length - frag.offset
      padding_delta = frag.padding - old_padding
      frag.length = total - frag.offset
      for name in frag.labels.keys():
        frag.labels[name] += padding_delta

  def relax_frag(self, prev: Fragment, frag: Fragment):
    if isinstance(frag, RelaxFragment):
      if frag.relax_type() == RelaxType.LD:
        return self.lower_ld(prev, frag)
      if frag.relax_type() == RelaxType.ABS_1_TO_2:
        return self.relax_fld(prev, frag)
      if frag.relax_type() == RelaxType.ABS_2_TO_3:
        return self.relax_ldg(prev, frag)
      if frag.relax_type() == RelaxType.MATCH_PAD:
        self.fix_match_padding(frag)
        return frag
      assert False
    if isinstance(frag, DataFragment):
      # align fragment up to multiple of 4
      self.align_frag(frag)
    return frag

  @dataclass
  class MatchPadding():
    starts_at: int # how many bytes into the instr does padding begin?
    align_to:  int # what modulus (mod 4) should the padding align to?
  _match_align_to = {
    'MATCH':  MatchPadding(3, 0),
    'MATCHD': MatchPadding(1, 0),
  }

  def fix_match_padding(self, frag: RelaxFragment):
    assert frag.relax_type() == RelaxType.MATCH_PAD
    info = BcRelax._match_align_to[frag.bc.name]
    old_padding = frag.bc.padding
    pad_start = frag.offset + info.starts_at
    pad_modulus = pad_start % 4
    padding = info.align_to + 4 - pad_modulus
    if padding >= 4:
      padding -= 4
    frag.bc.padding = padding
    frag.bc.current_length += padding - old_padding
    frag.length = frag.bc.current_length

  # Find all RelaxFragments whose RelaxType is MATCH_PAD and fix
  # up the padding and offsets throughout the entire program.
  # This is a useful first pass that prevents certain cases of immediate
  # relaxation. Later passes will fix match padding as they work.
  def fix_up_padding(self):
    frag = self.head
    frag_offset = frag.offset
    while frag is not None:
      frag.offset = frag_offset
      if isinstance(frag, RelaxFragment) and frag.relax_type() == RelaxType.MATCH_PAD:
        self.fix_match_padding(frag)
      frag_offset += frag.length
      frag = frag.next

  def relax_pass(self):
    self.changed = False
    frag = self.head
    prev = self.head
    while frag is not None:
      frag = self.relax_frag(prev, frag)
      prev, frag = frag, frag.next
      if frag is not None:
        frag.offset = prev.offset + prev.length

  def relax(self) -> Fragment:
    self.fix_up_padding()
    self.changed = True
    while self.changed:
      self.relax_pass()
    return self.head

def relax_fragments(frag: Fragment) -> Fragment:
  relaxer = BcRelax(frag)
  relaxer.relax()
  if relaxer.stop_after_relax:
    sys.exit(1)
  return relaxer.head

class BcAssembler():
  def reset(self):
    self.bytes = bytearray()
    self.offset = 0
    self.suppress_range_error = False
    self.stop_after_assembly = False

  def __init__(self):
    self.reset()

  def error(self, msg):
    print('Error:', msg)
    self.stop_after_assembly = True

  def check_match_bounds(self, lb: int, ub: int):
    lower = -(2**15)
    upper =  (2**15)-1
    for b in [lb, ub]:
      if b < lower or b > upper:
        self.error(f'Match bound {b} cannot be represented in 2 bytes')

  def check_range(self, v: int, num_bytes: int, signed: bool, num_bits=None):
    if num_bits is None:
      num_bits = num_bytes * 8
    if signed:
      lb = -(2**(num_bits-1))
      ub =  (2**(num_bits-1)) - 1
    else:
      lb = 0
      ub = (2**num_bits) - 1
    if (lb > v or v > ub) and not self.suppress_range_error:
      self.error(f'Value {v} is not within the range of a {num_bits}-bit value')
    if self.suppress_range_error:
      self.suppress_range_error = False

  def assemble_int(self, v: int, num_bytes: int, signed: bool):
    self.check_range(v, num_bytes, signed)
    self.offset += num_bytes
    while num_bytes > 0:
      self.bytes.append(v & 255)
      v >>= 8
      num_bytes -= 1
  def assemble_int1(self, v:int, signed:bool = True):
    self.assemble_int(v, 1, signed)
  def assemble_int2(self, v:int, signed:bool = False):
    self.assemble_int(v, 2, signed)
  def assemble_int3(self, v:int, signed:bool = False):
    self.assemble_int(v, 3, signed)
  def assemble_int4(self, v:int, signed:bool = False):
    self.assemble_int(v, 4, signed)

  def assemble_label(self, lbl: Label, num_bytes: int, absolute: bool):
    assert(absolute or num_bytes == 2)
    if lbl.segment == Segment.BUILTIN:
      if not absolute:
        self.error(f'builtin label {lbl} cannot be used in relative context')
      if num_bytes != 4:
        self.error(f'builtin label {lbl} can only be used in reloc data')
        for _ in range(num_bytes):
          self.bytes.append(0)
      else:
        self.bytes += lbl.builtin_relocation()
      self.offset += num_bytes
    else:
      offset = lbl.offset()
      if not absolute:
        assert self.current_bc.startswith('IF')
        offset = offset - self.offset
        if self.current_bc in ['IFTEQ', 'IFTLT']:
          offset += 2 # relative offset is preceeded by 1-byte tag and 1-byte opcode
        else:
          offset += 1 # use byte one before self.offset, the branch opcode
      if not (-32768 <= offset <= 32767):
        self.error(f'label {lbl}@{lbl.offset()} is out of range from {self.offset}')
        self.suppress_range_error = True
      self.assemble_int(offset, num_bytes, signed=not absolute)

  def asm_nullary(self, _bc: FixedBytecode):
    return

  def asm_i1(self, bc: Bytecode):
    [v] = bc.args
    self.assemble_int1(v)
  def asm_i2(self, bc: Bytecode):
    [v] = bc.args
    self.assemble_int2(v, signed=True)
  def asm_i4(self, bc: Bytecode):
    [v] = bc.args
    self.assemble_int4(v, signed=True)

  def asm_u1(self, bc: Bytecode):
    [v] = bc.args
    self.assemble_int1(v, signed=False)
  def asm_u2(self, bc: Bytecode):
    [v] = bc.args
    self.assemble_int2(v, signed=False)

  def asm_u1i1(self, bc: Bytecode):
    [u, i] = bc.args
    self.assemble_int1(u, signed=False)
    self.assemble_int1(i, signed=True)
  def asm_u1u1(self, bc: Bytecode):
    for u in bc.args:
      self.assemble_int1(u, signed=False)
  def asm_u1u2(self, bc: Bytecode):
    [u1, u2] = bc.args
    self.assemble_int1(u1, signed=False)
    self.assemble_int2(u2, signed=False)

  def asm_abs2(self, bc: Bytecode):
    [v] = bc.args
    self.assemble_label(v, 2, absolute=True)
  def asm_abs3(self, bc: Bytecode):
    [v] = bc.args
    self.assemble_label(v, 3, absolute=True)
  def asm_rel2(self, bc: Bytecode):
    [v] = bc.args
    self.assemble_label(v, 2, absolute=False)

  def asm_clo(self, bc: Bytecode):
    [n, cp, a] = bc.args
    self.assemble_int1(n, signed=False)
    self.assemble_label(cp, 3, absolute=True)
    self.assemble_int1(a, signed=False)

  def asm_branch(self, bc: Bytecode):
    if bc.name in ['IFTEQ', 'IFTLT']:
      self.assemble_int1(bc.args[0], signed=False)
      ro = bc.args[1]
    else:
      ro = bc.args[0]
    self.assemble_label(ro, 2, absolute=False)

  def asm_match(self, bc: RelaxBytecode):
    if bc.name == 'MATCH':
      # N, then padding, then branch targets
      self.assemble_int2(bc.args[0], signed=False)
      for _ in range(bc.padding):
        self.bytes.append(0)
      labels = bc.args[1:]
    else:
      # Padding, then Lo/Hi/targets
      for _ in range(bc.padding):
        self.bytes.append(0)
      self.check_match_bounds(bc.args[0], bc.args[1])
      self.suppress_range_error = True
      self.assemble_int2(bc.args[0])
      self.suppress_range_error = True
      self.assemble_int2(bc.args[1])
      labels = bc.args[2:]
    for lbl in labels:
      self.assemble_label(lbl, 4, absolute=True)

  nullary_bcs = {
    'HLT': 0x00, 'NOP': 0xff, 'NIL': 0x01, 'I0': 0x02, 'I1': 0x03, 'BT': 0x04, 'BF': 0x05,
    'LDE': 0x0e, 'POP': 0x0f, 'DUP': 0x10, 'TUCK': 0x11, 'TUCK2': 0x12, 'SWAP': 0x13,
    'OVER': 0x14, 'ADD': 0x18, 'SUB': 0x19, 'MUL': 0x1a, 'DIV': 0x1b, 'REM': 0x1c,
    'NEG': 0x1d, 'SHL': 0x1e, 'SHR': 0x1f, 'ASR': 0x20, 'AND': 0x22, 'OR': 0x23, 'XOR': 0x24,
    'TAG': 0x28, 'I2B': 0x2a, 'I2C': 0x2b, 'B2I': 0x2c, 'C2I': 0x2d, 'CLONE': 0x32,
    'UNPCK': 0x33, 'UAP': 0x70, 'UTL': 0x71, 'RET': 0x72
  }
  relative_js = {
    'EQ': 0x40, 'NE': 0x41, 'LT': 0x42, 'GT': 0x43, 'LE': 0x44, 'GE': 0x45,
    'F': 0x46, 'NF': 0x47, 'CEQ': 0x48, 'CNE': 0x49, 'CLT': 0x4a, 'CGT': 0x4b,
    'CLE': 0x4c, 'CGE': 0x4d, 'SAME': 0x4e, 'DIFF': 0x4f,
    'NIL': 0x50, 'NNIL': 0x51, 'FALSY': 0x52, 'TRUTHY': 0x53,
    'IFTEQ': 0x58, 'IFTLT': 0x59
  }
  table = {
    'LDH': (0x06, asm_i1), 'LDW': (0x07, asm_i2), 'LDD': (0x08, asm_i4),
    'CHR': (0x09, asm_u1), 'LDGW': (0x0a, asm_abs2), 'LDGD': (0x0b, asm_abs3),
    'LLCL': (0x0c, asm_u1), 'SLCL': (0x0d, asm_u1), 'INC': (0x21, asm_u1i1),
    'STAG': (0x29, asm_u2), 'ALLOC': (0x2e, asm_u2), 'MKOBJ': (0x2f, asm_u1u2),
    'MKCLO': (0x30, asm_clo), 'MKLCLO': (0x31, asm_clo),
    'LFLD': (0x34, asm_u1), 'LFLDW': (0x35, asm_u2),
    'SFLD': (0x36, asm_u1), 'SFLDW': (0x37, asm_u2),
    'LDCV': (0x38, asm_u1), 'LLCV': (0x39, asm_u1u1),
    'GOTO': (0x54, asm_abs3), 'MATCH': (0x55, asm_match), 'MATCHD': (0x56, asm_match),
    'CHKTAG': (0x57, asm_u2),
    'FASTAP': (0x60, asm_abs3), 'FASTTL': (0x61, asm_abs3),
    'EAP': (0x62, asm_u1), 'ETL': (0x63, asm_u1), 'SYS': (0x73, asm_u1)
  }
  i = 0x64
  for e in ['EAP', 'ETL']:
    for n in range(6):
      nullary_bcs[e+str(n)] = i
      i += 1
  i = 0x74
  for c in ['CAP', 'CTL']:
    for n in range(1, 7):
      nullary_bcs[c+str(n)] = i
      i += 1
  i = 0x80
  for lcl in ['LLCL', 'SLCL']:
    for n in range(10):
      nullary_bcs[lcl+str(n)] = i
      i += 1
  for fld in ['LFLD', 'SFLD', 'LDCV', 'LLCV1', 'LLCV2']:
    for n in range(4):
      nullary_bcs[fld+str(n)] = i
      i += 1
  del i

  table['LDG'] = table['LDGW']
  for name,b in nullary_bcs.items():
    table[name] = (b, asm_nullary)
  for name,b in relative_js.items():
    table['IF'+name] = (b, asm_branch)

  def assemble_bc(self, bc: Bytecode):
    self.bytes.append(BcAssembler.table[bc.name][0])
    self.offset += 1
    self.current_bc = bc.name
    BcAssembler.table[bc.name][1](self,bc)
    # assembler function is responsible for updating offset

  def assemble_static_hdr(self, tag: int, length: int):
    assert length % 4 == 0
    length = length // 4 # write length in words, not bytes
    self.check_range(tag, 2, False)
    self.check_range(length, None, False, 14)
    word  = tag << 16
    word |= length << 2
    word |= 3
    self.assemble_int4(word)

  def assemble_static(self, datum: Static):
    final_offset = self.offset + datum.length
    self.assemble_static_hdr(datum.get_tag(), datum.length)
    match datum:
      case UserObj(fields=fields):
        for label in fields:
          try:
            Label.memo[label.name] # does this label exist?
          except:
            self.error(f'Undefined static label {label.name}')
            label = Label.memo['nil']
          self.assemble_label(label, 4, absolute=True)
      case StaticInt(value=value):
        self.assemble_int4(value, signed=True)
      case StaticChar(value=value):
        value = ord(value)
        self.assemble_int4(value, signed=False)
      case StaticString(value=value):
        self.bytes += bytes(value, encoding='ascii')
        self.bytes.append(0)
        length = len(value) + 1
        while length % 4 != 0:
          self.bytes.append(0)
          length += 1
      case Closure(code_pointer=cp,arity=a):
        value  = cp.offset() << 8
        value |= a
        self.assemble_int4(value)
    self.offset = final_offset

  def assemble(self, frag: Fragment):
    self.reset()
    # we will prepare the header at the end when we know the bc length
    self.offset = 12
    reloc_length = frag.length
    assert reloc_length % 4 == 0
    reloc_length //= 4
    while frag is not None:
      match frag:
        case DataFragment(data=data,padding=padding):
          for _ in range(padding):
            self.bytes.append(0)
          self.offset += padding
          for datum in data:
            self.assemble_static(datum)
        case FixedFragment(seq=seq):
          for bc in seq:
            self.assemble_bc(bc)
        case RelaxFragment(bc=bc):
          self.assemble_bc(bc)
      frag = frag.next
    length = self.offset
    start_label = Label.memo.get('_start')
    if start_label is None:
      start_offset = 0
      self.error('Unable to find entrypoint label `_start\'')
    else:
      start_offset = start_label.offset()
    # build the header and staple it on the front
    post_header = self.bytes
    self.bytes = bytearray()
    self.assemble_int4(length)
    self.assemble_int4(reloc_length)
    self.assemble_int4(start_offset)
    self.bytes += post_header

def assemble_fragments(frag: Fragment) -> bytearray:
  assembler = BcAssembler()
  assembler.assemble(frag)
  if assembler.stop_after_assembly:
    sys.exit(1)
  return assembler.bytes

def drive(str, relocate_everything: bool = False,
          print_output: bool = False) -> bytearray:
  frag = fragmentize(parse(str, relocate_everything=relocate_everything))
  frag = relax_fragments(frag)
  ba = assemble_fragments(frag)
  if print_output:
    print_fragments(frag)
    print(ba.hex(sep=' '))
  return ba

def main(inpath: str, outpath: str, relocate_everything: bool = False):
  with open(inpath, 'r') as f:
    src = f.read()
  ba = drive(src, relocate_everything=relocate_everything, print_output=True)
  with open(outpath, 'wb') as f:
    f.write(ba)

if __name__ == "__main__":
  inpath  = sys.argv[1]
  outpath = sys.argv[2]
  relocate_everything = len(sys.argv) > 3
  main(inpath, outpath, relocate_everything=relocate_everything)
