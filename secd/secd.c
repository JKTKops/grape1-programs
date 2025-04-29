#include <assert.h> // sanity checks
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h> // exit
#include <string.h> // memcpy
#include <sys/types.h>

#ifdef NDEBUG
#define dprintf(...)
#else
#define dprintf(...) fprintf(stderr, __VA_ARGS__)
#endif

// runtime configuration
#define STACK_SZ 102400
#define NUM_STACK_OBJS (STACK_SZ/sizeof(OBJ))
#define HEAP_SZ  1994720
#define NUM_SEMISPACE_OBJS (HEAP_SZ/2*sizeof(OBJ))
#define CODE_SZ  2097152

typedef intptr_t word;
typedef uint8_t  byte;

typedef enum {
  TAG_INT    = 0,
  TAG_BOOL   = 1,
  TAG_CHAR   = 2,
  TAG_STRING = 3,
  TAG_FUN    = 4,
  TAG_PAP    = 5,
  TAG_MAX    = UINT16_MAX,
} tag_t;

/* All heap objects start with this header. */
typedef struct OBJ_HDR {
  // gcc on my platform does least-significant-first ordering
  // so this makes the correct layout. (Verified by loading list0.bc)
  uint16_t must_be_1:1;
  uint16_t stat:1;
  uint16_t size:14;
  uint16_t tag;
} OBJ_HDR;

// construct the correct word value for a dynamic header with the
// given tag and size.
#define HDR(tag, size) ((tag) << 16 | (size) << 2 | 1)

/* Format of a generic heap object. */
typedef struct HPOBJ {
  union {
    OBJ_HDR r;
    struct HPOBJ *f;
  } hd;
  word fields[0];
} HPOBJ;

/* Format of an immediate heap object (INT, BOOL, CHAR) */
typedef struct __IMMED {
  OBJ_HDR hdr;
  word payload;
} __IMMED;
typedef __IMMED INT;
typedef __IMMED BOOL;
typedef __IMMED CHAR;

/* Format of a string object. */
typedef struct STRING {
  OBJ_HDR hdr;
  char str[0]; // must be null-terminated
} STRING;

/* Format of a closure. */
typedef struct FUN {
  OBJ_HDR hdr;
  word padding[0];
  word arity:8;
  word code:24;
  word captures[0];
} FUN;

/* Format of a PAP. */
typedef struct PAP {
  OBJ_HDR hdr;
  word padding[0];
  byte arity; // make PAP::arity line up with FUN::arity
  byte count;
  // there will be 2 bytes padding here in ETCa, 6 bytes in x86
  HPOBJ *funlike; // either a FUN or another PAP or NULL.
  word params[0];
} PAP;

typedef HPOBJ* OBJ; // NULL is the value nil

struct SEMISPACE {
  OBJ area[NUM_SEMISPACE_OBJS];
};
typedef struct HEAP {
  int current_semispace;
  struct SEMISPACE half[2];
} HEAP;

/* [NOTE] Dump Layout ***************************
Each frame on the dump consists of 3 words:
1. A stack pointer
2. A closure (pointer)
3. A code pointer

The second of these is a root for garbage collection.
We always put them on the dump in this order, so the
roots for garbage collection on the dump are every third
word, starting from &stack[1], up to Dp.
*/

typedef struct DUMP_FRAME {
  OBJ *S;
  FUN *E;
  byte *C;
} dump_frame_t;

// let's say fixed space for the bytecode program, because why not
byte code[CODE_SZ] = { 0 }; // 2MB
// These are needed to handle the issue of 8-byte x86 pointers
// but 4-byte bytecode offsets when relocating static data.
// See the comment at the Relocatable Data section header.
word *static_data = NULL;
uint32_t static_data_len;

// Global heap (nongenerational) and stack.
HEAP heap = { 0 };
OBJ stack[NUM_STACK_OBJS];
// Pin these to registers.
  // Hp points to the first unused word.
OBJ *Hp    = heap.half[0].area;
  // HpLim points to the last USABLE word.
  // gc when Hp would exceed HpLim.
OBJ *HpLim = heap.half[0].area + NUM_SEMISPACE_OBJS;
  // HpAlloc stores the amount of the allocation that caused a gc.
  // Do not pin this, just reserve a word for it in upper RAM.
  // In the C code we are storing the size in words, because it
  // matches pointer arithmetic semantics in C, but in assembly we
  // should store the size in bytes.
word HpAlloc;
#define INIT_SP (stack + NUM_STACK_OBJS)
  // Sp points to the top word on the stack, which is actually at
  // the lowest address. Initially nothing is on the stack, so it
  // points past the end of &stack.
OBJ *Sp    = INIT_SP;
  // Dp points to the topmost frame on the dump.
  // Initially there is nothing here because we have to initialize
  // this frame in main (empty S, null E, C points to a HLT bytecode
  // somewhere in the program).
  // When Sp == Dp+1, the stack is exactly full (this is not checked).
dump_frame_t *Dp = (void*)stack;
// We might also want to pin a copy of the S in the top dump frame
// (less one) to a register as this is the "locals" pointer.
OBJ *Lcl = INIT_SP-1;
  // The currently-executing function.
FUN *E  = NULL;
  // Pointer to current byte.
byte *C = code;

#define PUSH(exp) *--Sp=(exp)
#define POP(dst)  (dst)=*Sp++
#define LCL(n)    (Lcl[-(n)])

void die(char *msg) {
  fputs(msg, stdout);
  exit(1);
}

/* [Note: Non-relocatable Static Data]
When static data is not in the relocatable segment, this interpreter has a
major problem: the LDG and LDGW instructions which try to push pointers to
that data would indicate an object whose format (using 4-byte words)
is incompatible with HPOBJ (using 8-byte words). The use of 8-byte words in
HPOBJ is non-negotiable in the interpreter as it really must be able to store
whole pointers.

For relocatable static data, this is not so hard to handle:
we can eagerly extract (and adjust for word size) all of the relocatable data
into a separate relocatable static data region as it is loaded.
In fact, we must do this, because the entire point of relocation is to correct
the pointers in the data -- and they must be corrected to 8-byte values.
We have no choice.

For non-relocatable static data, however, we can do no such thing.
In order to do so, we would have to find the data during relocations,
but the bytecode format gives us no mechanism to find it.

In the future, we may perhaps solve this problem by lazily extracting static
data into a growable region when we first encounter an LDG(W) that interacts
with it. (Or FASTAP/FASTTL.) This comes with some challenges, mainly,
the need to distinguish in-code static data from data that hasn't yet been
extracted, and the possibility of a need to distinguish relocatable static
data (allocated eagerly into a separate non-growable region)
from non-relocatable static data.
This last need could potentially be dodged by using the same region for both.
This would have performance implications as copying the whole region when it
needs to grow would become rather slow.
A hybrid data structure which uses a linked list of arraylists, growing the
last arraylist until it reaches some limit and then making a new chunk instead,
could be appropriate here. I don't know. It all sounds rather complicated,
and the straightforward approach would have to be profiled to determine if it
is problematic in the first place.

For now, the solution is simple: to use this bytecode interpreter, you must
make all static data relocatable.
*/

// See above. This can be made more complicated if we take a diff approach.
#define GET_STATIC(offset) \
  (OBJ)(static_data + *(uint32_t*)(code + offset))

/******************************* garbage collection *************************/

void gc(void);
OBJ gc_evacuate(OBJ obj);
void gc_scavenge(OBJ obj);

#define SMALL_INT(i)  {.hdr={.tag=TAG_INT,.size=2,.stat=1,.must_be_1=1},.payload=i}
INT common_INTs[11] = {
  SMALL_INT(-1),
  SMALL_INT(0),
  SMALL_INT(1),
  SMALL_INT(2),
  SMALL_INT(3),
  SMALL_INT(4),
  SMALL_INT(5),
  SMALL_INT(6),
  SMALL_INT(7),
  SMALL_INT(8),
  SMALL_INT(9),
};
#undef SMALL_INT
#define BOOL(b) {.hdr={.tag=TAG_BOOL,.size=2,.stat=1,.must_be_1=1},.payload=b}
BOOL common_BOOLs[2] = {
  BOOL(0),
  BOOL(1),
};
#undef BOOL

void gc(void) {
  heap.current_semispace = 1 - heap.current_semispace;
  Hp = heap.half[heap.current_semispace].area;
  OBJ Work = (OBJ)Hp; // points to first object in to-space not yet scavenged
  HpLim = Hp + NUM_SEMISPACE_OBJS;

  // gc roots: E, every word from Sp to &stack+sizeof(stack), dump roots
  E = (FUN*)gc_evacuate((OBJ)E);
  for (OBJ *Root = Sp; Root < stack + NUM_STACK_OBJS; ++Root) {
    *Root = gc_evacuate(*Root);
  }
  for (dump_frame_t *F = (void*)stack; F < Dp; ++F) {
    F->E = (FUN*)gc_evacuate((OBJ)(F->E));
  }

  // Now we scavange objects in to-space until there's nothing left to
  // scavenge.
  while (Work < (OBJ)Hp) {
    gc_scavenge(Work);
    Work += (Work)->hd.r.size;
  }

  // Finally check if we were actually able to free sufficient memory.
  if (HpLim - Hp >= HpAlloc) return;
  die("out of memory");
}

// Given a pointer to something in from-space, either return the forwarding
// pointer, or copy it to to-space and overwrite the header with a pointer
// to the new location.
OBJ gc_evacuate(OBJ obj) {
  if (!obj) return NULL;

  if (!obj->hd.r.must_be_1) {
    // object already evacuated, header overwritten with fwding pointer
    return obj->hd.f;
  }

  if (obj->hd.r.stat) {
    // object is static and better not be messed with!
    return obj;
  }

  OBJ newptr;

  // commoning for small integers and booleans
  if (obj->hd.r.tag == TAG_INT &&
      // note that these are signed comparisons
      obj->fields[0] >= -1 && obj->fields[0] < 10) {
    newptr = (OBJ)&common_INTs[obj->fields[0]+1];
    goto update;
  }
  if (obj->hd.r.tag == TAG_BOOL) {
    newptr = (OBJ)&common_BOOLs[obj->fields[0]];
    goto update;
  }

  // must be evacuated. How big is it?
  uint16_t size = obj->hd.r.size;
    // note that sizeof(OBJ) == sizeof(word) by defn
  // This heap check is not necessary because while copying from a semispace
  // that was not overfull, we cannot end up with an overfull semispace.
  //if (Hp + size > HpLim) die("out of memory");
  newptr = (OBJ)Hp;
  memcpy(Hp, obj, size*sizeof(OBJ));
  Hp += size;
update:
  obj->hd.f = newptr;
  return newptr;
}

// Given a pointer to something in to-space, evacuate each of its fields
// and update the fields to point to their new locations.
// (This function can probably be inlined into gc().)
void gc_scavenge(OBJ obj) {
  // TAKE CARE! INT, BOOL, CHAR, STRING, FUN, and PAP all have non-OBJ
  // data in their first (and possibly only) field. Skip the first
  // field for those objects.
  tag_t tag = obj->hd.r.tag;
  uint16_t size = obj->hd.r.size;
  uint16_t work = 0;
  // faster check for this is tag <= TAG_PAP
  if (tag == TAG_INT || tag == TAG_BOOL || tag == TAG_CHAR
      || tag == TAG_STRING || tag == TAG_FUN || tag == TAG_PAP) {
    work = 1;
  }

  for (; work < size-1; ++work) {
    obj->fields[work] = (word)gc_evacuate((OBJ)obj->fields[work]);
  }
}

/*********************** Debugging Facilities ************************/
// These are for debugging the C code and do not need to be compiled
// to the ETCa version.
void print_obj(OBJ obj) {
  if (!obj) {
    printf("nil");
    return;
  }
  tag_t tag = obj->hd.r.tag;
  printf("%p:%s",obj, obj->hd.r.stat ? "[s]" : "   ");
  FUN *fun; // just obj pointer, casted
  PAP *pap; // just obj pointer, casted
  uint32_t field = 0;
  switch (tag) {
    case TAG_INT:
      printf("INT(%ld)", obj->fields[0]); return;
    case TAG_BOOL:
      printf("BOOL(%s)", obj->fields[0] ? "true" : "false"); return;
    case TAG_CHAR:
      printf("CHAR(%c)", (char)obj->fields[0]); return;
    case TAG_STRING:
      printf("STRING(%s)", (char*)obj->fields); return;
    case TAG_FUN:
      fun = (FUN*)obj;
      printf("FUN(0x%x,%d", fun->code, fun->arity);
      field = 1;
      break;
    case TAG_PAP:
      pap = (PAP*)obj;
      printf("PAP(%p,%d", pap->funlike, pap->arity);
      field = 2;
      break;
    default:
      printf("CON(%d", tag);
      break;
  }

  for (; field < obj->hd.r.size-1; ++field) {
    printf(",%p", (OBJ)obj->fields[field]);
  }
  printf(")");
}

void print_internal_objs() {
  printf("Common INTs:\n");
  for (int i = 0; i < sizeof(common_INTs)/sizeof(INT); ++i) {
    print_obj((OBJ)&common_INTs[i]); printf("\n");
  }
  printf("\nCommon BOOLs:\n");
  print_obj((OBJ)&common_BOOLs[0]); printf("\n");
  print_obj((OBJ)&common_BOOLs[1]); printf("\n\n");

  printf("Static data array (@%p):\n", static_data);
  {
    word *W = static_data;
    while (W < static_data + static_data_len) {
      OBJ obj = (OBJ)W;
      W += obj->hd.r.size;
      printf("  "); print_obj(obj); printf("\n");
    }
  }
  printf("\n");
}

void print_heap() {
  printf("**************** HEAP ****************\n");
  printf("Current semispace: %d\n", heap.current_semispace);
  for ( OBJ W = (OBJ)heap.half[heap.current_semispace].area
      ; W < (OBJ)Hp; W += W->hd.r.size) {
    print_obj(W); printf("\n");
  }
  printf("************** END HEAP **************\n");
  printf("\n");
}

void print_machine_state() {
  int i;
  OBJ *o;
  printf("Stack, 0 is bottom:\n");
  for (i = 0, o = &stack[NUM_STACK_OBJS-1]; o >= Sp; ++i, --o) {
    printf("%d: ", i);
    print_obj(*o);
    printf("\n");
  }
  printf("Dump, 0 is bottom (S index, C offset, E):\n");
  i = 0;
  for (dump_frame_t *F = (void*)stack; (void*)F <= (void*)Dp; ++i, ++F) {
    printf("%d: (%ld, 0x%lx, ", i, INIT_SP-1 - F->S, F->C - code);
    print_obj((OBJ)F->E);
    printf(")\n");
  }
  printf("E: "); print_obj((OBJ)E); printf("\n");
  printf("C: 0x%lx\n", C - code); 

  print_heap();
}

/**************** BYTECODES **************** */

typedef struct BC_HDR {
  uint32_t byte_len, reloc_seg_len, entry_off;
} bc_header_t;

enum BYTECODE {
  BC_HLT = 0x00,
  /* Standard stack manip */
  BC_NIL = 0x01,
  BC_I0  = 0x02,
  BC_I1  = 0x03,
  BC_BT  = 0x04,
  BC_BF  = 0x05,
  BC_LDH = 0x06,
  BC_LDW = 0x07,
  BC_LDD = 0x08,
  BC_CHR = 0x09,
  BC_LDGW = 0x0A,
  BC_LDGD = 0x0B,
  BC_LLCL = 0x0C,
  BC_SLCL = 0x0D,
  BC_LDE = 0x0E,
  BC_POP = 0x0F,
  BC_DUP = 0x10,
  BC_TUCK = 0x11,
  BC_TUCK2 = 0x12,
  BC_SWAP = 0x13,
  BC_OVER = 0x14,
  /* Arithmetic */
  BC_ADD = 0x18,
  BC_SUB = 0x19,
  BC_MUL = 0x1A,
  BC_DIV = 0x1B,
  BC_REM = 0x1C,
  BC_NEG = 0x1D,
  BC_SHL = 0x1E,
  BC_SHR = 0x1F,
  BC_ASR = 0x20,
  BC_INC = 0x21,
  BC_AND = 0x22,
  BC_OR  = 0x23,
  BC_XOR = 0x24,
  /* Object manip */
  BC_TAG = 0x28,
  BC_STAG = 0x29,
  BC_I2B = 0x2A,
  BC_I2C = 0x2B,
  BC_B2I = 0x2C,
  BC_C2I = 0x2D,
  BC_ALLOC = 0x2E,
  BC_MKOBJ = 0x2F,
  BC_MKCLO = 0x30,
  BC_MKLCLO = 0x31,
  BC_CLONE = 0x32,
  BC_UNPCK = 0x33,
  BC_LFLD  = 0x34,
  BC_LFLDW = 0x35,
  BC_SFLD  = 0x36,
  BC_SFLDW = 0x37,
  BC_LDCV = 0x38,
  BC_LLCV = 0x39,
  /* Intra-function control */
  BC_IFEQ = 0x40,
  BC_IFNE = 0x41,
  BC_IFLT = 0x42,
  BC_IFGT = 0x43,
  BC_IFLE = 0x44,
  BC_IFGE = 0x45,
  BC_IFF  = 0x46,
  BC_IFNF = 0x47,
  BC_IFCEQ = 0x48,
  BC_IFCNE = 0x49,
  BC_IFCLT = 0x4A,
  BC_IFCGT = 0x4B,
  BC_IFCLE = 0x4C,
  BC_IFCGE = 0x4D,
  BC_IFSAME = 0x4E,
  BC_IFDIFF = 0x4F,
  BC_IFNIL  = 0x50,
  BC_IFNNIL = 0x51,
  BC_IFFALSY  = 0x52,
  BC_IFTRUTHY = 0x53,
  BC_GOTO   = 0x54,
  BC_MATCH  = 0x55,
  BC_MATCHD = 0x56,
  BC_CHKTAG = 0x57,
  BC_IFTEQ  = 0x58,
  BC_IFTLT  = 0x59,
  /* Inter-function control */
  BC_FASTAP = 0x60,
  BC_FASTTL = 0x61,
  BC_EAP  = 0x62,
  BC_ETL  = 0x63,
  BC_EAP0 = 0x64,
  BC_EAP1 = 0x65,
  BC_EAP2 = 0x66,
  BC_EAP3 = 0x67,
  BC_EAP4 = 0x68,
  BC_EAP5 = 0x69,
  BC_ETL0 = 0x6A,
  BC_ETL1 = 0x6B,
  BC_ETL2 = 0x6C,
  BC_ETL3 = 0x6D,
  BC_ETL4 = 0x6E,
  BC_ETL5 = 0x6F,
  BC_UAP  = 0x70,
  BC_UTL  = 0x71,
  BC_RET  = 0x72,
  BC_SYS  = 0x73,
  BC_CAP1 = 0x74,
  BC_CAP2 = 0x75,
  BC_CAP3 = 0x76,
  BC_CAP4 = 0x77,
  BC_CAP5 = 0x78,
  BC_CAP6 = 0x79,
  BC_CTL1 = 0x7A,
  BC_CTL2 = 0x7B,
  BC_CTL3 = 0x7C,
  BC_CTL4 = 0x7D,
  BC_CTL5 = 0x7E,
  BC_CTL6 = 0x7F,
  /* Shortened bytecodes */
  BC_LLCL0 = 0x80,
  BC_LLCL1 = 0x81,
  BC_LLCL2 = 0x82,
  BC_LLCL3 = 0x83,
  BC_LLCL4 = 0x84,
  BC_LLCL5 = 0x85,
  BC_LLCL6 = 0x86,
  BC_LLCL7 = 0x87,
  BC_LLCL8 = 0x88,
  BC_LLCL9 = 0x89,
  BC_SLCL0 = 0x8A,
  BC_SLCL1 = 0x8B,
  BC_SLCL2 = 0x8C,
  BC_SLCL3 = 0x8D,
  BC_SLCL4 = 0x8E,
  BC_SLCL5 = 0x8F,
  BC_SLCL6 = 0x90,
  BC_SLCL7 = 0x91,
  BC_SLCL8 = 0x92,
  BC_SLCL9 = 0x93,
  BC_LFLD0 = 0x94,
  BC_LFLD1 = 0x95,
  BC_LFLD2 = 0x96,
  BC_LFLD3 = 0x97,
  BC_SFLD0 = 0x98,
  BC_SFLD1 = 0x99,
  BC_SFLD2 = 0x9A,
  BC_SFLD3 = 0x9B,
  BC_LDCV0 = 0x9C,
  BC_LDCV1 = 0x9D,
  BC_LDCV2 = 0x9E,
  BC_LDCV3 = 0x9F,
  BC_LLCV10 = 0xA0,
  BC_LLCV11 = 0xA1,
  BC_LLCV12 = 0xA2,
  BC_LLCV13 = 0xA3,
  BC_LLCV20 = 0xA4,
  BC_LLCV21 = 0xA5,
  BC_LLCV22 = 0xA6,
  BC_LLCV23 = 0xA7,
  /* nop */
  BC_NOP = 0xFF,
};

void load_bytecode(char *path) {
  FILE *bcf = fopen(path, "rb");
  size_t frn;
  bc_header_t *hdr = (void*)code;
  // read the header into code
  frn = fread(code, sizeof(bc_header_t), 1, bcf);
  if (!frn) die("unable to read bytecode header");
  C = code + hdr->entry_off;

  uint32_t *p = (uint32_t*)(code + 12);
  uint32_t *reloc_end = p + hdr->reloc_seg_len;
  uint32_t *prog_end  = (uint32_t*)(code + hdr->byte_len);

  /* relocatable data */
  // now we have to deal with a difference between compiling for my x86
  // computer and for ETCa. The 4-byte fields in the bytecode program
  // are perfect for ETCa, but pose a huge problem here: we want to overwrite
  // them with pointers, but we cannot overwrite a 4-byte field with an
  // 8-byte pointer.
  // The solution is, as always, another layer of indirection.
  // We allocate (dynamically) a second array with space for as many (x86)
  // words as the bytecode declares for the relocation segment. This means
  // we allocate twice as many bytes as the segment takes in the bytecode
  // file. Then we copy the objects into the array in the correct format for
  // 8-byte pointers, and overwrite the static header with the offset of the
  // object in this array. Whenever the program uses the LDGW or LDGD bytecodes,
  // we find the static object, read the offset in the array, and push that
  // pointer instead. What a mess, but what can you do?

  // We have this same problem for data in the program which is pushed by
  // LDG(W). At the moment, for use with this interpreter, all static data
  // must be placed in the relocatable segment.
  // See [Note: Non-relocatable Static Data].

  // The relocations themselves follow the same logic for both portable C
  // and ETCa.

  static_data = calloc(hdr->reloc_seg_len, sizeof(word));
  uint32_t static_index = 0;

  while (p < reloc_end) {
    // read objhdr of next object
    frn = fread(p, sizeof(OBJ_HDR), 1, bcf);
    OBJ_HDR *hdr = (OBJ_HDR*)p;
    uint16_t size = hdr->size;
    OBJ obj = (OBJ)(static_data + static_index);
    static_data[static_index] = *p; // copy header to static arr
    *p = static_index++; // overwrite bytecode object header with location,
      // the "static index" of the object.
    dprintf("Overwriting static header at 0x%lx with %d\n", (byte*)p-code, static_index-1);

    // handle static strings specially (getting this out of the way here
    // is also important for ETCa as strings aren't reloc'd).
    if (obj->hd.r.tag == TAG_STRING) {
      // read the string directly into static data array,
      // header has already been read so size-1 words remain.
      frn = fread(&obj->fields[0], sizeof(uint32_t), size-1, bcf);
      p += size;
      // sizeof(uint32_t)/sizeof(word) is not an integer on x86 and probably
      // also isn't on other platforms, unfortunately,
      // so we can't just add size*4/sizeof(word) to static_index as we would
      // lose half-words. For portability, we add (sizeof(word)-1) to the
      // numerator, which ensures that the result of the division doesn't lose
      // fractional words needed for the end of the string.
      int bytesize = size * 4;
      static_index += (bytesize + sizeof(word) - 1) / sizeof(word);
      continue;
    }
    p++; // advance p past the bytecode object's header.
    for (int i = 0; i < obj->hd.r.size-1; ++i) {
      // read 4 bytes from the bytecode file 
      frn = fread(p, sizeof(uint32_t), 1, bcf);
      dprintf("Read 0x%x\n", *p);
      // extract that 4 byte value so we can determine how to reloc,
      // and increment p to prepare for the next field.
      uint32_t relocation = *p++;
      // If this is the first iteration and the object is
      // immediate or FUN, its first field is not an object and must not
      // be relocated.
      if (i == 0 && obj->hd.r.tag <= TAG_FUN) {
        static_data[static_index++] = relocation;
        continue;
      }
      dprintf("Relocating 0x%x ... ", relocation);
      if (relocation == 0) {
        dprintf("nil\n");
        static_data[static_index++] = 0;
        continue; // don't relocate nil
      }
      if (relocation - 0xFF000000 <= 1) {
        // relocate offset to common boolean
        static_data[static_index++] =
          (word)&common_BOOLs[relocation-0xFF000000];
        dprintf("%p\n", &common_BOOLs[relocation-0xFF000000]);
      } else if (relocation - 0xFF000002 <= 10) {
        // relocate offset to common int
        static_data[static_index++] =
          (word)&common_INTs[relocation-0xFF000002];
        dprintf("%p\n", &common_INTs[relocation-0xFF000002]);
      } else if (relocation >= 0x20000000) {
        die("malformed relocation");
      } else {
        // relocate to bytecode offset. Unfortunately, that offset might
        // be further in the file than we are currently. Fortunately,
        // that's OK, because that object will have its "static index"
        // set later. We just have to set the static object's field here
        // to have the bytecode offset and a bottom-bit tag so that we
        // know to chase the pointer when we go back over it shortly.
        // In ETCa, just write `code+relocation` into the bytecode obj's
        // field directly, that is the correct offset.
        static_data[static_index++] = (relocation << 1) | 1;
        dprintf("delaying (to offset 0x%x)\n", relocation);
      }
    }
  }

  // x86 only: go over all the objects in the static data array and update
  // delayed relocations. Also, sanity check the array size.
  // (equal is OK because we keep it pointing past last used index.)
  assert(static_index <= hdr->reloc_seg_len);
  static_data_len = static_index;
  {
    word *W = static_data;
    while (W < static_data + static_data_len) {
      OBJ obj = (OBJ)W;
      W += obj->hd.r.size;
      if (obj->hd.r.tag <= TAG_FUN) continue;
      for (int i = 0; i < obj->hd.r.size-1; ++i) {
        // if bottom bit of field is 1, it's delayed
        if (obj->fields[i] & 1) {
          uint32_t offset = obj->fields[i] >> 1;
          uint32_t index = *(uint32_t*)(code + offset);
          obj->fields[i] = (word)(static_data + index);
          dprintf("Delayed reloc: offset 0x%x -> index %d -> %p\n",
             offset, index, static_data+index);
        }
      }
    }
  }

  // dprintf("After reloc seg: end:%p p:%p\n", reloc_end, p);

  /* text */
  while (p < prog_end) {
    frn = fread(p, sizeof(uint32_t), 1, bcf);
    // dprintf("TEXT: read %x\n", *p);
    p++;
  }

  // dprintf("After text seg: end:%p p:%p\n", prog_end, p);

  // targetting my machine stuff: check that bytecode was honest
  ungetc(fgetc(bcf), bcf);
  //dprintf("{ %d %d %d } %ld\n", hdr->byte_len, hdr->reloc_seg_len, hdr->entry_off, ftell(bcf));
  assert(feof(bcf));
  fclose(bcf);
  (void)frn; // willfully ignore all results of fread
}

uint16_t read16(void) {
  uint16_t r = *C++;
  r |= (*C++) << 8;
  return r;
}

uint32_t read24(void) {
  uint32_t r;
  if ((word)C & 1) {
    r = *C++;
    r |= (*(uint16_t*)C) << 8;
    C += 2;
  } else {
    r = (*(uint16_t*)C);
    C += 2;
    r |= (*C++) << 16;
  }
  return r;
}

uint32_t read32(void) {
  uint32_t r;
  if (((word)C & 3) == 0) {
    r = *(uint32_t*)C;
    C += 4;
    return r;
  }
  if (((word)C & 1) == 0) {
    r = *(uint16_t*)C;
    C += 2;
    r |= (*(uint16_t*)C) << 16;
    C += 2;
    return r;
  }
  // completely unaligned
  r = *C++;
  r |= (*(uint16_t*)C) << 8;
  C += 2;
  r |= (*C++) << 24;
  return r;
}

static inline uint32_t read32_aligned(void) {
  assert(((word)C & 3) == 0);
  uint32_t r = *(uint32_t*)C;
  C += 4;
  return r;
}

/************** Curried-Semantics Helpers **************/
FUN *zonk(OBJ f) {
  assert(f->hd.r.tag == TAG_FUN || f->hd.r.tag == TAG_PAP);
  if (f->hd.r.tag == TAG_FUN) return (FUN*)f;

  PAP *pap = (PAP*)f;
  for (int i = pap->count - 1; i >= 0; --i) {
    PUSH((OBJ)pap->params[i]);
  }
  return zonk(pap->funlike);
}

byte curry_ctls[6] = {
  BC_CTL1, BC_CTL2, BC_CTL3, BC_CTL4, BC_CTL5, BC_CTL6
};

// This can almost certainly be inlined into the BC_LLCV cases in main.
void LL(word level, word n) {
  FUN *clo = E;
  while (level-- > 0) {
    clo = (FUN*)clo->captures[0];
  }
  PUSH((OBJ)clo->captures[n]);
}

#define HPALLOC(n)                           \
  do {                                       \
    word __HPALLOC_amt = (n);                \
    Hp += __HPALLOC_amt;                     \
    if (Hp >= HpLim) {                       \
      /* UNLIKELY */                         \
      HpAlloc = __HPALLOC_amt;               \
      gc();                                  \
    }                                        \
  } while(0)

int main(int argc, char *argv[]) {
  assert(sizeof(*Dp) == 3*sizeof(OBJ));
  if (argc != 2) {
    die("no bytecode file provided");
  }

  load_bytecode(argv[1]);
  print_internal_objs();

  enum BYTECODE bc;
  word arg1, arg2;
  OBJ operand1, operand2;
  FUN *F;

  static byte return_to_hlt = BC_HLT;
  Dp->S = Sp;
  Dp->E = NULL;
  Dp->C = &return_to_hlt;

  while ( (bc = *C++) != BC_HLT ) {
    switch (bc) {

case BC_NOP: break;
case BC_HLT: goto HLT; // should not occur, see above comparison

case BC_NIL:
  PUSH(0); break;

case BC_I0:
  PUSH((OBJ)&common_INTs[1]); break;

case BC_I1:
  PUSH((OBJ)&common_INTs[2]); break;

case BC_BT:
  PUSH((OBJ)&common_BOOLs[1]); break;

case BC_BF:
  PUSH((OBJ)&common_BOOLs[0]); break;

case BC_LDW:
  arg1 = (int16_t)read16(); // sign extension
  goto LDINT;
case BC_LDD:
  arg1 = (int32_t)read32(); // sign extension
  goto LDINT;
case BC_LDH:
  arg1 = (int8_t)*C++; // sign extension
LDINT:
  HPALLOC(2);
  Hp[-2] = (OBJ)HDR(TAG_INT, 2);
  Hp[-1] = (OBJ)arg1;
  PUSH((OBJ)(Hp-2));
  break;

case BC_CHR:
  arg1 = *C++; // zero ext'd
  HPALLOC(2);
  Hp[-2] = (OBJ)HDR(TAG_CHAR, 2);
  Hp[-1] = (OBJ)arg1;
  PUSH((OBJ)(Hp-2));
  break;

case BC_LDGD:
  arg1 = read24();
  goto LDG;
case BC_LDGW:
  arg1 = read16();
LDG:
  // In ETCa:
  // PUSH((OBJ)(code + arg1));
  // In x86:
  // We have to handle static_data indexing.
  //dprintf("LDG index: %d\n", *(uint32_t*)(code + arg1));
  PUSH(GET_STATIC(arg1));
  break;

case BC_LLCL:
  arg1 = (*C++); goto LLCL;
case BC_LLCL9:
case BC_LLCL8:
case BC_LLCL7:
case BC_LLCL6:
case BC_LLCL5:
case BC_LLCL4:
case BC_LLCL3:
case BC_LLCL2:
case BC_LLCL1:
case BC_LLCL0:
  arg1 = bc - BC_LLCL0;
LLCL:
  PUSH(LCL(arg1)); break;

case BC_SLCL:
  arg1 = (*C++); goto SLCL;
case BC_SLCL9:
case BC_SLCL8:
case BC_SLCL7:
case BC_SLCL6:
case BC_SLCL5:
case BC_SLCL4:
case BC_SLCL3:
case BC_SLCL2:
case BC_SLCL1:
case BC_SLCL0:
  arg1 = bc - BC_SLCL0;
SLCL:
  POP(operand1);
  LCL(arg1) = operand1;
  break;

case BC_LDE:
  PUSH((OBJ)E); break;

case BC_POP:
  Sp++; break;
case BC_DUP:
  operand1 = *Sp;
  PUSH(operand1); break;

case BC_TUCK:
  Sp[-1] = Sp[0];
  Sp[0]  = Sp[1];
  Sp[1]  = Sp[-1];
  --Sp;
  break;

case BC_TUCK2:
  Sp[-1] = Sp[0];
  Sp[0]  = Sp[1];
  Sp[1]  = Sp[2];
  Sp[2]  = Sp[-1];
  --Sp;
  break;

case BC_SWAP:
  operand1 = Sp[0];
  Sp[0] = Sp[1];
  Sp[1] = operand1;
  break;

case BC_OVER:
  operand1 = Sp[1];
  PUSH(operand1);
  break;

// in assembly we can probably also use a macro for this but it might be a good
// idea to at least share the allocation code?
#define BINOP_TYPED(typ, op) \
  do { \
    POP(operand2); \
    POP(operand1); \
    typ r = (typ)(operand1->fields[0]) op ((typ)operand2->fields[0]); \
    HPALLOC(2); \
    Hp[-2] = (OBJ)HDR(TAG_INT,2); \
    Hp[-1] = (OBJ)r; \
    PUSH((OBJ)(Hp-2)); \
  } while (0)
#define BINOP(op) BINOP_TYPED(intptr_t, op)

case BC_ADD:
  BINOP( + ); break;
case BC_SUB:
  BINOP( - ); break;
case BC_MUL:
  BINOP( * ); break;
case BC_DIV:
  BINOP( / ); break;
case BC_REM:
  BINOP( % ); break;
case BC_NEG:
  POP(operand1);
  HPALLOC(2);
  Hp[-2] = (OBJ)HDR(TAG_INT,2);
  Hp[-1] = (OBJ)(-operand1->fields[0]);
  PUSH((OBJ)(Hp-2));
  break;
case BC_SHL:
  BINOP( << ); break;
case BC_SHR:
  BINOP_TYPED(uintptr_t, >> ); break;
case BC_ASR:
  BINOP( >> ); break;
case BC_AND:
  BINOP( & ); break;
case BC_OR:
  BINOP( | ); break;
case BC_XOR:
  BINOP( ^ ); break;
case BC_INC:
  arg1 = (*C++);
  arg2 = (int8_t)(*C++);
  arg2 = LCL(arg1)->fields[0] + arg2;
  HPALLOC(2);
  Hp[-2] = (OBJ)HDR(TAG_INT,2);
  Hp[-1] = (OBJ)arg2;
  LCL(arg1) = (OBJ)(Hp-2);
  break;

case BC_TAG:
  arg1 = (*Sp)->hd.r.tag;
  HPALLOC(2);
  Hp[-2] = (OBJ)HDR(TAG_INT,2);
  Hp[-1] = (OBJ)arg1;
  PUSH((OBJ)(Hp-2));
  break;

case BC_I2B:
  arg1 = TAG_BOOL; goto STAG;
case BC_I2C:
  arg1 = TAG_CHAR; goto STAG;
case BC_B2I:
case BC_C2I:
  arg1 = TAG_INT; goto STAG;
case BC_STAG:
  arg1 = read16();
STAG:
  (*Sp)->hd.r.tag = arg1;
  break;

case BC_ALLOC:
  arg1 = read16();
  HPALLOC(arg1);
  Hp[-arg1] = (OBJ)HDR(0, arg1);
  PUSH((OBJ)(Hp-arg1));
  break;

case BC_MKCLO:
  arg1 = *C++ + 2; // size = n + 2
  HPALLOC(arg1);
  Hp[-arg1] = (OBJ)HDR(TAG_FUN, arg1);
  arg2  = read24() << 8;
  arg2 |= *C++;
  Hp[-arg1+1] = (OBJ)arg2;
  arg2 = arg1;
  arg1 -= 2;
  goto FINISHOBJ;
case BC_MKLCLO:
  arg1 = *C++ + 3; // size = n + 3
  HPALLOC(arg1);
  Hp[-arg1]   = (OBJ)HDR(TAG_FUN, arg1);
  Hp[-arg1+2] = (OBJ)E;
  arg2  = read24() << 8;
  arg2 |= *C++;
  Hp[arg1+1] = (OBJ)arg2;
  arg2 = arg1;
  arg1 -= 3;
  goto FINISHOBJ;
case BC_MKOBJ:
  arg1 = *C++ + 1; // size = n + 1;
  arg2 = read16(); // tag
  HPALLOC(arg1);
  Hp[-arg1] = (OBJ)HDR(arg2, arg1);
  arg2 = arg1;
  arg1 -= 1;
FINISHOBJ:
  memcpy(Hp - arg1, Sp, sizeof(word)*arg1);
  Sp += arg1;
  PUSH((OBJ)(Hp-arg2));
  break;

case BC_CLONE:
  POP(operand1);
  arg1 = operand1->hd.r.size;
  HPALLOC(arg1);
  memcpy(Hp - arg1, operand1, arg1*sizeof(word));
  PUSH((OBJ)(Hp-arg1));
  break;

case BC_UNPCK:
  POP(operand1);
  arg1 = operand1->hd.r.size - 1;
  Sp -= arg1;
  memcpy(Sp, operand1->fields, arg1*sizeof(word));
  break;

case BC_LFLD:
  arg1 = *C++;
  goto LFLD;
case BC_LFLDW:
  arg1 = read16();
  goto LFLD;
case BC_LFLD3:
case BC_LFLD2:
case BC_LFLD1:
case BC_LFLD0:
  arg1 = bc - BC_LFLD0;
LFLD:
  POP(operand1);
  // When targetting Grape1, it might be cleaner in ETCa to add 1 to
  // all of the numbers in the basic blocks that terminate here.
  // However it is unknown if Grape1 is even the target anymore.
  PUSH((OBJ)operand1->fields[arg1]);
  break;

case BC_SFLD:
  arg1 = *C++;
  goto SFLD;
case BC_SFLDW:
  arg1 = read16();
  goto SFLD;
case BC_SFLD3:
case BC_SFLD2:
case BC_SFLD1:
case BC_SFLD0:
  arg1 = bc - BC_SFLD0;
SFLD:
  POP(operand2);
  POP(operand1);
  // comment as for LFLD
  operand1->fields[arg1] = (word)operand2;
  break;

case BC_LDCV:
  arg1 = *C++;
  goto LDCV;
case BC_LDCV3:
case BC_LDCV2:
case BC_LDCV1:
case BC_LDCV0:
  arg1 = bc - BC_LDCV0;
LDCV:
  PUSH((OBJ)E->captures[arg1]);
  break;

case BC_LLCV:
  arg1 = *C++; // level
  arg2 = *C++; // n
  LL(arg1, arg2);
  break;
case BC_LLCV10:
  LL(1, 0);
  break;
case BC_LLCV11:
  LL(1, 1);
  break;
case BC_LLCV12:
  LL(1, 2);
  break;
case BC_LLCV13:
  LL(1, 3);
  break;
case BC_LLCV20:
  LL(2, 0);
  break;
case BC_LLCV21:
  LL(2, 1);
  break;
case BC_LLCV22:
  LL(2, 2);
  break;
case BC_LLCV23:
  LL(2, 3);
  break;

/* Branches */
// (do not use this macro in a braceless control construct)
#define BRANCH(cond)      \
  if (cond) {             \
    goto RELATIVE_BRANCH; \
  }                       \
  break
#define UNARY_BR(cond)    \
  arg1 = read16();        \
  POP(operand1);          \
  BRANCH(operand1 cond)
#define BINARY_BR(cond)   \
  arg1 = read16();        \
  POP(operand2);          \
  POP(operand1);          \
  BRANCH(cond)
#define BINARY_CMP(op)    \
  BINARY_BR(operand1->fields[0] op operand2->fields[0])

case BC_IFEQ:
  UNARY_BR(->fields[0] == 0);
case BC_IFNE:
  UNARY_BR(->fields[0]);
case BC_IFLT:
  UNARY_BR(->fields[0] < 0);
case BC_IFGT:
  UNARY_BR(->fields[0] > 0);
case BC_IFLE:
  UNARY_BR(->fields[0] <= 0);
case BC_IFGE:
  UNARY_BR(->fields[0] >= 0);
case BC_IFF:
  UNARY_BR(== (OBJ)&common_BOOLs[0]);
case BC_IFNF:
  UNARY_BR(!= (OBJ)&common_BOOLs[0]);
case BC_IFCEQ:
  BINARY_CMP( == );
case BC_IFCNE:
  BINARY_CMP( != );
case BC_IFCLT:
  BINARY_CMP( < );
case BC_IFCGT:
  BINARY_CMP( > );
case BC_IFCLE:
  BINARY_CMP( <= );
case BC_IFCGE:
  BINARY_CMP( >= );
case BC_IFSAME:
  BINARY_BR(operand1 == operand2);
case BC_IFDIFF:
  BINARY_BR(operand1 != operand2);

case BC_IFNIL:
  UNARY_BR(== NULL);
case BC_IFNNIL:
  UNARY_BR(!= NULL);
case BC_IFFALSY:
  UNARY_BR(== NULL ||
    (operand1->hd.r.size > 1 && operand1->fields[0] == 0));
case BC_IFTRUTHY:
  UNARY_BR(!= NULL &&
    (operand1->hd.r.size == 0 || operand1->fields[0] != 0));
RELATIVE_BRANCH:
  C = C - 3 + arg1;
  break;

#undef BINARY_CMP
#undef BINARY_BR
#undef UNARY_BR
#undef BRANCH

case BC_GOTO:
  arg1 = read24();
  C = code + arg1;
  break;

case BC_MATCH:
  C = (byte*)(((uintptr_t)C + 5) & ~3); // skip 2 bytes N, then align up to 4
  if ((*Sp)->hd.r.tag <= TAG_CHAR) { // in [TAG_INT, TAG_BOOL, TAG_CHAR]
    arg1 = (*Sp)->fields[0];
  } else {
    arg1 = (*Sp)->hd.r.tag - 16;
  }
  // block to localize jump_table
  {
    uint32_t *jump_table = (uint32_t*)C;
    C = code + jump_table[arg1];
  }
  break;
case BC_MATCHD:
  C = (byte*)(((uintptr_t)C + 3) & ~3); // align up to 4
  arg1 = read16(); // lo
  if ((*Sp)->hd.r.tag <= TAG_CHAR) {
    arg2 = (*Sp)->fields[0];
  } else {
    arg2 = (*Sp)->hd.r.tag;
  }
  // test in this order so that read16's side effects definitely occur
  if (arg1 > read16() || arg1 < arg2) {
    C = code + read32_aligned(); // that's DEF
  } else {
    // otherwise arg2-arg1 is the table index
    uint32_t *jump_table = (uint32_t*)(C+4); // skip DEF
    C = code + jump_table[arg2-arg1];
  }
  break;

case BC_CHKTAG:
  arg1 = read16();
  POP(operand1);
  if (operand1->hd.r.tag != arg1) {
    printf("Type error: expected tag %d, got tag %d\n",
      (tag_t)arg1, operand1->hd.r.tag);
    die("");
  }
  break;

case BC_IFTEQ:
  arg1 = *C++;
  arg2 = read16();
  if (arg1 == (*Sp)->hd.r.tag) {
    C = C - 4 + arg2;
  }
  break;
case BC_IFTLT:
  arg1 = *C++;
  arg2 = read16();
  if (arg1 > (*Sp)->hd.r.tag) {
    C = C - 4 + arg2;
  }
  break;

#define MAKE_FRAME(arity)  \
  do {                     \
    Dp++;                  \
    Dp->E = E;             \
    Dp->C = C;             \
    Dp->S = Sp + (arity);  \
    Lcl = Dp->S - 1;       \
  } while(0)
#define STACK_SLIDE(arity)        \
  do {                            \
    OBJ *where = Dp->S - arity;   \
    if (where != Sp) {            \
      memmove(where, Sp, arity * sizeof(word)); \
      Sp = where;                 \
    }                             \
  } while (0)

case BC_FASTAP:
  arg1 = read24();
  F = (FUN*)GET_STATIC(arg1);
APF:
  MAKE_FRAME(F->arity);
  /* Enact call */
  E = F; // obvious opportunity for good register allocation above
  C = code + E->code;
  break;

case BC_FASTTL:
  arg1 = read24();
  E = (FUN*)GET_STATIC(arg1);
TLE:
  C = code + E->code;
  STACK_SLIDE(E->arity);
  break;

  // would have to measure which arity is most common but I imagine it's 2
case BC_EAP:
  arg1 = *C++; goto EAP;
case BC_EAP0:
case BC_EAP1:
case BC_EAP2:
case BC_EAP3:
case BC_EAP4:
case BC_EAP5:
  arg1 = bc - BC_EAP0;
EAP:
  POP(operand1);
  F = (FUN*)operand1;
  if (F->arity != arg1) {
    printf("Function arity mismatch: expected ");
    printf("%ld", arg1);
    printf(" but got ");
    printf("%d", F->arity);
    die("");
  }
  goto APF;

case BC_ETL:
  arg1 = *C++; goto ETL;
case BC_ETL0:
case BC_ETL1:
case BC_ETL2:
case BC_ETL3:
case BC_ETL4:
case BC_ETL5:
  arg1 = bc - BC_ETL0;
ETL:
  POP(operand1);
  E = (FUN*)operand1;
  if (E->arity != arg1) {
    printf("Function arity mismatch: expected ");
    printf("%ld", arg1);
    printf(" but got ");
    printf("%d", E->arity);
    die("");
  }
  goto TLE;
 
case BC_UAP:
  POP(operand1);
  F = (FUN*)operand1;
  goto APF;
case BC_UTL:
  POP(operand1);
  E = (FUN*)operand1;
  goto TLE;

case BC_RET:
  operand1 = *Sp;
RET: // Return the value in operand1
  Sp = Lcl; // pinned copy of Dp->S - 1
  *Sp = operand1;
  E = Dp->E;
  C = Dp->C;
  Dp--;
  Lcl = Dp->S - 1;
  break;

case BC_SYS:
  arg1 = *C++;
  if (arg1 == 0) {
    print_machine_state();
  } else if (arg1 == 1) {
    gc();
  }
  break;

case BC_CAP1:
case BC_CAP2:
case BC_CAP3:
case BC_CAP4:
case BC_CAP5:
case BC_CAP6:
  arg1 = bc - (BC_CAP1-1); // P
  POP(operand1);
  // FUN::arity and PAP::arity line up, so this will work either way.
  arg2 = ((FUN*)operand1)->arity;
  //dprintf("CAP: P=%ld A=%ld\n", arg1, arg2);
  // compare number of values being applied to arity of funlike
  if (arg2 > arg1) {
    // more arguments required than supplied; build a new PAP.
    byte difference = arg2 - arg1;
    word size = sizeof(PAP)/sizeof(word) + arg1;
    HPALLOC(size);
    PAP *pap = (PAP*)(Hp-size);
    *(word*)pap = HDR(TAG_PAP, size);
    pap->arity = difference;
    pap->count = arg1;
    pap->funlike = operand1;
    memcpy(pap->params, Sp, arg1*sizeof(OBJ));
    Sp += arg1;
    PUSH((OBJ)pap);
    break;
  }
  // Otherwise, zonk the thing on top of the stack...
  F = zonk(operand1);
  if (arg2 == arg1) {
    // If provided args exactly matches desired args, enter as if by UAP.
    goto APF;
  }
  assert(arg2 < arg1);
  // more arguments supplied than required, time to do black magic.
  // Least possible required is 0 (well, 1 in practice) and most possible
  // supplied is 6. So difference cannot be more than 6. The difference
  // also cannot be 0, since we would have hit the previous case.
  MAKE_FRAME(arg1);
  // set code pointer to the correct internal CTL bytecode...
  C = &curry_ctls[arg1 - arg2 - 1];
  // Then act as UAP applying F, returning to C.
  goto APF;

case BC_CTL1:
case BC_CTL2:
case BC_CTL3:
case BC_CTL4:
case BC_CTL5:
case BC_CTL6:
  arg1 = bc - (BC_CTL1-1); // P
  POP(operand1);
  // FUN::arity and PAP::arity line up, so this will work either way.
  arg2 = ((FUN*)operand1)->arity;
  //dprintf("CTL: P=%ld A=%ld\n", arg1, arg2);
  // compare number of values being applied to arity of funlike
  if (arg2 > arg1) {
    // more arguments required than supplied; build a new PAP.
    byte difference = arg2 - arg1;
    word size = sizeof(PAP)/sizeof(word) + arg1;
    HPALLOC(size);
    PAP *pap = (PAP*)(Hp-size);
    *(word*)pap = HDR(TAG_PAP, size);
    pap->arity = difference;
    pap->count = arg1;
    pap->funlike = operand1;
    memcpy(pap->params, Sp, arg1*sizeof(OBJ));
    Sp += arg1;
    operand1 = (OBJ)pap;
    goto RET; // returns the value in operand1
  }
  // Otherwise, zonk the thing on top of the stack...
  F = zonk(operand1);
  if (arg2 == arg1) {
    // If provided args exactly matches desired args, enter as if by UTL.
    E = F;
    goto TLE;
  }
  assert(arg2 < arg1);
  // more arguments supplied than required, time to do black magic.
  // Most possible required is 0 (well, 1 in practice) and most possible
  // supplied is 6. So difference cannot be more than 6. The difference
  // also cannot be 0, since we would have hit the previous case.

  // The CTL after UAP would move the stack anyway so I'm not convinced
  // that this slide is actually required, but since we do the slide
  // here it won't try and slide it again there. So that's fine.
  STACK_SLIDE(arg1);
  // Set code pointer to the correct internal CTL bytecode...
  C = &curry_ctls[arg1 - arg2 - 1];
  // then act as UAP of F returning to C.
  goto APF;

    }
  }
HLT:

  print_machine_state();
}

/*
Next test program:

type 'a option = None | Some of 'a;;
Some (fun () -> Some ());; (* nil in closure and code builds from LDCV *)

header: 30 0 12
12: NIL
13: MKCLO 1 3'24 1
19: MKOBJ 1 2'16
23: HLT
24: LDCV0
25: MKOBJ 1 2'16
29: RET

*/