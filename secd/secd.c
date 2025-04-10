#include <assert.h> // sanity checks
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h> // exit
#include <string.h> // memcpy

#include <sys/types.h>

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
  word code:24;
  word arity:8;
  word captures[0];
} FUN;

/* Format of a PAP. */
typedef struct PAP {
  OBJ_HDR hdr;
  HPOBJ *funlike; // either a FUN or another PAP or NULL.
  word params[0];
} PAP;

typedef HPOBJ* OBJ; // NULL is the value nil

struct SEMISPACE {
  HPOBJ area[NUM_SEMISPACE_OBJS];
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

// Global heap (nongenerational) and stack.
HEAP heap = { 0 };
OBJ stack[NUM_STACK_OBJS];
// Pin these to registers.
  // Hp points to the first unused word.
OBJ  Hp    = heap.half[0].area;
  // HpLim points to the last USABLE word.
  // gc when Hp would exceed HpLim.
OBJ  HpLim = heap.half[0].area + NUM_SEMISPACE_OBJS;
  // Sp points to the top word on the stack, which is actually at
  // the lowest address. Initially nothing is on the stack, so it
  // points past the end of &stack.
OBJ *Sp    = stack + NUM_STACK_OBJS;
  // Dp points to the first unused frame on the dump.
  // When Sp == Dp, the stack is exactly full (this is not checked).
dump_frame_t *Dp = (void*)stack;
// We might also want to pin a copy of the S in the top dump frame
// to a register as this is the "locals" pointer.
// OBJ *Lcl = Sp;
  // The currently-executing function.
FUN *E  = NULL;
  // Pointer to current byte.
byte *C = code;

void die(char *msg) {
  fputs(msg, stdout);
  exit(1);
}

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

void gc() {
  heap.current_semispace = 1 - heap.current_semispace;
  Hp = heap.half[heap.current_semispace].area;
  OBJ Work = Hp; // points to first object in to-space not yet scavenged
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
  while (Work < Hp) {
    gc_scavenge(Work);
    Work += (Work)->hd.r.size;
  }
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
  if (Hp + size > HpLim) die("out of memory");
  newptr = Hp;
  memcpy(Hp, obj, size*sizeof(OBJ));
update:
  obj->hd.f = newptr;
  return newptr;
}

// Given a pointer to something in to-space, evacuate each of its fields
// and update the fields to point to their new locations.
// (This function can probably be inlined into gc().)
void gc_scavenge(OBJ obj) {
  // TAKE CARE! INT, BOOL, CHAR, STRING, and FUN all have non-OBJ
  // data in their first (and possibly only) field. Skip the first
  // field for those objects.
  tag_t tag = obj->hd.r.tag;
  uint16_t size = obj->hd.r.size;
  uint16_t work = 1;
  // faster check for this is tag <= TAG_FUN
  if (tag == TAG_INT || tag == TAG_BOOL || tag == TAG_CHAR
      || tag == TAG_STRING || tag == TAG_FUN) {
    work = 2;
  }

  for (; work < size; ++work) {
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
      printf("FUN(%x,%d", fun->code, fun->arity);
      field = 1;
      break;
    case TAG_PAP:
      pap = (PAP*)obj;
      printf("PAP(%p", pap->funlike);
      field = 1;
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
}

void print_heap() {
  printf("**************** HEAP ****************\n");
  printf("Current semispace: %d\n", heap.current_semispace);
  for (OBJ W = heap.half[heap.current_semispace].area; W < Hp; W += W->hd.r.size) {
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
  for (dump_frame_t *F = (void*)stack; (void*)F < (void*)Dp; ++i, ++F) {
    printf("%d: (%ld, 0x%lx, ", i, stack - F->S, F->C - code);
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
  BC_LDGW = 0x09,
  BC_LDGD = 0x0A,
  BC_LLCL = 0x0B,
  BC_SLCL = 0x0C,
  BC_LDE = 0x0D,
  BC_POP = 0x0E,
  BC_DUP = 0x0F,
  BC_TUCK = 0x10,
  BC_TUCK2 = 0x11,
  BC_SWAP = 0x12,
  BC_OVER = 0x13,
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
  BC_IF_F = 0x46,
  BC_IF_NF = 0x47,
  BC_IFCEQ = 0x48,
  BC_IFCNE = 0x49,
  BC_IFCLT = 0x4A,
  BC_IFCGT = 0x4B,
  BC_IFCLE = 0x4C,
  BC_IFCGE = 0x4D,
  BC_IFSAME = 0x4E,
  BC_IFDIFF = 0x4F,
  BC_IF_NIL  = 0x50,
  BC_IF_NNIL = 0x51,
  BC_IF_FALSY  = 0x52,
  BC_IF_TRUTHY = 0x53,
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

  /* nop */
  BC_NOP = 0xFF,
};

void load_bytecode(char *path) {
  FILE *bcf = fopen(path, "rb");
  bc_header_t *hdr = (void*)code;
  // read the header into code
  fread(code, sizeof(bc_header_t), 1, bcf);
  C = code + hdr->entry_off;

  word *p = (word*)(code + 12);
  word *reloc_end = p + hdr->reloc_seg_len;
  word *prog_end  = (word*)(code + hdr->byte_len);

  /* relocatable data */
  while (p < reloc_end) {
    // read objhdr of next object
    fread(p, sizeof(OBJ_HDR), 1, bcf);
    OBJ obj = (OBJ)p;
    uint16_t size = obj->hd.r.size;
    // handle static strings specially
    if (obj->hd.r.tag == TAG_STRING) {
      fread(&obj->fields[0], sizeof(uint32_t), size, bcf);
      p += size;
      continue;
    }
    // now we have to deal with a difference between compiling for my x86
    // computer and for ETCa. The 4-byte fields in the bytecode program
    // are perfect for ETCa, but do not match the sizeof(word) on my x86
    // computer. So we have to read them one at a time and handle them.
    // In ETCa we can do the same thing but without the shuffling.
    for (int i = 0; i < obj->hd.r.size-1; ++i) {
      // read 4 bytes from the bytecode file 
      fread(&obj->fields[i], sizeof(uint32_t), 1, bcf);
      // If this is the first iteration and the object is
      // immediate or FUN, its first field is not an object and must not
      // be relocated.
      printf("Read %lx\n", obj->fields[i]);
      if (i == 0 && obj->hd.r.tag <= TAG_FUN) continue;
      // extract that 4 byte value so we can determine how to reloc.
      uint32_t relocation = (uint32_t)obj->fields[i];
      printf("Relocating %x ... ", relocation);
      if (relocation == 0) {
        printf("nil\n");
        continue; // don't relocate nil
      }
      if (relocation - 0xFF000000 <= 1) {
        // relocate offset to common boolean
        obj->fields[i] = (word)&common_BOOLs[relocation-0xFF000000];
        printf("%p\n", &common_BOOLs[relocation-0xFF000000]);
      } else if (relocation - 0xFF000002 <= 10) {
        // relocate offset to common int
        obj->fields[i] = (word)&common_INTs[relocation-0xFF000002];
        printf("%p\n", &common_INTs[relocation-0xFF000002]);
      } else if (relocation >= 0x20000000) {
        die("malformed relocation");
      } else {
        // relocate to bytecode offset
        obj->fields[i] = (word)(code + relocation);
        printf("%p\n", code + relocation);
      }
    }
    p += size;
  }

  /* text */
  while (p < prog_end) {
    fread(p, sizeof(word), 1, bcf);
    p++;
  }

  assert(feof(bcf));
  fclose(bcf);
}

int main(int argc, char *argv[]) {
  assert(sizeof(*Dp) == 3*sizeof(OBJ));
  if (argc != 2) {
    die("no bytecode file provided");
  }

  print_internal_objs();
  print_machine_state();

  load_bytecode(argv[1]);

  while (*C != BC_HLT) {
    switch (*C) {

case BC_NOP:
  ++C; break;

case 



  }
}
