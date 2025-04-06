;;; ELF Header manipulation
;;; Definitions for structs and macros for manipulating
;;; their fields, for storing information read from
;;; an ELF header or an ELF program header.

; struct ELF_header_data {
;     // pointer to the complete ELF file data.
;     // This isn't checked; it's just the pointer we extracted
;     // the rest of this data from.
;   char *raw_data;       0
;   code *entry;          4
;     // phoff and shoff store actual
;     // pointers to the program and section headers,
;     // not their offsets in the file data.
;   struct phdr *ph;      8
;   struct shdr *sh;      12
;   short phentsz;        16
;   short phnum;          18
;   short shentsz;        20
;   short shnum;          22
; }

.set __EHD_raw_data, 0
.set __EHD_entry, 4
.set __EHD_ph, 8
.set __EHD_sh, 12
.set __EHD_phentsz, 16
.set __EHD_phnum, 18
.set __EHD_shentsz, 20
.set __EHD_shnum, 22
.set sizeof_EHD, 24

    ; ldehd dst ptr X ==> dst = ptr->X
    ; You are required to know the size of the field
    ; and provide the appropriate size for dst.
    ; Otherwise wierd behavior will result.
    ; I can't find a way to automatically set the size
    ; depending on the value of field that isn't completely awful.
    .macro  ldehd dst ptr field
    mov   \dst , [\ptr + __EHD_\field]
    .endm

    ; stehd src ptr X ==> ptr->X = src
    ; see ldehd.
    .macro  stehd src ptr field
    mov   [\ptr + __EHD_\field], \src
    .endm


; struct phdr {
;   int type;     0
;   int offset;   4
;   int vaddr;    8
;   int rsvd;     12
;   int filesz;   16
;   int memsz;    20
;   int flags;    24
;   int align;    28  // validate that this is at least 8 and vaddr is 0 mod 8
; } 32

.set __PHD_type,   0
.set __PHD_offset, 4
.set __PHD_vaddr,  8
.set __PHD_filesz, 16
.set __PHD_memsz,  20
.set __PHD_flags,  24
.set __PHD_align,  28
.set sizeof_PHD, 32

    ; ldphd dst ptr X ==> dst = ptr->X
    ; see ldehd.
    .macro  ldphd dst ptr field
    mov   \dst , [\ptr + __PHD_\field]
    .endm

    ; stphd src ptr X ==> ptr->X = src
    ; see ldehd.
    .macro  stphd src ptr field
    mov   [\ptr + __PHD_\field], \src
    .endm
