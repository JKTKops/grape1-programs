
# This build script does not use MegaIng's wrapper to reduce
# the number of tools that need to be installed.
EXTENSIONS="SAF,BYTE"
ARCH_ARG="-march=base+$EXTENSIONS"
AS="etca-elf-as $ARCH_ARG"

# If you require a linker script, specify its filepath here.
LDSCR=

$AS wozmon.s -o wozmon.o "$@"
if [ -z "$LDSCR" ]; then
  etca-elf-ld wozmon.o -o wozmon
else
  etca-elf-ld -T "$LDSCR" wozmon.o -o wozmon
fi
etca-elf-objcopy -O binary wozmon wozmon.bin
