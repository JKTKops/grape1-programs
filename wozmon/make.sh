# Script arguments:
# $1: Which system to build for. See the README.
# rest: extra arguments directly for assembler
# Can invoke with no arguments if everything would be ""

if [ -z "$1" ]; then
  echo "No system provided!"
  exit 1
fi

SYSTEM="$1"
CFG="$SYSTEM/$SYSTEM.cfg"
LDS="$SYSTEM/$SYSTEM.lds"
EXTS="$SYSTEM/exts"
SYSECHO="$SYSTEM/echo.s"
shift

if [ -f "$LDS" ]; then
  LDSCR="-T $LDS"
fi

# This build script does not use MegaIng's wrapper in order to
# reduce the number of tools that need to be installed.
EXTENSIONS="SAF,BYTE"
if [ -f "$EXTS" ]; then
  EXTRAS=$(head -n 1 "$EXTS")
  EXTENSIONS="$EXTENSIONS,$EXTRAS"
fi
ARCH_ARG="-march=base+$EXTENSIONS"
AS="etca-elf-as $ARCH_ARG"

# Create copy of wozmon.s with the appropriate .include for
# the selected system...
tmp=$(mktemp)
trap "rm -f $tmp" 0 2 3 15

echo "  .include \"$CFG\"" >> $tmp
echo "  .file \"wozmon.s\"" >> $tmp
echo "  .line 1" >> $tmp
sed "s,\\\"echo.s\\\",\\\"$SYSECHO\\\"," wozmon.s >> $tmp

# cp $tmp tmp.s

$AS $tmp -o wozmon.o "$@"
etca-elf-ld $LDSCR wozmon.o -o wozmon
etca-elf-objcopy -O binary wozmon wozmon.bin
