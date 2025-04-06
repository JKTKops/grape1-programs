
EXTENSIONS = INT,PM,MO1,MO2,FI,SAF,BYTE,DW,QW,DWAS,REX,ASP,EXOP
ARCH_ARG = -march=base+$(EXTENSIONS)
MODEL = -mcmodel=medany -mpw=d

AS = etca-elf-as $(ARCH_ARG) $(MODEL)

shim: shim.o puts.o loader.o
	etca-elf-ld -T linker_script.lds shim.o puts.o loader.o -o shim
	etca-elf-objcopy -O binary shim shim.bin
	cp shim.bin /mnt/c/Users/zergl/AppData/Roaming/godot/app_userdata/Turing\ Complete/schematics/architecture/ETCA/uETCa-v1/

puts.o:	puts.s
	$(AS) puts.s -o puts.o

loader.o: loader.s
	$(AS) loader.s -o loader.o

shim.o: shim.s
	$(AS) shim.s -o shim.o

clean:
	rm *.o shim
