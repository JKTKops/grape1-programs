
EXTENSIONS = INT,PM,MO1,MO2,FI,SAF,BYTE,DW,QW,DWAS,REX,ASP,EXOP
ARCH_ARG = -march=base+$(EXTENSIONS)
MODEL = -mcmodel=medany -mpw=d

AS = etca-elf-as $(ARCH_ARG) $(MODEL)

disk_test: disk.o start.o disk_test_main.o
	etca-elf-ld -T linker_script.lds start.o disk.o disk_test_main.o -o disk_test
	etca-elf-objcopy -O binary disk_test disk_test.bin
	cp disk_test.bin /mnt/c/Users/zergl/AppData/Roaming/godot/app_userdata/Turing\ Complete/schematics/architecture/ETCA/uETCa-v1/

disk.o:	disk.s
	$(AS) disk.s -o disk.o

start.o: start_for_testing.s
	$(AS) start_for_testing.s -o start.o

disk_test_main.o: disk_test_main.s
	$(AS) disk_test_main.s -o disk_test_main.o
