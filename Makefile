AS = ca65
CC = cc65
LD = ld65

.PHONY: clean
build: main.fds

integritycheck: main.fds
	radiff2 -x main.fds original.fds | head -n 100

%.o: %.asm
	$(AS) -DANN --listing "$@.lst" --create-dep "$@.dep" -g --debug-info $< -o $@

main.fds: layout entry.o
	$(LD) --dbgfile main.dbg -C layout entry.o -o $@

clean:
	rm -f main*.fds *.o *.o.bin

include $(wildcard ./*.dep)
