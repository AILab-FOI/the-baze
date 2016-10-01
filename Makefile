GBC=gbc3
GBA=gba3

CFLAGS=-a
AFLAGS=-o baze

all: compile link

compile:
	$(GBC) $(CFLAGS)

link:
	$(GBA) $(AFLAGS)

.PHONY: clean

clean:
	rm -f baze scriptapi/*.pyc

install:
	cp baze /usr/bin

uninstall:
	rm -f /usr/bin/baze
