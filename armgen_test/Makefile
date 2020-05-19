.PHONY: out dump clean

all: out dump

out: fuck.txt
	python3 unfuck.py

dump: out
	for i in *.out; do aarch64-linux-gnu-objdump -EL -D -b binary -maarch64 $$i > "$$(basename $$i .out)".dump; done

clean:
	rm -f fuck_*.out fuck_*.dump
