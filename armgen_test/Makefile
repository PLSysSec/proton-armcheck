.PHONY: out dump clean

CXXFLAGS := -m64 -pedantic -pedantic-errors -std=c++17 -Werror -Wall -Wextra -Wshadow -Wpointer-arith -Wcast-qual -Wformat=2 -Weffc++

all: out dump armgen_test

out: fuck.txt
	python3 unfuck.py

dump: out
	for i in *.out; do aarch64-linux-gnu-objdump -EL -D -b binary -maarch64 $$i > "$$(basename $$i .out)".dump; done

armgen_64.o: armgen_64.cpp

armgen_test: armgen_test.cpp armgen_64.o
	$(CXX) $(CXXFLAGS) -o $@ $^

clean:
	rm -f fuck_*.out fuck_*.dump armgen_64.o
