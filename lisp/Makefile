files := $(wildcard *.lisp)
names := $(files:.lisp=)

.PHONY: all clean $(names)

all: $(names)

$(names): %: bin/% man/man1/%.1

bin/%: %.lisp build-bins.sh Makefile
	mkdir -p bin
	./build-bins.sh $<
	mv $(@F) bin/

man/man1/%.1: %.lisp build-man.sh Makefile
	mkdir -p man/man1
	./build-man.sh $<
	mv $(@F) man/man1/

clean:
	rm -rf bin man
