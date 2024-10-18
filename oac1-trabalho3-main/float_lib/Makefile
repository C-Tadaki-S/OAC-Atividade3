.PHONY : all
all : run

float_lib.o: float_lib.c float_lib.h
	gcc -c float_lib.c

test: float_lib.o test.c
	gcc -o test float_lib.o test.c -lm

run: test
	./test

clean:
	rm -f *.o test



