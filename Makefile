.PHONY: build clean run solve

build:
	dune build

run:
	./run

clean:
	dune clean

task/lambdaman% task/spaceship%:
	if ./run get $(shell basename $@) > $@.tmp; then mv $@.tmp $@; else rm $@.tmp; fi

tasks: $(foreach n,$(shell seq 1 21),task/lambdaman$n) $(foreach n,$(shell seq 1 25),task/spaceship$n)
	./run get lambdaman > task/lambdaman
	./run get spaceship > task/spaceship

solve_lambdaman:
	for n in `seq 1 21`; do ./run solve task/lambdaman$$n; done

solve_spaceship:
	for n in `seq 1 25`; do ./run solve task/spaceship$$n; done
