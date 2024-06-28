.PHONY: build clean run solve

build:
	dune build

run:
	./run

clean:
	dune clean

task/lambdaman% task/spaceship% task/3d%:
	if ./run get $(shell basename $@) > $@.tmp; then mv $@.tmp $@; else rm $@.tmp; fi

tasks: $(foreach n,$(shell seq 1 21),task/lambdaman$n) $(foreach n,$(shell seq 1 25),task/spaceship$n) $(foreach n,$(shell seq 1 12),task/3d$n)
	./run get lambdaman > task/lambdaman
	./run get spaceship > task/spaceship
	./run get 3d > task/3d

solve_lambdaman:
	for n in `seq 1 21`; do ./run solve task/lambdaman$$n; done

solve_spaceship:
	for n in `seq 1 25`; do ./run solve task/spaceship$$n; done

solve: solve_lambdaman solve_spaceship
