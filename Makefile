.PHONY: build clean run solve index tasks

build:
	dune build

run:
	./run

clean:
	dune clean

task/lambdaman% task/spaceship% task/3d%:
	if ./run get $(shell basename $@) > $@.tmp; then mv $@.tmp $@; else rm $@.tmp; fi

task/efficiency%:
	if ./run raw get $(shell basename $@) > $@.tmp; then mv $@.tmp $@; else rm $@.tmp; fi

tasks: $(foreach n,$(shell seq 1 21),task/lambdaman$n) $(foreach n,$(shell seq 1 25),task/spaceship$n) $(foreach n,$(shell seq 1 12),task/3d$n) $(foreach n,$(shell seq 1 13),task/efficiency$n) index

index:
	./run get index > task/index
	./run get scoreboard > task/scoreboard
	./run get lambdaman > task/lambdaman
	./run get spaceship > task/spaceship
	./run get 3d > task/3d
	./run get efficiency > task/efficiency

solve_lambdaman:
	for n in `seq 1 21`; do ./run solve task/lambdaman$$n; done

solve_spaceship:
	for n in `seq 1 25`; do ./run solve task/spaceship$$n; done

solve_hello:
	./run get language_test
	./run send solve language_test 4w3s0m3

solve: solve_lambdaman solve_spaceship solve_hello
