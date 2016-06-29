Overview:
=========
Sk-sampler executes in parallel the following experiment. Take a random SK
combinator of size n, normalize it up to r (standard) reduction steps. If its
normal form was reached, print the number of performed reduction steps.
Otherwise, print -1. The number of samples can be controlled using the s
input parameter. The goal of this experiment is to "approximate" the ratio of
normalizing SK combinators of size n and the number of required reductions in
order to compute the normal forms. 

The data/ folder contains several data sets obtained from super-computer experiments
performed on the Prometheus cluster in ACC Cyfronet AGH in Kraków. 
See naming conventions for more details.

The scripts/ folder contains ruby scripts useful in organizing and managing the
program output data sets.

Building:
=========
1. stack build

Usage:
======
./sk-sampler -?

Naming convention:
==================
merge-s-r-n.out contains the total result of s uniformly random samples of size
n and their required reduction steps using r as upper bound.
