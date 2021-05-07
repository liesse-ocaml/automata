# automata

Simple code for basic exercises on the notion of automaton.

The files automaton_v{1,2,3} contain the progressive implementation of an abstract notion
of automaton.

It starts with a very simple example:

alphabet is ['a';'b'] and 1 is initial, 2 is final and transitions 1 --a--> 2, 2 --b--> 1

parsing_polynomial cover the exercise on parsing simple polynomials

the version done live during the session is parsing_polynomial_live, the .txt file
contains the utop

To build and execute a file: `dune exec ./`*name*`.exe`
