# MES-LshapedDomain
Project for Differential Equations Course at AGH UST.
Task: http://home.agh.edu.pl/~paszynsk/RRIR/Lshapedomain.pdf
Project also for Functional Programming Course at AGH UST.



# Installation
Install gnuplot first, you can download it from http://www.gnuplot.info/

stack build

stack install

# Running

stack exec LShape-exe n | gnuplot -p

where n is the number of divisions

# Generating documentation

stack haddock

# Running tests

stack test
