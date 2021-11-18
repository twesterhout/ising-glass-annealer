# Ising Glass Annealer

This package allows you to find ground states of classical Ising glasses using
Simulated Annealing algorithm.

It exists for a few reasons:

1) I failed to find a suitable open-source package Simulated Annealing. There
   are a lot of packages, but most of them focus on either a specific geometry
   (such as a hypercube or chimera graph or bipartite graphs) or do not allow
   arbitrary couplings. We use techniques from [Isakov et al "Optimised
   simulated annealing for Ising spin
   glasses"](https://doi.org/10.1016/j.cpc.2015.02.015) to make the library fast
   with no loss of generality.

2) As a proof of concept to show that it's possible to write low-level Haskell
   loops which run fast and don't allocate memory.

3) As a proof of concept to show how one can wrap Haskell code into a Python
   package and use Conda to distribute it. I.e. you write code in your favorite
   programming language and your users stick to Python and don't have
   to learn Haskell :stuck_out_tongue:


## Installing

```shell
conda install -c twesterhout -c conda-forge ising-glass-annealer
```


## Technical details, i.e. how it all works

To be written.


## Contributing

To be written.
