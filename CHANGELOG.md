# Changelog

`ising-ground-state` uses [PVP Versioning][1].

## 0.1.1.0

* Haskell exposes energy history instead of just the lowest energy. Python
  supports this as well.
* Seed is chosen automatically when not provided.
* Python wrapper now accepts initial configuration from which to start
  annealing.

## 0.1.0.1

* Fixed a bug with lifetimes (`exchange` and `field` matrices were deallocated
  on Python side too early).

## 0.1.0.0

* First working prototype.

## 0.0.0.0

* Initially created.

[1]: https://pvp.haskell.org
