# SimFin API wrapper for Haskell

[![Build Status](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2F414owen%2Fsimfin%2Fbadge&style=flat)](https://actions-badge.atrox.dev/414owen/simfin/goto)

See [simfin.com](https://simfin.com/)

Pre-alpha, expect frequent breaking changes.

## Examples

These charts were produced using code in the `examples` repository:

![prices](https://user-images.githubusercontent.com/1714287/169410772-021b0d73-1b75-448a-b11f-2f53ae3f78bd.svg)
![relative-eps](https://user-images.githubusercontent.com/1714287/169545083-7c1fac59-9e82-4342-9d83-423317d3de69.svg)

## TODO:

* [ ] Move to `Web` namespace
* [x] ~Handle upgrade prompt answers~
* [ ] Add shares outstanding call
* [ ] ~Differentiate between ticker/id missing vs. time period missing~  
  Won't do, you can figure it out form the response anyway.
* [ ] Expose currency in Price data
* [x] ~Move tests to test suite~
  * [x] ~Requires making an Internal module?~
* [x] ~Add CI~
* [x] Add examples (with charts?)
* [ ] Add documentation
* [ ] Publish to Hackage

## Might Do

* [x] ~Specialize Standard / SimFin+ data structures and results~
* [ ] Don't export all constructors, but let people import them individually, so they don't have to use `DuplicateRecordFields`?
