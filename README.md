# SimFin API wrapper for Haskell

[![Build Status](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2F414owen%2Fsimfin%2Fbadge&style=flat)](https://actions-badge.atrox.dev/414owen/simfin/goto)

See [simfin.com](https://simfin.com/)

Pre-alpha, expect frequent breaking changes.

## Examples

These charts were produced using code in the `examples` repository:  

![prices](https://user-images.githubusercontent.com/1714287/169568674-1bf54e8f-5602-4ee8-8e00-6e88c1a7b17f.svg)
![eps-diluted](https://user-images.githubusercontent.com/1714287/169568597-937a1c85-d1e2-4c3f-8667-64daf54c24c0.svg)
![relative-performance](https://user-images.githubusercontent.com/1714287/169568604-ca15c7be-1053-436c-a106-0ed2d62f9b8c.svg)

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
