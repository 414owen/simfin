# SimFin API wrapper for Haskell

[![Build Status](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2F414owen%2Fsimfin%2Fbadge&style=flat)](https://actions-badge.atrox.dev/414owen/simfin/goto)

See [simfin.com](https://simfin.com/)

Pre-alpha, expect frequent breaking changes.

No SimFin+ features have been tested at all yet.

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
* [ ] Add examples (with charts?)
* [ ] Add documentation
* [ ] Publish to Hackage

## Might Do

* [x] ~Specialize Standard / SimFin+ data structures and results~
* [ ] Don't export all constructors, but let people import them individually, so they don't have to use `DuplicateRecordFields`?
