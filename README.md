# SimFin API wrapper for Haskell

See [simfin.com](https://simfin.com/)

Pre-alpha, expect frequent breaking changes.

No SimFin+ features have been tested at all yet.

## TODO:

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
