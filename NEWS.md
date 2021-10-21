# ids 1.2.0

* The `openssl` package is now optional, with `ids` now shipping with an internal random number generator which can be used for identifiers that do not depend on R's global random number state (used by `ids::random_id`, `ids::proquint` and `ids::uuid`). If openssl is found it will still be used. (#11)
* The `uuid` package is no longer used by `ids::uuid` and we instead generate version 4 uuids. As before these do not depend on R's global random number state (#4)

# ids 1.1.1

* The `adjective_animal` and `sentence` generators now support [MocKiNg sPoNgEbOb CaSe](https://knowyourmeme.com/memes/mocking-spongebob) in addition to usual case conversions.

# ids 1.1.0 (unreleased)

* The `adjective_animal` generator now supports alliterative adjective animals (such as `convectional_conflictory_cod`), though this reduces the pool size (#5, requested by @gadenbuie).

# ids 1.0.1 (2017-05-22)

* Fix occasionally failing test (removes one animal from the pool)
* New identifier type "proquint"

# ids 1.0.0 (2016-11-03)

* Initial CRAN release
