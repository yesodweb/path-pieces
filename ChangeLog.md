## Unreleased changes

* Use `http-api-data` package as a source for `PathPiece` instances
* Add new `PathPiece` instances:
    * `Char`
    * `Ordering`
    * `Double`, `Float`
    * `()`, `Void`
    * `Version`
    * `All`, `Any`, `Dual a`, `Sum a`, `Product a`, `First a`, `Last a`
    * `Either a b`
* Make `PathPiece` instances encoded in lower case and decoded case-insensitively
* Add `Web.PathPiece.Internal` module

## 0.2.0

* Make `PathMultiPiece` instance for lists based on `PathPiece` [Yesod issue #932](https://github.com/yesodweb/yesod/issues/932)

## 0.1.5

Added more integral instances, and check bounds on them [#5](https://github.com/yesodweb/path-pieces/pull/5).
