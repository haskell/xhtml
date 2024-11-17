## 3000.4.0.0

- Memory allocation improvements [#20](https://github.com/haskell/xhtml/pull/20)

## 3000.3.0.0

- The internal representation has changed from `String` and `[String]` to a
  `Data.ByteString.Builder` and difference lists.
  [#19](https://github.com/haskell/xhtml/pull/19)

## 3000.2.2.1

- Special release which supports *only* `base >= 4.11`
  (while 3000.2.2 supports `base < 4.11`) but is otherwise
  morally the same as `xhtml-3000.2.2`.

## 3000.2.2

- Add `Semigroup Html` instance
- Update to `cabal-version:>=1.10`
- Clean-up compiler warnings
