0.5.0.0 (9 Jul 2015)
---------------------

- Functions in the `Path` module output numbers to 4 decimal places
  using `Data.Text.Lazy.Builder.RealFloat (formatRealFloat)` instead of `show`.

0.4.0.4 (9 Mar 2015)
---------------------

- Allow blaze-builder < 0.5

0.4.0.3 (24 Feb 2015)
---------------------

- Require lucid 2.9.2 (hashable instance for `Attribute`).

0.4.0.2 (10 Feb 2015)
---------------------

- Allow lucid 2.9

0.4.0.1 (27 Jan 2015)
---------------------

- Widen dependency bounds.

0.4 (27 Jan 2015)
---------------------

- Add `prettyText` function for pretty printing SVG.

0.3 (27 Jan 2015)
---------------------

 - Change names of attribute functions to more closely match SVG spec.
   For example: SVG's cap-height is lucid-svg `cap_height`_.

0.2.1 (27 Jan 2015)
---------------------

- Expose `Attribute(..)` constructor from Lucid.Base.

0.2.0.2 (27 Jan 2015)
---------------------

- allow lucid 2.8.1.

0.2.0.1 (23 Jan 2015)
---------------------

- Fix documentation.

0.2 (22 Jan 2015)
-----------------

- Fix bug in elements functions.
- Make path creating functions polymorphic.

0.1 (17 Jan 2015)
-----------------

- Initial release.
