v16.0.0 2024-09-11 Zagreb
-------------------------

- Unicode 16.0.0 support.
- Add `Uuseg.equal` thanks to @lord for suggesting.

v15.1.0 2023-09-15 Zagreb
-------------------------

- Unicode 15.1.0 support. 
- Requires OCaml 4.14.0 for the UTF decoders.
- The `Uuseg_string` module was rewritten to use the standard library
  UTF decoders and was moved to the `uuseg` library. The `uuseg.string`
  library is deprecated, it warns on usage and simply requires `uuseg`.
- The sample code was rewritten to use the standard library UTF
  decoders.

v15.0.0 2022-09-15 Zagreb
-------------------------

- Unicode 15.0.0 support.

v14.0.0 2021-09-17 Zagreb
-------------------------

- Unicode 14.0.0 support.

v13.0.0 2020-03-11 La Forclaz (VS)
----------------------------------

- Unicode 13.0.0 support.
- Grapheme clusters and word boundaries w.r.t. emojis are segmented
  according to the specification (#5 is closed).
- Internal rewrite of word and line break boundaries. Implementations
  are less hairy, less ad-hoc (not there yet though) and more correct.
- Require OCaml >= 4.03.0.

v12.0.0 2019-03-08 La Forclaz (VS)
----------------------------------

- Unicode 12.0.0 support. Grapheme cluster and word boundaries
  w.r.t. emojis are still only partially according to the specification
  see issue #5 for details.

v11.0.0 2018-06-06 Zürich
-------------------------

- Unicode 11.0.0 support. Grapheme cluster and word boundaries
  w.r.t. emojis are only partially supported according to the
  specification see issue #5 for details.
  
v10.0.0 2017-06-20 Cambridge (UK)
---------------------------------

- Unicode 10.0.0 support.

v1.0.1 2016-03-07 La Forclaz (VS)
---------------------------------

- OCaml 4.05 compatibility (removal of `Uchar.dump`).

v1.0.0 2016-11-23 Zagreb
------------------------

- Unicode 9.0.0 support.
- OCaml standard library `Uchar.t` support.
  - Removes and substitutes `type Uuseg.uchar = int` by the (abstract)
    `Uchar.t` type. `Uchar.{of,to}_int` allows to recover the previous
    representation.
  - Removes the `Uuseg.is_uchar`. `Uchar.is_valid` can be used instead.
- Safe string support.
- Build depend on topkg.
- Relicense from BSD3 to ISC.

v0.9.0 2015-06-17 Cambridge (UK)
--------------------------------

- Support for Unicode 8.0.0's new line breaking and sentence boundary rules.
- `Uuseg.custom` add a unit argument.


v0.8.0 2014-12-23 Cugy (VD)
---------------------------

First release.
