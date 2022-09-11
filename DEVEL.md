# New unicode release

Update the new Unicode version number in [`B0.ml`](B0.ml).

Before the formal Unicode release check the proposed update of [UAX29]
and [UAX14]. If the rules need to be adjusted then do so.

If there are only data additions then simply update [Uucp] with the
new data, compile Uuseg against this new uucp and check the reference
tests (see below).

Update the opam file with: 

```
b0 cmd .opam.file > opam
```

[UAX29]: https://www.unicode.org/reports/tr29/
[UAX14]: https://www.unicode.org/reports/tr14/
[Uucp]: https://erratique.ch/software/uucp

# Reference tests

To download the reference segmentation test files for the version 
mentioned in `B0.ml` to the `test` directory issue:

    b0 cmd download-tests

You can then check them via: 

    b0 -a test
