## TabCode Parser

The Tabcode language was developed by Tim Crawford for typing lute
tablature sources in a simple ASCII format. This package provides a
parser for the language, a collection of data types for representing
lute tablature, and tools for checking and converting Tabcode files to
[MEI](http://music-encoding.org/).

### Installation

1. Ensure that you have `ghc` and `cabal` installed on your
   system. One option is to install the
   [Haskell Platform](https://www.haskell.org/platform/).
2. Clone the repository:

        $ git clone https://github.com/TransformingMusicology/tabcode-haskell.git

   Or download it.

3. Set up a Cabal sandbox in the `tabcode-haskell` directory:

        $ cd tabcode-haskell
        $ cabal sandbox init
        $ cabal configure

4. Ensure your Cabal package database is up-to-date and install
   `tabcode-haskell`'s dependencies (and optionally the test suite's
   dependencies):

        $ cabal update
        $ cabal install --only-dependencies
    
    Or:
    
        $ cabal install --only-dependencies --enable-tests
    
    to install dependencies for the tests.

5. Optionally enable the tests:

        $ cabal configure --enable-tests

6. Compile (and optionally run the tests):

        $ cabal build
        $ cabal test

### Usage

Following the build process, you have two executables in
`tabcode-haskell/dist`: `tc2mei` and `tccheck`. Both read Tabcode data
from standard in. `tc2mei` then writes an MEI XML version of its input
to standard out. `tccheck` will either report errors in parsing the
input and return exit code `1` or, if the input was valid, just return
exit code `0`.

It may be useful to copy the `tc2mei` and/or `tccheck` executables
into a directory on your `PATH`, e.g.:

    $ sudo cp tabcode-haskell/dist/build/tc2mei/tc2mei /usr/local/bin

This way, you can use `tc2mei` from anywhere.

The output produced by the `xmlgen` library is not very readable. If
you need to inspect it visually, you may consider formatting it with
`xmllint`:

    $ cat tabcodefile.tc | tc2mei | xmllint --format -

Or to put it into a file:

    $ cat tabcodefile.tc | tc2mei | xmllint --format - > tabcodefile.xml

When `tc2mei` encounters an error in the TabCode input it will abort
immediately with an error message. You can optionally supply the
`--permissive` command line switch which will cause it instead to
continue parsing the rest of the input and then report any unparsable
tabwords in the XML output as comments, e.g.:

    $ cat tabcodefile-with-errors.tc | tc2mei --permissive | xmllint --format - > tabcodefile-with-errors.xml

### Limitations

* Various features of Tabcode are not yet converted to MEI by
  `tc2mei`, including: ornaments, articulation markings, and
  connecting lines.

### License

Copyright (C) 2015, 2016 Richard Lewis, Goldsmiths' College
Author: Richard Lewis <richard.lewis@gold.ac.uk>

This file is part of TabCode

TabCode is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

TabCode is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with TabCode.  If not, see <http://www.gnu.org/licenses/>.
