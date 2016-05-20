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
   `tabcode-haskell`'s dependencies:

        $ cabal update
        $ cabal install --only-dependencies

5. Compile:

        $ cabal build

### Usage

Following the build process, you have two executables in
`tabcode-haskell/dist`: `tc2mei` and `tccheck`. Both take a single
file name as an argument. `tc2mei` then writes an MEI XMl version of
the file to standard out. `tccheck` will either report errors in
parsing the file or print a representation of the data structure into
which the file was pasred.

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
