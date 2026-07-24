#!/bin/sh
set -e -x

# XXX: hakyll's watch command does not work with symlinks due to a liminitation
# of the fs-notify library. Work around that by using entr(1) for watching.
#
# See: https://github.com/jaspervdj/hakyll/issues/502

find index.md css templates/ notes/ -type f | \
	entr cabal run site -- build
