# notes

A website generated from my [zk notebook].

## Usage

Invoke using [Guix] as follows:

	$ guix time-machine -C channels.scm -- shell -CN
	[env] cabal run site -- build

Important to run this in a container, otherwise it messes with our host's Guix config.

[zk notebook]: https://github.com/zk-org/zk
[Guix]: https://guix.gnu.org
