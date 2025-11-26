# notes

A [website] generated from my [zk notebook].

## Usage

**Note:** Usage via Guix is currently broken, waiting for next Hakyll release.

Invoke using [Guix] as follows:

	$ guix time-machine -C channels.scm -- shell -CN --expose=~/doc/zk-notebook
	[env] cabal run site -- build

Important to run this in a container, otherwise it messes with the host's Cabal config.

[website]: https://notes.8pit.net
[zk notebook]: https://github.com/zk-org/zk
[Guix]: https://guix.gnu.org
