# notes

A [website] generated from my [zk notebook].

More information about the setup: <https://notes.8pit.net/notes/c0p9.html>.

## Usage

Invoke using [Guix] as follows:

	$ guix time-machine -C channels.scm -- shell -CN --expose=~/doc/zk-notebook
	[env] cabal run site -- build

Important to run this in a container, otherwise it messes with the host's Cabal config.

[website]: https://notes.8pit.net
[zk notebook]: https://github.com/zk-org/zk
[Guix]: https://guix.gnu.org
