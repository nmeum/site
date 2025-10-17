#!/bin/sh
set -e

# TODO: obtain this information using the zk(1) command.
NOTEBOOK="${HOME}/doc/zk-notebook"

mkdir -p notes/
zk list --format {{path}} public/ | while read -r note; do
	ln -sf "${NOTEBOOK}/${note}" notes/${note##*/}
done
