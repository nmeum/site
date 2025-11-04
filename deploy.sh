#!/bin/sh
exec rsync \
	--delete-excluded \
	--checksum \
	--progress \
	--archive \
	--verbose \
	_site/ magnesium:/var/www/htdocs/notes.8pit.net/
