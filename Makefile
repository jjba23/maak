# ggg: guile glyph generator

# Copyright © Josep Bigorra <jjbigorra@gmail.com>

# ggg is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# ggg is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.

# You should have received a copy of the GNU Affero General Public License
# along with ggg.  If not, see <https://www.gnu.org/licenses/>.

dev-run:
	guix shell -m manifest.scm -- \
		guile -L ./src -c '((@(ggg main) main))' \
		--file $(realpath ./glyphs/jjba23.scm) --destination ./dist \
		--resources-dir ./resources
dev:
	watchexec -i "dist/**" -r -e scm,css,js -- "make dev-run"

run:
	guix shell -f guix.scm -- \
		ggg --file $(realpath ./glyphs/jjba23.scm) --destination ./dist

run-convert:
	guix shell -f guix.scm -- \
		ggg --file $(realpath ./glyphs/jjba23.scm) --destination ./dist --convert

run-stdin:
	cat glyphs/jjba23.scm | guix shell -f guix.scm -- \
		ggg --destination ./dist

run-stdout:
	guix shell -f guix.scm -- \
		ggg --file $(realpath ./glyphs/jjba23.scm) --stdout

run-stdin-stdout:
	cat ./glyphs/jjba23.scm | guix shell -f guix.scm -- \
		ggg -i -o

help:
	guix shell -f guix.scm -- ggg --help

# Target to format source code files according to the style guide
# Find all .scm files and format them using 'guix style'
fmt:
	find . -maxdepth 8 -name "*.scm" -type f -exec guix style -f {} \;

# Development REPL
repl:
	guix shell -m manifest.scm -- \
		guile \
		-L ./src \
		-c '((@ (ares server) run-nrepl-server))'

# Deploy generated assets to jointhefreeworld
deploy:
	CI=true make run
	mkdir -p /srv/http/jointhefreeworld.org/ggg
	rm -rfv /srv/http/jointhefreeworld.org/ggg/dist
	mv -v dist /srv/http/jointhefreeworld.org/ggg/dist
