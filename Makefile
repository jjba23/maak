# kmk

# Copyright © Josep Bigorra <jjbigorra@gmail.com>

# ggg is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# ggg is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with ggg.  If not, see <https://www.gnu.org/licenses/>.

run:
	guix shell -f guix.scm -- \
		kmk -- fmt
# guix shell -f guix.scm -- \
# 	kmk -f ./kmk.scm -- fmt

fmt-then-repl:
	guix shell -f guix.scm -- \
		kmk -- fmt,repl

help:
	guix shell -f guix.scm -- kmk --help

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
