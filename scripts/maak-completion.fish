#!/usr/bin/env fish

# Copyright © Josep Bigorra <jjbigorra@gmail.com>

# maak is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# maak is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with maak. If not, see <https://www.gnu.org/licenses/>.

function __fish_maak_targets
    set -l maak_file_name "maak.scm"

    if test -f $maak_file_name
        awk '/^\(define \(/ { sub(/;.*/, ""); gsub("[()]", ""); if (NF == 2) print $2 }' $maak_file_name
    end
end

complete -c maak -f -a "(__fish_maak_targets)"
