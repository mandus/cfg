# Fish function to import bash aliases v0.2
# Copyright (C) 2016 Malte Biermann
# Copyright (C) 2017 Rodrigo Bermudez Schettino
# Released under the GPL
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Imports aliases defined in ~/.bash_aliases formatted as alias name='command'
function import_bash_aliases --description 'bash aliases to .fish function files'
    for a in (cat ~/.bash_alias  | grep "^alias")
        #echo $a
        
        # set aname (echo $a | sed "s/alias \(.*\)='.*/\1/")
        # To import aliases with commands enclosed in double quotes uncomment
        # the following line
        set aname (echo $a | sed 's/alias \(.*\)=".*/\1/')
        #echo $aname
        
        #set command (echo $a | sed "s/alias.*='\(.*\)'/\1/")
        # To import aliases with commands enclosed in double quotes uncomment
        # the following line
        set command (echo $a | sed 's/alias.*="\(.*\)"/\1/')
        #echo $command
        
        echo "Processing alias $aname as $command"
        
        # TODO: Check if abbreviation already exists and avoid overwriting it
        # This can be achieved by using the fish function in
        # https://github.com/fish-shell/fish-shell/blob/036b708d9906d4f1fcc7ab2389aa06cf5ec05d11/share/functions/abbr.fish#L130
        abbr -a $aname $command
    end
end
