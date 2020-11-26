#!/usr/bin/env bash

#
# run.sh
#
# Copyright (C) 2016 frnmst (Franco Masotti) <franco.masotti@student.unife.it>
#
# This file is part of swish-installer.
#
# swish-installer is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# swish-installer is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with swish-installer.  If not, see <http://www.gnu.org/licenses/>.
#
#

# This is the file called from the /usr/bin/trill-on-swish symlink

pkg_dir="/home/mlunife/trill-on-swish"
pid_file="/home/mlunife/trill-on-swish/swish.pid"
output_file="/home/mlunife/trill-on-swish/output.txt"
user="mlunife"
group="mlunife"

help()
{
    cat<<-EOF
swish [OPTION]
SWI-Prolog for SHaring: a SWI-Prolog web IDE
Only a single option is permitted.
    -h      print this help
    -k      kill swish
    -s      start swish
Exit status:
 0  if OK,
 1  some error occurred.
Full documentation at: <https://github.com/SWI-Prolog/swish>
EOF
}

killd()
{
    # kill action only if process exists.
    if [ -f "$pid_file" ]; then
        pid=$(cat "$pid_file")
        ps -p $pid > /dev/null
        if [ $? -eq 0 ]; then
            kill -s TERM $pid
        fi
    fi
}

init()
{
    :
}

remove()
{
    :
}

startd()
{
    local pid=""

    {
        (
            cd "$pkg_dir"
            ./daemon.pl --port=3020 --pidfile="$pid_file" --output="$output_file"
        )  
    }
}

case "$1" in
        -s)
                startd
                ;;
        -k)
                killd
                ;;
        *)
		help
esac
