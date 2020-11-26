#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $DIR

# find -iname sanity_*pl -exec echo "swipl -g \"ensure_loaded(pack('logicmoo_base/t/{}')),halt.\" " \;

find -iname sanity_*pl -printf "swipl -g \"ensure_loaded(pack('logicmoo_base/t/%P')),halt.\"\n"

cd -

