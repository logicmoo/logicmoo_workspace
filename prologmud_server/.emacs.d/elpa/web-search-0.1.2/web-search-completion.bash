#!/usr/bin/env bash
#
# web-search-completion - bash/zsh provider completion support for web-search
#
# Usage: Add the following line to .bashrc or .zshrc
#
#   . /path/to/web-search-completion.bash
# 
# This file is adapted from https://raw.githubusercontent.com/zquestz/s/master/autocomplete/s-completion.bash

if [[ -n ${ZSH_VERSION-} ]]; then
    autoload -U +X compinit && compinit
    autoload -U +X bashcompinit && bashcompinit
fi

_provider_completion()
{
    local cur=${COMP_WORDS[COMP_CWORD]}
    local prev=${COMP_WORDS[COMP_CWORD-1]}
    local longOpts="--help --list-providers --list-tags --provider --tag --output --verbose --version --completion"

    # XXX: This doesn't work in Zsh
    # _filedir (in newer bash completions) is a huge improvement on compgen -f or compgen -G
    # because it deals correctly with spaces, ~ expansion, and .inputrc preferences.
    comp      () { COMPREPLY=( $(compgen "$@" -- "$cur") ); }
    comp_path () { if type _filedir >/dev/null; then _filedir ; else comp -G $cur\* ; fi; }

    case "$cur" in
        -) comp -W "-h -l -p -t -o -v" && return 0 ;;
        -*) comp -W "$longOpts" && return 0 ;;
    esac

    # show the long options if current word is empty and previous one
    # isn't an option which expects an argument
    case "$prev" in
        # To learn about IFS, refer to (info "(bash) Word Splitting")
        -p|--provider) IFS=$'\n' comp -W "$(web-search --list-providers)" ;;
        -t|--tag) IFS=$'\n' comp -W "$(web-search --list-tags)" ;;
        *) [[ -z "$cur" ]] && comp -W "$longOpts" ;;
    esac


} && complete -o filenames -F _provider_completion web-search
