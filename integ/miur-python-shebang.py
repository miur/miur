#!/usr/bin/env -S python -SIB -X faulthandler
# %WARN!. zcompile will error-out on this multi-shebang!
# %SUMMARY: frontend for both .py and .sh
# %USAGE: $ mi || miur || . =mi
"""":
if (return 0 2>/dev/null); then
    _app=$(realpath -e "${BASH_SOURCE[0]:-$0}")
    _app=${_app%/*/*}/src/__main__.py  # <TEMP
    if [[ ${_app#/usr/} != ${_app} ]]
    then _als="${_app%/*/*}/share/miur/integ/miur-shell-aliases.sh"
    else _als="${_app%/*/*}/integ/miur-shell-aliases.sh"
    fi
    source "$_als" "$_app" "$@"
    unset _app _als
    return 0
fi
echo "ERR: '$0' is not supposed to be run by $SHELL"
# exec "$0" "$@"
exit -2
"""

print("hi!")
