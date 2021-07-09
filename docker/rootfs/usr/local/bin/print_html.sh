#!/bin/bash

SCREEN=0
if [ -n "$STY" ]; then
SCREEN=1
fi
if [ -t 1 ] && [ -t 0 ]; then
  s=$(stty -g)
  stty -icanon -echo min 0 time 3
  printf '\033[>c'
  type=$(dd count=1 2> /dev/null)
  stty "$s"
  case $type in
    (*'>83;'*) SCREEN=1
  esac
fi
case $TERM in
  (screen*) SCREEN=1
esac

function output_html() {
# [ SCREEN==1 ] && echo -n -e "\eP"
echo $TEXT | /usr/local/bin/b html
[ SCREEN==1 ] && echo -n -e "\e\\"
}

if [ "$#" -ne 0 ]; then
 TEXT=$(cat $1)
 output_html "$TEXT" "$TEXT"
 return 0 2>/dev/null
 exit 0
fi


set +m # disable job control in order to allow lastpipe
shopt -s lastpipe
TEXT=""
for foo in `cat /dev/stdin`
do
TEXT=$foo
done
#echo "Captured Text: $TEXT"
output_html "$TEXT" "$TEXT"

return 0 2>/dev/null
exit 0


while IFS= read -r line; do lines[i]="$line"; ((i++)); done
echo "${lines[1]}" 
echo -n -e "\eP"
echo -n -e "\e]8;;"
echo -n -e "https://example.com"
echo -n -e "\a"
echo -n -e "This is a link"
echo -n -e "\e]8;;\a\c"
echo -n -e "\e\\"

function output_html() {
[ SCREEN==1 ] && echo -n -e "\eP"
echo $EXT | /usr/local/bin/b html
echo -n -e "\e]8;;"
echo -n -e "\eP;HTML|"
echo -n -e "<div class='pre'><pre>"
echo -n -e " "
echo -n -e $1
echo -n -e " "
echo -n -e "</pre></div>\eP\e\\"
echo -n -e "\a"
echo -n -e " "
echo -n -e $2
echo -n -e " \n"
echo -n -e "\e]8;;\a\c"
[ SCREEN==1 ] && echo -n -e "\e\\"
echo -n -e "\e\\"

