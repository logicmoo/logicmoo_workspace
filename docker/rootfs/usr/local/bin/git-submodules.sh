cls 

export mstr="${@:2}"
echo mstr=$mstr
find $1 -maxdepth 2 -name ".git" -type d ! -path '*yesbot*' ! -path '*/.n*' ! -path '*~*' \
   -printf "\n\n------------------------------\nCHECKING.. %P\n"  -execdir sh -c "pwd ; ${@:2}" \;
