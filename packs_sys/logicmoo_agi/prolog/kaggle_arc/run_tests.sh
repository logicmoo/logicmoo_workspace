#!/bin/bash


#!/bin/bash

REPO_URL="https://github.com/example/repository.git"
DEST_DIR="versions"
NUM_SHAS=10

# Create the destination directory if it doesn't exist
mkdir -p "$DEST_DIR"

# Clone the repository
# git clone "$REPO_URL" "$DEST_DIR/original"
# cd "$DEST_DIR/original"

# Get the last 10 SHA values
SHA_ARRAY=($(git log --pretty=format:'%h' -n 10))

# Loop through the SHA array and check out each version
for sha in "${SHA_ARRAY[@]}"; do
    git archive -o $sha-update.zip $sha $(git diff-tree -r --no-commit-id --name-only --diff-filter=ACMRT $sha^)
    unzip $sha-update.zip -d $sha-update-folder
    mv $sha-update-folder/prolog/kaggle_arc/?*.pl $sha-update-folder/
done

  # Check out the specific SHA into the version directory
 # git checkout "$sha" ./kaggle_arc_*.pl
   cp -R ./kaggle_arc_*.pl "$version_dir"

  # Return to the original directory
  # git checkout master
done



   git archive -o update.zip HEAD $(git diff --name-only HEAD^)
# Clean up the cloned repository
rm -rf "$DEST_DIR/original"



swipl -l kaggle_arc -t $* | tee -i out.ansi
#echo cat output.ansi | ansi2html -l --style 'pre {font-family: Consolas}' > all_tasks.html

cat out.ansi |  ansi2html -a -W -u  > out.html

#cat output.ansi | ansi2html -l --style 'pre {font-family: Consolas}' > all_tasks.html
