!# /bin/bash

quarto render
git add .
git commit -a -m "edits"
git push