#! /bin/bash

quarto render
git add .
git commit -a -m "edits $now"
git push

echo "Quarto render commited!"