#!/bin/bash

DIR=$(dirname ${0})
DOT_FILES=( .zshrc .gitconfig .tmux.conf .dir_colors .emacs.d )

for file in ${DOT_FILES[@]}
do
    ln -s "$DIR/$file" "$HOME/$file"
done
