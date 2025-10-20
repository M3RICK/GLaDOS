#!/bin/bash

make re

read -p "Verbose output? (y/n): " verbose_choice

if [[ "$verbose_choice" == "y" || "$verbose_choice" == "Y" ]]; then
    verbose_flag="--verbose"
else
    verbose_flag=""
fi

echo "stack clean"
stack clean $verbose_flag

echo "stack build"
stack build $verbose_flag

echo "stack test"
stack test $verbose_flag

make fclean

echo "Okay"

echo "
⣿⣿⣿⣿⣿⣿⣿⣧⣝⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⢸⣿⣧⡹⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⢸⣿⣿⣿⣎⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⢸⢿⣻⡽⢷⢖⣊⣯⣭⣭⣽⣛⠿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⡿⢐⣭⢖⣭⣾⠿⠛⠛⠉⠉⠙⠻⣷⡜⣿⣿⣿⣿⣿⣿⣿
⣿⣛⣛⣛⣛⢭⡺⣿⣳⣿⣟⣵⣿⣿⣿⣿⢿⡓⠤⠀⢹⢸⣿⢫⣹⣿⣿⣿
⣷⣝⠿⣿⣿⢻⣇⡏⣿⣿⣘⠩⠭⠁⣀⣤⡀⠀⠀⢀⡘⣾⢣⣿⡇⣿⣿⣿
⣿⣿⣿⢈⣵⣿⣾⣷⣿⡇⠀⠀⠀⢀⣼⠿⣿⣶⣟⣵⢳⣿⢱⣿⣷⣜⢿⣿
⣿⣿⡿⣸⣿⣿⡿⣼⣿⢻⣀⣒⣻⣭⣶⣿⣿⣿⢟⢣⡇⢫⣼⣿⣶⣿⣦⢻
⣿⣏⡸⠏⡿⢟⣽⣿⡟⢺⡿⠿⠿⢿⣿⣛⠯⣗⣵⣿⢳⢿⣿⣿⣿⢿⣿⣿
⣿⣿⣿⣧⡺⠟⠯⣟⡼⣿⣶⣶⣿⣿⣿⣶⣿⣿⢟⣵⣿⢸⣿⣷⣾⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣮⣭⣿⣛⣛⣯⣭⣶⣿⣿⣿⣷⣭⣛⠿⠿⢟⣼
"
