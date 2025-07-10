#!/bin/bash

# # Compile all theoretical notes
# for dir in ./session_1 ./session_2 ./session_3 ./session_4 ./session_5; do
#     pdflatex -output-directory="${dir}/theory/" "${dir}/theory/theory.tex"
# done

# Session 1
cp "./session_1/exercises/exercises.tex" "./solutions/session_1/exercises/exercises.tex"
sed -i 's/% \\showanswerstrue/\\showanswerstrue/' "./solutions/session_1/exercises/exercises.tex"
pdflatex -output-directory="./solutions/session_1/exercises/" "./solutions/session_1/exercises/exercises.tex"

# Session 2
cp "./session_2/exercises/exercises.tex" "./solutions/session_2/exercises/exercises.tex"
sed -i 's/% \\showanswerstrue/\\showanswerstrue/' "./solutions/session_2/exercises/exercises.tex"
pdflatex -output-directory="./solutions/session_2/exercises/" "./solutions/session_2/exercises/exercises.tex"

touch "./session_2/exercises/exercises.tex"

# Session 3
cp "./session_3/exercises/exercises.tex" "./solutions/session_3/exercises/exercises.tex"
sed -i 's/% \\showanswerstrue/\\showanswerstrue/' "./solutions/session_3/exercises/exercises.tex"
pdflatex -output-directory="./solutions/session_3/exercises/" "./solutions/session_3/exercises/exercises.tex"

# Session 4
# generate exercises for Session 4
file1="./solutions/session_4/exercises/exercises.R"
file2="./session_4/exercises/exercises.R"
sed '/# @solution/,/# end/d' "$file1" > "$file2"

# Session 5
# generate exercises for Session 5
file1="./solutions/session_5/exercises/exercises.R"
file2="./session_5/exercises/exercises.R"
sed '/# @solution/,/# end/d' "$file1" > "$file2"
