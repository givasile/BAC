#!/bin/bash

# generate exercises for Session 4
file1="./session_4/theory/bayesian_linear_regression.R"
file2="./session_4/exercises/exercises.R"

# Remove solution blocks from all .R files in the current directory
for file in *.R; do
  sed '/# @solution/,/# end/d' "$file1" > "$file2"
done

# generate exercises for Session 5
file1="./session_5/theory/bayesian_logistic_regression.R"
file2="./session_5/exercises/exercises.R"

# Remove solution blocks from all .R files in the current directory
for file in *.R; do
  sed '/# @solution/,/# end/d' "$file1" > "$file2"
done
