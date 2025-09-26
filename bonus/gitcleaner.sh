#!/bin/bash

branches=(
  dev_CI/CD
  dev_compilator
  dev_parsing
  dev_stdin
)

for old in "${branches[@]}"; do
  new="LISP/$old"

  # Checkout the old branch
  git checkout "$old"

  # Rename locally
  git branch -m "$new"

  # Delete old branch remotely
  git push origin --delete "$old"

  # Push new branch to remote
  git push origin "$new"

  # Set upstream tracking
  git push --set-upstream origin "$new"

  echo "Renamed $old → $new"
done
