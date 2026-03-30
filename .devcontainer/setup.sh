#!/bin/bash
# Post-creation setup script for GitHub Codespaces
# Installs Quicklisp and required Common Lisp libraries

set -e

QUICKLISP_DIR="$HOME/quicklisp"

# Install Quicklisp if not already present
if [ ! -d "$QUICKLISP_DIR" ]; then
    echo "Installing Quicklisp..."
    tmpdir=$(mktemp -d)
    curl -o "$tmpdir/quicklisp.lisp" https://beta.quicklisp.org/quicklisp.lisp
    sbcl --load "$tmpdir/quicklisp.lisp" \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(quit)'
    rm -rf "$tmpdir"
    echo "Quicklisp installed."
else
    echo "Quicklisp already installed, skipping."
fi

# Load Quicklisp and install required libraries
# These match the ql:quickload calls in the .lisp source files.
# Update this list if new libraries are added to the project.
echo "Installing required Common Lisp libraries..."
sbcl --load "$QUICKLISP_DIR/setup.lisp" \
     --eval "(ql:quickload '(:with-user-abort :adopt :str :parse-float :cl-charms :fuzzy-match :cl-csv :cl-ppcre) :silent t)" \
     --eval '(quit)'

echo "Setup complete. SBCL and Common Lisp libraries are ready."
