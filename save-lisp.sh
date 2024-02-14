#!/bin/bash

# Purpose: Build the executable image containing the SHOP3 planner.
#
# Steps:
#   1) Install quicklisp https://www.quicklisp.org/beta/ .
#   2) Install libzmq5 and libzmq3-dev. Ubuntu has them. The 5/3 is not a typo.
#   3) Copy source-registry.conf from this directory to ~/.config/common-lisp
#   4) Ugh! git clone git://github.com/nikodemus/sb-texinfo.git into somewhere below ~/Documents/git (See source-registry.conf.)
#   5) Execute this file in a shell: ./save-lisp.sh
#   6) If all goes well and the shell is waiting on a REPL prompt, it is because the last form in load.lisp (save-lisp-and-die)
#      is commented out. Uncommented it, rename the image (typically) and do it again.

sbcl --load /home/pdenno/Documents/git/shop3-zmq/load.lisp
