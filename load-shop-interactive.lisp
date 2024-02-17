(in-package :cl-user)
(require :asdf)

(load "~/quicklisp/setup.lisp")

(asdf:load-system :sb-texinfo)
(ql:quickload "fiveam")
(ql:quickload "bordeaux-threads")
(ql:quickload "local-time")
(ql:quickload "shop3")
(ql:quickload "cl-ppcre") ; Perl-compatible regular expressions

;;; Once all the above is okay, you can call it from shell with ./save-lisp.sh and this line uncommented:
;;; I copy these to ~/Documents/lisp

;;; /usr/bin/sbcl --core pzmq-shop3-2024-02-15 --non-interactive --disable-debugger
(sb-ext:save-lisp-and-die "shop3-interactive-2024-02-15" :purify t :executable t)
