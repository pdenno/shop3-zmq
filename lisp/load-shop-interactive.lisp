(in-package :cl-user)
(require :asdf)

;;; This is pretty much what load-list does, sans save-and-die.

(load "~/quicklisp/setup.lisp")

(asdf:load-system :sb-texinfo)
(ql:quickload "fiveam")
(ql:quickload "bordeaux-threads")
(ql:quickload "local-time")
(ql:quickload "pzmq")
(ql:quickload "shop3")
(ql:quickload "cl-ppcre") ; Perl-compatible regular expressions

(pushnew :plan-server *features*)
(load (compile-file "~/Documents/git/shop3-zmq/src/package.lisp"))
(asdf:load-system :shop3-zmq)
