(in-package :cl-user)
(require :asdf)

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

;;; Once all the above is okay, you can call it from shell with ./save-lisp.sh and this line uncommented:
;;; I copy these to ~/Documents/lisp
;;(sb-ext:save-lisp-and-die "pzmq-shop3-2023-07-29" :purify t :executable t :toplevel 's3zmq:start-server)
