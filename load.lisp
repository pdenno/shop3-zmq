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

;;; Without the :executable t, you'd have to do: /usr/bin/sbcl --core pzmq-shop3-2024-02-15 --non-interactive --disable-debugger IF you want to use it with slime.
;;; Since this one start the server, you don't want to use it with slime anyway.
;;; In schedulingTBD, or just from a shell, you can use this simply by doing  ./pzmq-shop3-2024-02-15
;;;                                                                        or ./pzmq-shop3-2024-02-15 --non-interactive --disable-debugger.
(sb-ext:save-lisp-and-die "pzmq-shop3-2024-02-15" :purify t :executable t :toplevel 's3zmq:start-server)
