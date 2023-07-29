;;;(defpackage :shop3-zmq
;;;  (:use :common-lisp :asdf :pzmq))

;;; Check that ~/.config/common-lisp/source-registry.conf contains the src directory for shop3-zmq
(asdf:defsystem "shop3-zmq"
  :long-description "A server of UMd's/Robert P. Goldman's SHOP3 planner using ZeroMQ"
  :depends-on ("shop3")
  :serial t
  :components ((:file "package")
	       (:file "plan-server")))
