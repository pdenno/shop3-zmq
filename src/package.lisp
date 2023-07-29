(in-package :cl-user)

(defpackage :shop3-zmq
  (:nicknames :s3zmq)
  (:use :cl :shop2 :asdf/interface)
  (:export #:start-server))
