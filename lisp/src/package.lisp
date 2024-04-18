(in-package :cl-user)

(defpackage :shop3-zmq
  (:nicknames :s3zmq)
  (:shadowing-import-from :pzmq #:close #:version)
  (:use :cl :shop3 :pzmq :asdf/interface)
  (:export #:start-server))
