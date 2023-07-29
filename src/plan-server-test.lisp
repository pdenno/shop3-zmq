(in-package :s3zmq)

;;; ======> Most of the stuff here is out of date! <============

(defun try1 ()
  (ask-shop
   "(defdomain basic-example (
     (:operator (!pickup ?a) () () ((have ?a)))
     (:operator (!drop ?a) ((have ?a)) ((have ?a)) ())
     (:method (swap ?x ?y)
       ((have ?x))
       ((!drop ?x) (!pickup ?y))
       ((have ?y))
       ((!drop ?y) (!pickup ?x)))))"))

(defun try2 ()
  (ask-shop
   "(defproblem problem1 basic-example
      ((have banjo)) ((swap banjo kiwi)))"))

(defun try3 ()
  (ask-shop "(find-plans 'problem1 :verbose :plans)"))

(defun try4 ()
  (equal :bad-input (ask-shop "(/ 3 0)")))

(defvar *collected* nil)

;;;(defun test-client ()
;;;  "Test server by sending 3 requests"
;;;  (log-msg "Test-client-write starting.")
;;;  (sb-thread:make-thread
;;;   (lambda ()
;;;     (sleep 5)
;;;     (zmq:with-context (ctx)
;;;      (zmq:with-socket (sock ctx :push)
;;;       (loop for x in '(:hello :silly :world)
;;;	     do (sleep 1)
;;;            (zmq:connect sock *endpoint*)
;;;	    (log-msg "Test-client-write: ~A" x)
;;;	    (zmq:send sock (format nil "~S" x))
;;;	    (let ((got (zmq:recv sock 100000)))
;;;	      (log-msg "--->Test-client-read: ~A" got)
;;;	      (push got *collected*))))))))
(defun test-client (&optional (server-address *endpoint*))
  "Based on pzmq hwclient (hello world)."
  (log-msg "Test-client-write starting.")
  (pzmq:with-context (ctx :max-sockets 10)
    (pzmq:with-socket (requester ctx) (:req :affinity 3 :linger 100)
      ;; linger is important in case of (keyboard) interrupt;
      ;; see http://api.zeromq.org/3-3:zmq-ctx-destroy
      (write-line "Connecting to hello world server...")
      (pzmq:connect requester server-address)
      (loop for x in '(:hello :silly :world)
	do (sleep 1)
	   (format t "~%Sending Hello ~S...~%" x)
	   (pzmq:send requester (format nil "~S" x))
	   (write-string "Receiving... ")
	   (let ((got (pzmq:recv-string requester)))
	     (log-msg "--->Test-client-read: ~A" got)
	     (push got *collected*))))))

(defun diag-start-server-for-repl ()
  "Start the server that listens on *endpoint*."
  (format t "~%Starting plan server at ~A~%" *endpoint*)
  (log-msg "======Starting server.")
  (bt:make-thread #'server-loop :name "shop-server"))

(defun run-server-test ()
  (setf *collected* nil)
  (kill-server)
  (diag-start-server-for-repl)
  (test-client)
  (sleep 2)
  (kill-server)
  (format t "~% *colllected* = ~S" *collected*)
  *collected*)
