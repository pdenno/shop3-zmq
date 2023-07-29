(in-package :shop3-zmq)

(defparameter *endpoint* "tcp://localhost:31726")

(defun ask-shop (request-str)
  "Eval request string in :shop2-user package."
  (handler-case
      (let* ((*package* (find-package :shop3-user))
	     (form (read-from-string request-str)))
	(eval form))
    (serious-condition (e)
      (format t "Serious condition: ~A" e)
      (log-msg  "Serious condition: ~A" e)
      :bad-input)))

(defun kill-server ()
  "Kill the SHOP-Server"
  (loop for thread in (bt:all-threads)
	do (when (string-equal "shop-server" (bt:thread-name thread))
	     (format t "~% Killing thread = ~S" thread)
	     (bt:destroy-thread thread))))

;;; (bt:make-thread #'server-loop :name "shop-server")
(defun server-loop (&optional (listen-address "tcp://*:31726")) ; See *endpoint* above.
  "Translation of http://zguide.zeromq.org/c:hwserver updated for ZMQ 3. "
  (handler-case
      (pzmq:with-context (ctx :max-sockets 10)
	(pzmq:with-socket responder :rep
	  (pzmq:bind responder listen-address)
	  (loop
	    ;;(format t "~%Waiting for a request... ")
	    (let ((got (pzmq:recv-string responder)))
	      (if (string-equal got "(sb-ext:exit)")
		  (progn
		    (pzmq:send responder ":bye!")
		    (log-msg (format nil "-----Server shutting down on  ~S----" got))
		    (sleep 2)
		    (sb-ext:exit))
		  (progn
		   (log-msg (format nil "Server receives: ~S" got))
		   (let ((result (ask-shop got)))
		     (log-msg (format nil "Server replying: ~A" (fmt-msg result)) result)
		     (pzmq:send responder (format nil (fmt-msg result) result)))))))))
    (serious-condition (c)
      (log-msg "Server stopping ~A" c))))

(defun fmt-msg (msg)
  "Return a format string for a message"
  (cond ((stringp msg) "~S")
	((keywordp msg) "~S")
	(t "~A")))

(defun start-server ()
  "Start the server that listens on *endpoint*."
  (format t "~%Starting plan server at ~A~%" *endpoint*)
  (log-msg "======Starting server.")
  (server-loop))

(defparameter *local-time-format* '((:hour 2) ":" (:min 2) ":" (:sec 2) "." (:msec 3) "  "))
(defun log-msg (fmt &rest args)
  (let ((msg (apply #'format nil fmt args)))
    (with-open-file (out "/tmp/plan-log.txt"
			 :if-does-not-exist :create
			 :direction :output :if-exists :append)
      (format out "~%~A: ~A" (local-time:now) msg))))

;;; ========================= Lisp as client ================================
(defparameter *clj-endpoint* "tcp://localhost:31728") ; not 31726

;;; If these are even needed, use Bordeaux Threads.

;;;(defun wait-response (sock timeout)
;;;  "Return within timeout or return :timeout."
;;;  (let* ((ready? nil)
;;;	 (reader (sb-thread:make-thread
;;;		  (lambda ()
;;;		    (setf ready? (pzmq:msg-recv sock 100000)))))
;;;	 (result (or (sb-ext:wait-for ready? :timeout timeout) :timeout)))
;;;    (when (sb-thread:thread-alive-p reader)
;;;      (sb-thread:terminate-thread result))
;;;    result))

;;;(defun ask-clojure (request-form)
;;;  (pzmq:with-context (ctx)
;;;    (pzmq:with-socket (sock ctx :req)
;;;      (pzmq:connect sock *clj-endpoint*)
;;;      ;(pzmq:send sock (pzmq:make-msg :data (format nil "~S" request-form)))
;;;      (pzmq:send sock (format nil "~S" request-form))
;;;      ;(make-instance 'pzmq:msg :data (format nil "~S" request-form))
;;;      (let ((result (wait-response sock 5)))
;;;        (format t "~%Received: ~A " result)
;;;	(read-from-string (format nil "~A" result))))))

;;; ========================================================================
;;; Planlib helpers.
(in-package :shop2-user)

(defun query-test (&rest args)
  (format t "~%*********** Query-test: args = ~A" args)
  t)

(defun query (&rest args)
  (format t "~%*********** Query: args = ~A" args)
  t)
