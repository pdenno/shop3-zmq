(in-package :shop3-zmq)

(defparameter *endpoint* "tcp://localhost:31726")

(defun ask-shop (request-str)
  "Eval request string in :shop2-user package."
  (handler-case
      (let* ((*package* (find-package :shop3-user))
	     (form (read-from-string request-str)))
	(eval form))
    (serious-condition (e)
      (log-msg  "Serious condition: ~A" e)
      :bad-input)))

(declaim (ftype (function (&optional boolean) t) stop-server))

(defun server-loop (&optional (listen-address "tcp://*:31726")) ; See *endpoint* above.
  "Translation of http://zguide.zeromq.org/c:hwserver updated for ZMQ 3. "
  (handler-case
      (pzmq:with-context (ctx :max-sockets 10) ; ctx is a variable not a function!
	(pzmq:with-socket responder :rep
	  (pzmq:bind responder listen-address)
	  ;; Flush messages from prior invocations.
	  ;; recv-string actually returns values; second value is about rcvmore.
	  (dotimes (i 2000)
	    (handler-case
		(progn (log-msg "Flushing ~S" (pzmq:recv-string responder :dontwait t))
		       (sleep 1e-4))
	      (pzmq:eagain ())))
	  (log-msg "Entering server loop")
	  (loop
	    ;;(log-msg "Waiting for a request... ")
	    (let ((got (pzmq:recv-string responder))) ; See above about more. (This will matter for big defdomains.)
	      (log-msg "Server receives: ~S" got)
	      (if (string-equal got "(sb-ext:exit)")
		  (progn
		    (log-msg "-----Server shutting down on  ~S----" got)
		    (pzmq:send responder ":bye!")
		    (log-msg "after bye")
		    (sleep 0.1)
		    (pzmq:close responder)   ;  See Page 20 in the PDF.
		    (log-msg "after close")
		    (pzmq:ctx-shutdown ctx)
		    (log-msg "after shutdown")
		    (pzmq:ctx-term ctx)      ;  See Page 20 in the PDF.
		    (log-msg "Exiting lisp.")
		    (sb-ext:exit))
		  (progn
		    (let ((result (ask-shop got)))
		      (log-msg "Server replying: ~S" result)
		      (pzmq:send responder (format nil (fmt-msg result) result)))))))))
    (serious-condition (c)
      (log-msg "Serious condition, exiting loop: ~S" c))))

(defun start-server () (server-loop))

(defun send-server-stop-msg ()
  "Send '(sb-ext:exit)' which means stop. For debugging, I think."
  (log-msg "======Telling the plan server to stop")
  (pzmq:with-context (ctx :max-sockets 10)
    (pzmq:with-socket (socket ctx) :req
      (pzmq:connect socket *endpoint*)
      (pzmq:send socket "(sb-ext:exit)"))))
