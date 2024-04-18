(in-package :shop3-zmq)

(defvar *endpoint* "tcp://*:31888")
(defvar *diag* nil)

(defun ask-shop (request-str)
  "Eval request string in :shop2-user package.
   Return a list of the three elements
    - result of evaluation
    - any output to *standard-output*
    - any output to *err-output*"
  (let ((std-out-str (make-array '(0) :element-type 'base-char
					   :fill-pointer 0 :adjustable t))
	(err-out-str (make-array '(0) :element-type 'base-char
				      :fill-pointer 0 :adjustable t)))
    (handler-case
	(let* ((*package* (find-package :shop3-user))
	       (form (read-from-string request-str))
	       (result (with-output-to-string (*standard-output* std-out-str)
			 (with-output-to-string (*error-output* err-out-str)
			   (eval form)))))
	  (list result :break-here std-out-str :break-here err-out-str))
      (serious-condition (e)
	(log-msg  "Serious condition: ~A. Out string = ~S Error string = ~S" e std-out-str err-out-str)
	:bad-input))))

(defun server-loop (listen-address)
  "Translation of http://zguide.zeromq.org/c:hwserver updated for ZMQ 3. "
  (handler-case
      (pzmq:with-context (ctx :max-sockets 10) ; ctx is a variable not a function!
	(pzmq:with-socket responder :rep
	  (pzmq:bind responder listen-address)
	  ;; Flush messages from prior invocations.
	  ;; recv-string actually returns values; second value is about rcvmore.
	  ;; ToDo: Determine whether this or the proper ctx shutdown fixed things.
	  (dotimes (i 2000)
	    (handler-case
		(progn (log-msg "Flushing ~S" (pzmq:recv-string responder :dontwait t))
		       (sleep 1e-4))
	      (pzmq:eagain ())))
	  (log-msg "Entering server loop. Listening on ~S." listen-address)
	  (loop
	    ;;(log-msg "Waiting for a request... ")
	    (let ((got (pzmq:recv-string responder))) ; See above about more. (This will matter for if I send big defdomains.)
	      (log-msg "Server receives: ~S" got)
	      (if (string-equal got "(sb-ext:exit)")
		  (progn
		    (log-msg "-----Server shutting down on  ~S----" got)
		    (pzmq:send responder ":bye!")
		    (log-msg "Returned bye.")
		    (sleep 0.1)
		    (pzmq:close responder)   ;  See Page 20 in the PDF.
		    (pzmq:ctx-shutdown ctx)
		    (pzmq:ctx-term ctx)      ;  See Page 20 in the PDF.
		    (log-msg "Exiting lisp.")
		    (sb-ext:exit))
		  (progn
		    (let ((result (ask-shop got)))
		      (log-msg "Server replying: ~S" result)
		      (pzmq:send responder (format nil (fmt-msg result) result)))))))))
    (serious-condition (c)
      (log-msg "Serious condition, exiting loop: ~S" c))))

(defun start-server ()
  (if *endpoint*
      (server-loop *endpoint*)
      (log-msg "Endpoint port not set. Not starting.")))

(defun send-server-stop-msg ()
  "Send '(sb-ext:exit)' which means stop. For debugging, I think."
  (log-msg "======Telling the plan server to stop")
  (pzmq:with-context (ctx :max-sockets 10)
    (pzmq:with-socket (socket ctx) :req
      (pzmq:connect socket *endpoint*)
      (pzmq:send socket "(sb-ext:exit)"))))
