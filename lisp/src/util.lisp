(in-package :shop3-zmq)

(defmacro when-let ((var expr) &body body)
  "Paul Graham 'On LISP' pg 145. when+let"
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defun fmt-msg (msg)
  "Return a format string for a message"
  (cond ((stringp msg) "~S")
	((keywordp msg) "~S")
	(t "~A")))

;;; Don't break this up!
(defparameter *lt-format* '(:SHORT-WEEKDAY #\  :SHORT-MONTH #\  (:DAY 2 #\ ) #\  (:YEAR 4) #\  (:HOUR 2) #\:
			    (:MIN 2) #\: (:SEC 2) #\. (:USEC 6)))

(defun log-msg (fmt &rest args)
  (let ((msg (apply #'format nil fmt args)))
    (format t "~%~A" msg)
    (with-open-file (out "/tmp/plan-log.txt"
			 :if-does-not-exist :create
			 :direction :output :if-exists :append)
      (format out "~%~A: ~A"
	      (local-time:format-timestring nil (local-time:now) :format *lt-format*)
	      msg))))

(defun find-thread (name)
  (loop for thread in (bt:all-threads)
	do (when (string-equal name (bt:thread-name thread))
	     (return thread))))
