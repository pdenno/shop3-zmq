(in-package :shop3-zmq) ;; First you have to do the load below...

;;; I use this with slime with the REPL (in-package :shop3-user).
;;; I evaluate these with C-c c one at a time. REPL is F7.
(load "~/Documents/git/shop3-zmq/load-shop-interactive.lisp")

;;; ===================== To get started =================================
(defdomain basic-example (
     (:operator (!pickup ?a) () () ((have ?a)))
     (:operator (!drop ?a) ((have ?a)) ((have ?a)) ())
     (:method (swap ?x ?y)
       ((have ?x))
       ((!drop ?x) (!pickup ?y))
       ((have ?y))
       ((!drop ?y) (!pickup ?x)))))

(defproblem problem1 basic-example
  ((have banjo)) ((swap banjo kiwi)))

(find-plans 'problem1 :verbose :plans)

;;; ===================== More realistic ==================================
;;; This demonstrates pausing planning

(defdomain process-interview
    ((:method (characterize-process ?proj)
       ORDINARY
       ((plan-state started))
       ((!start-interview ?proj)
	(get-process-steps ?proj)
	(!tbd-wait-response ?proj)))      ; This will add

     (:method (get-process-steps ?proj)
       WELL-KNOWN ((well-known-process ?proj))
		  ((!yes-no-process-steps ?proj))

       UNKNOWN    ((unknown-process ?proj))
		  ((!query-unknown-process-steps ?proj)))

     (:operator (!start-interview ?proj)
		((plan-state started))
		()
		((initial-question ?proj)))

     (:operator (!yes-no-process-steps ?proj)
		((proj-name ?proj) (well-known-process ?proj))
		()
		((have-process-steps ?proj)))

     (:operator (!query-unknown-process-steps ?proj)
		((proj-name ?proj))
		()
		((have-process-steps ?proj)))

     (:operator (!tbd-wait-response ?proj)
		((proj-name ?proj))
		((plan-state started))
		((plan-state stopped)))

     (:method (print-current-state)
       ((eval (print-current-state))) ())

     (:method (print-current-tasks)
       ((eval (print-current-tasks))) ())

     (:method (print-current-plan)
       ((eval (print-current-plan))) ())))

;;; You have to re-evaluate this every time you change the plan
(defproblem test-problem
  ;; Planning domain
  process-interview
  ;; States
  ((plan-state started)
   (proj-name my-project)
   (well-known-process my-project))
  ;; Goals
  ((characterize-process my-project)))

;;;(find-plans 'test-problem :verbose :long-plans :which :all :state t)


(shop-trace :tasks)
(shop-trace :methods)
(shop-trace :goals)
(shop-trace)    ; Tells you what is being trace
(shop-untrace)  ; Can also take arguments
(find-plans 'test-problem :verbose :long-plans :which :all :explanation nil)
(find-plans 'test-problem :verbose :long-plans :which :all :explanation t)


;;; When you have a problem that does not solve as expected, the following general recipe may help you home in on bugs in your domain definition:
;;;
;;; 1. Start by doing (SHOP-TRACE :TASKS) and then try FIND-PLANS again.
;;; 2. In many cases, the domain will be written so that there will be little or no backtracking.
;;;    In this case, examine the output of the traced call to FIND-PLANS and look for the first backtracking point.
;;; 3. The above process should help you identify a particular task, either a primitive or a complex task, as a likely problem spot.
;;;    If it’s a primitive task, the next step is to examine the operator definition.
;;;    If it’s a complex task, you should check the method definitions.
;;;    If you have any trouble identifying which method definition is relevant, you can use (SHOP-TRACE :METHODS) to further focus your attention.
;;; 4. If visual inspection of method and operator definitions does not reveal the problem, you most likely have problems with precondition expressions.
;;;    In this case, try using (SHOP-TRACE :GOALS), rerunning FIND-PLANS and check to see what’s happened when your problem method or operator’s preconditions are checked.
;;;
;;; This recipe has proven effective for finding the vast majority of bugs in SHOP2 domains.”


;;;================================================================================
;;; Now with the plan-server interface
;;;================================================================================
(start-server)

;(shop3-zmq::send-server-stop-msg)

(ask-shop
"(defdomain basic-example (
     (:operator (!pickup ?a) () () ((have ?a)))
     (:operator (!drop ?a) ((have ?a)) ((have ?a)) ())
     (:method (swap ?x ?y)
       ((have ?x))
       ((!drop ?x) (!pickup ?y))
       ((have ?y))
       ((!drop ?y) (!pickup ?x)))))")

(shop3-zmq::ask-shop
"(defproblem problem1 basic-example
  ((have banjo)) ((swap banjo kiwi)))")

(shop3-zmq::ask-shop
 "(find-plans 'problem1 :verbose :plans)")
