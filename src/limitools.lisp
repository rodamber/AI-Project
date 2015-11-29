;;;; -*- Mode: Lisp; Base: 10; Syntax: ANSI-Common-Lisp; Lowercase: Yes; Package: (CL-USER) -*-

;;;; Limitools = Tools to limit computation resource usage.
;;;; Limitime = Tools to limit computation time. A part of Limitools.

;;;; (c) Juan Jose Garcia 2007-2014
;;;; juanjo@eurogaran.com
;;;;
;;;; Licence
;;;; =======
;;;;
;;;; This software is provided 'as-is', without any express or implied
;;;; warranty. In no event will the author be held liable for any damages
;;;; arising from the use of this software.
;;;;
;;;; Permission is granted to anyone to use this software for any purpose,
;;;; including commercial applications, and to alter it and redistribute
;;;; it freely, subject to the following restrictions:
;;;;
;;;; 1. The origin of this software must not be misrepresented; you must
;;;;    not claim that you wrote the original software. If you use this
;;;;    software in a product, an acknowledgment in the product documentation
;;;;    would be appreciated but is not required.
;;;;
;;;; 2. Altered source versions must be plainly marked as such, and must
;;;;    not be misrepresented as being the original software.
;;;;
;;;; 3. All altered versions must include source, or make the source
;;;;    easy available on the internet or by any other means when required.
;;;;
;;;; 4. This notice may not be removed or altered from any source distribution,
;;;;    whether modified or not.
;;;;
;;;; 5. Use of the same function names in otherwise unrelated code is allowed
;;;;    and in fact encouraged, as long as function behavior is kept.
;;;;
;;;; Notes
;;;; =====
;;;;
;;;; More recent versions of this software may be available at:
;;;;   http://www.eurogaran.com/downloads/lisp/
;;;;
;;;; Comments, suggestions and bug reports to the author, Juanjo Garcia, at:
;;;; juanjo@eurogaran.com



;;; WITH-MAX-TIME is a wrapper that implements a
;;; functionality essential for programs to test
;;; other programs (possibly written by themselves)
;;; in what is known as 'speculative programming'.

(defmacro with-max-time (secs &body body)
"Abandon processing -except for its UNWIND-PROTECTs- when not finished
before the specified time limit.
Type:    Macro.
Package: (CL-USER)
Arglist:
  Arg 1: SECONDS = non-negative real, or complex with a non-negative real part,
         or some expression that will evaluate to that.
         Execution time is guaranteed not to be greater (but allowed to be
         slightly less) than the real part of the value of the form supplied,
         always being at least a minimum imposed by each implementation's
         scheduler granularity.
  Rest : The form-s to be processed.
Example: (with-max-time (+ 3 2) (loop))  would execute an empty loop during
         at most 5 seconds.
Results: The results of processing, or no values if abandoned.
         Thus, more powerful hardware may change results, but not max. execution
         time, whereas precisely the opposite is normally expected.
  Note : A result of NIL will be indistinguishable from no result when using
         constructs like  (if (with-max-time... ) ...)
         This ambiguity is both deliberate and useful.
         To determine whether evaluation completed or not, use instead
         a construct like (if (multiple-value-list (with-max-time... )) ...)
         that will produce in each case either (NIL) or NIL, which are different
         in Lisp :   NIL is false, whereas (NIL) is in fact true (not NIL).
Side effects:  Does not have any itself.   Beware though :
         If the forms to be processed and possibly interrupted have side effects
         -on globals, locks, etc.-  then overall behavior could become erratic :
         unpredictable results, possible deadlocks...  So USE FUNCTIONAL STYLE.
Apparent bug:   You cannot throw out of WITH-MAX-TIME: Timer can not be aborted
         from inside the body forms. Not even by a BREAK. This is intended."


  ;; FIXME abcl: Time limit usually exceeded regardless of mintime increase.
  ;; The Java Virtual Machine seems to induce latencies.
  #+(or abcl xcl)
  (let ((package (if (eq (symbol-package 'thread) (find-package "EXTENSIONS"))
                   "ext"
                   "threads"))
        (seconds (gensym))
        (valsecs (gensym))
        (results (gensym))
        (complet (gensym))
        (computs (gensym))
        (fintime (gensym))
        (mintime 0.02)) ; min. time period (>= scheduler granularity)
    (declare (type float mintime))

    `(let ((,seconds ,secs))
       (if (not (numberp ,seconds))
         (progn
           (warn ";;; WITH-MAX-TIME: Time arg. not a number.
;;; Leaving indefinite (infinite?).")
           ,@body)
    (let ((,valsecs (truncate (* (- (realpart ,seconds) ,mintime)
                                  internal-time-units-per-second))))
      (declare (type integer ,valsecs))

      (when (minusp ,valsecs)
        (setq ,valsecs 0)
        (warn ";;; WITH-MAX-TIME: Time arg. negative or inferior to min. Assuming min."))

      (let* (,results
             ,complet
             (,fintime (+ ,valsecs (get-internal-real-time)))
             (,computs (funcall (symbol-function (read-from-string (format nil "~A:make-thread" ,package)))
                        #'(lambda (&optional (*standard-input* *standard-input*)
                                             (*terminal-io* *terminal-io*)
                                             (*query-io* *query-io*))
                            (setq ,results (multiple-value-list (progn ,@body))
                                  ,complet t))
                        :name "with-max-time-active-process")))
        (declare (type list ,results)
                 (type boolean ,complet)
                 (type integer ,fintime)
                 (type (read-from-string (format nil "~A:thread" ,package)) ,computs))

        (unwind-protect
             (loop while (and (not ,complet)
                              (< (get-internal-real-time) ,fintime))
                   finally (return (values-list ,results))
                   do (sleep ,mintime))
          (unless ,results
            (ignore-errors (funcall (symbol-function (read-from-string (format nil "~A:destroy-thread" ,package))) ,computs)))))))))

  #+allegro
  (let ((seconds (gensym))
        (valsecs (gensym))
        (results (gensym))
        (computs (gensym))
        (mintime 0.01)) ; min. time period (>= scheduler granularity)
    (declare (type float mintime))

    `(require :process)
    `(let ((,seconds ,secs))
       (if (not (numberp ,seconds))
         (progn
           (warn ";;; WITH-MAX-TIME: Time arg. not a number.
;;; Leaving indefinite (infinite?).")
           ,@body)
    (let* ((,valsecs (- (realpart ,seconds) ,mintime))
            ,results
            (,computs (mp:process-run-function
                       `(:name "with-max-time-active-process"
                         :initial-bindings
                         ((*readtable* . ',*readtable*)
                          ,@*default-cg-bindings*
                          ,@excl:*cl-default-special-bindings*))
                       #'(lambda (*standard-input* *terminal-io* *query-io*)
                           (setq ,results (multiple-value-list (progn ,@body))))
                       *standard-input* *terminal-io* *query-io*)))
      (declare (type list ,results) (type real ,valsecs)
       (type mp:process ,computs))

      (when (minusp ,valsecs)
        (setq ,valsecs 0)
        (warn ";;; WITH-MAX-TIME: Time arg. negative or inferior to min. Assuming min."))

      (unwind-protect
           (progn (mp:process-wait-with-timeout
                   "with-max-time-waiting-process"
                   ,valsecs
                   #'(lambda () (not (mp:process-active-p ,computs))))
                  (values-list ,results))
        (if (mp:process-active-p ,computs)
          (mp:process-kill ,computs :wait nil)))))))

  #+(and clisp mt)
  (let ((seconds (gensym))
        (valsecs (gensym))
        (results (gensym))
        (computs (gensym))
        (complet (gensym))
        (fintime (gensym))
        (mintime 0.02)) ; min. time period (>= scheduler granularity)
    (declare (type float mintime))

    `(let ((,seconds ,secs))
       (if (not (numberp ,seconds))
         (progn
           (warn ";;; WITH-MAX-TIME: Time arg. not a number.
;;; Leaving indefinite (infinite?).")
           ,@body)
    (let ((,valsecs (truncate (* (- (realpart ,seconds) ,mintime)
                                   internal-time-units-per-second))))
      (declare (type integer ,valsecs))
      (when (minusp ,valsecs)
        (setq ,valsecs 0)
        (warn ";;; WITH-MAX-TIME: Time arg. negative or inferior to min. Assuming min."))

      (let* (,results
             ,complet
             (,fintime (+ ,valsecs (get-internal-real-time)))
             (,computs (mt:make-thread
                        #'(lambda ()
                            (setq ,results (multiple-value-list (progn ,@body))
                                  ,complet t))
                        ;;:initial-bindings (list  (*standard-input* . *standard-input*)
                        ;;                         (*terminal-io* . *terminal-io*)
                        ;;                         (*query-io* . *query-io*))
                        :name "with-max-time-active-process")))
        (declare (type list ,results)
                 (type boolean ,complet)
                 (type integer ,fintime)
                 (type mt:thread ,computs))

        (unwind-protect
             (loop while (and (not ,complet)
                              (< (get-internal-real-time) ,fintime))
                   finally (return (values-list ,results))
                   do (sleep ,mintime))
          (unless ,complet
            ;; :override is the whole point of using WITH-MAX-TIME, though CLOS
            ;; and many other things are not thread-safe at the time in CLISP
            (ignore-errors (mt:thread-interrupt ,computs :function t :override t)))))))))

  #+cormanlisp
  (let ((seconds (gensym))
        (valsecs (gensym))
        (results (gensym))
        (complet (gensym))
        (computs (gensym))
        (fintime (gensym))
        (mintime 0.02)) ; min. time period (>= scheduler granularity)
    (declare (type float mintime))

    `(require 'THREADS)
    `(let ((,seconds ,secs))
       (if (not (numberp ,seconds))
         (progn
           (warn ";;; WITH-MAX-TIME: Time arg. not a number.
;;; Leaving indefinite (infinite?).")
           ,@body)
    (let* ((,valsecs (truncate (* (- (realpart ,seconds) ,mintime)
                                   internal-time-units-per-second))))
      (declare (type integer ,valsecs))

      (when (minusp ,valsecs)
        (setq ,valsecs 0)
        (warn ";;; WITH-MAX-TIME: Time arg. negative or inferior to min. Assuming min."))

      (let* (,results
             ,complet
             (,fintime (+ ,valsecs (get-internal-real-time)))
             (,computs (th:create-thread
                        #'(lambda (&optional (*standard-input* *standard-input*)
                                             (*terminal-io* *terminal-io*)
                                             (*query-io* *query-io*))
                            (setq ,results (multiple-value-list (progn ,@body))
                                  ,complet t))
                        :report-when-finished nil)))
        (declare (type list ,results)
                 (type boolean ,complet)
                 (type integer ,fintime))

        (unwind-protect
             (loop while (and (not ,complet)
                              (< (get-internal-real-time) ,fintime))
                   finally (return (values-list ,results))
                   do (sleep ,mintime))
          (unless ,results
            (ignore-errors (th:terminate-thread ,computs)))))))))

  #+(and ecl threads)
  (let ((seconds (gensym))
        (valsecs (gensym))
        (results (gensym))
        (computs (gensym))
        (fintime (gensym))
        (mintime 0.02)) ; min. time period (>= scheduler granularity)
    (declare (type float mintime))

    `(let ((,seconds ,secs))
       (if (not (numberp ,seconds))
         (progn
           (warn ";;; WITH-MAX-TIME: Time arg. not a number.
;;; Leaving indefinite (infinite?).")
           ,@body)
    (let ((,valsecs (truncate (* (- (realpart ,seconds) ,mintime)
                                  internal-time-units-per-second))))
      (declare (type integer ,valsecs))
      (when (minusp ,valsecs)
        (setq ,valsecs 0)
        (warn ";;; WITH-MAX-TIME: Time arg. negative or inferior to min. Assuming min."))
      (let* (,results
             (,fintime (+ ,valsecs (get-internal-real-time)))
             (,computs (mp:process-run-function
                        'with-max-time-active-process
                        #'(lambda (*standard-input* *terminal-io* *query-io*)
                            (setq ,results (multiple-value-list (progn ,@body))))
                        *standard-input* *terminal-io* *query-io*)))
        (declare (type list ,results)
                 (type integer ,fintime)
                 (type mp:process ,computs))

        (unless (mp:process-active-p ,computs)
          (mp:process-enable ,computs))

        (unwind-protect
             (do () ((or (not (mp:process-active-p ,computs))
                         (>= (get-internal-real-time) ,fintime))
                     (values-list ,results))
               (sleep ,mintime))
          (if (mp:process-active-p ,computs)
            (mp:interrupt-process ,computs 'mp:exit-process))))))))

  #+genera
  (let ((seconds (gensym))
        (valsecs (gensym))
        (results (gensym))
        (complet (gensym))
        (computs (gensym)))
    `(let ((,seconds ,secs))
       (if (not (numberp ,seconds))
         (progn
           (warn ";;; WITH-MAX-TIME: Time arg. not a number.
;;; Leaving indefinite (infinite?).")
           ,@body)
    (let* ((,valsecs (realpart ,seconds))
            ,results
            ,complet
            (,computs (process:process-run-function
                       "with-max-time-active-process"
                       #'(lambda (*standard-input* *terminal-io* *query-io*)
                           (setq ,results (multiple-value-list (progn ,@body))
                                 ,complet t))
                       *standard-input* *terminal-io* *query-io*)))
      (declare (type list ,results) (type boolean ,complet)
       (type real ,valsecs) (type process ,computs))

      (when (minusp ,valsecs)
        (setq ,valsecs 0)
        (warn ";;; WITH-MAX-TIME: Time arg. negative or inferior to min. Assuming min."))

      (unwind-protect
           (progn (process:process-wait-with-timeout
                   "with-max-time-waiting-process"
                   ,valsecs
                   #'(lambda () ,complet))
                  (values-list ,results))
        (process:process-kill ,computs))))))

  #+lispworks
  (let ((seconds (gensym))
        (valsecs (gensym))
        (results (gensym))
        (complet (gensym))
        (computs (gensym))
        (mintime 0.09)) ; min. time period (>= scheduler granularity)
    (declare (type float mintime))

    `(let ((,seconds ,secs))
       (if (not (numberp ,seconds))
         (progn
           (warn ";;; WITH-MAX-TIME: Time arg. not a number.
;;; Leaving indefinite (infinite?).")
           ,@body)
    (let* ((,valsecs (- (realpart ,seconds) ,mintime))
            ,results
            ,complet
            (,computs (mp:process-run-function
                       "with-max-time-active-process"
                       '()
                       #'(lambda (*standard-input* *terminal-io* *query-io*)
                           (setq ,results (multiple-value-list (progn ,@body))
                                 ,complet t))
                       *standard-input* *terminal-io* *query-io*)))
      (declare (type list ,results) (type boolean ,complet)
       (type real ,valsecs) (type mp:process ,computs))

      (when (minusp ,valsecs)
        (setq ,valsecs 0)
        (warn ";;; WITH-MAX-TIME: Time arg. negative or inferior to min. Assuming min."))

      (unwind-protect
           (progn (mp:process-wait-with-timeout
                   "with-max-time-waiting-process"
                   ,valsecs
                   #'(lambda () ,complet))
                  (values-list ,results))
        (mp:process-kill ,computs))))))

  #+(and lucid multitasking)
  (let ((seconds (gensym))
        (valsecs (gensym))
        (results (gensym))
        (complet (gensym))
        (computs (gensym))
        (mintime 0.09)) ; min. time period (>= scheduler granularity)
    (declare (type float mintime))

    `(let ((,seconds ,secs))
       (if (not (numberp ,seconds))
         (progn
           (warn ";;; WITH-MAX-TIME: Time arg. not a number.
;;; Leaving indefinite (infinite?).")
           ,@body)
    (let* ((,valsecs (- (realpart ,seconds) ,mintime))
            ,results
            ,complet
            (,computs (lcl:make-process
                       :name
                       "with-max-time-active-process"
                       :function
                       #'(lambda (*standard-input* *terminal-io* *query-io*)
                           (setq ,results (multiple-value-list (progn ,@body))
                                 ,complet t))
                       :args
                       '(*standard-input* *terminal-io* *query-io*))))
      (declare (type list ,results) (type boolean ,complet)
       (type real ,valsecs) (type lcl:process ,computs))

      (when (minusp ,valsecs)
        (setq ,valsecs 0)
        (warn ";;; WITH-MAX-TIME: Time arg. negative or inferior to min. Assuming min."))

      (unwind-protect
           (progn (lcl:process-wait-with-timeout
                   "with-max-time-waiting-process"
                   ,valsecs
                   #'(lambda () ,complet))
                  (values-list ,results))
        (lcl:process-kill ,computs))))))

  ;; FIXME rmcl: Time limit frequently exceeded regardless of mintime increase.
  ;; (Rosetta emulation seems to induce latencies.)
  #+(and mcl processes (not openmcl-native-threads))
  (let ((seconds (gensym))
        (valsecs (gensym))
        (results (gensym))
        (complet (gensym))
        (computs (gensym))
        (mintime 0.04)) ; min. time period (>= scheduler granularity)
    (declare (type float mintime))

    `(let ((,seconds ,secs))
       (if (not (numberp ,seconds))
         (progn
           (warn ";;; WITH-MAX-TIME: Time arg. not a number.
;;; Leaving indefinite (infinite?).")
           ,@body)
    (let* ((,valsecs (truncate (* 60 (- (realpart ,seconds) ,mintime))))
            ,results
            ,complet
            (,computs (ccl:process-run-function
                       "with-max-time-active-process"
                       #'(lambda (*standard-input* *terminal-io* *query-io*)
                           (setq ,results (multiple-value-list (progn ,@body))
                                 ,complet t))
                       *standard-input* *terminal-io* *query-io*)))
      (declare (type list ,results) (type boolean ,complet)
       (type integer valsecs) (type ccl::process ,computs))

      (when (minusp ,valsecs)
        (setq ,valsecs 0)
        (warn ";;; WITH-MAX-TIME: Time arg. negative or inferior to min. Assuming min."))

      (unwind-protect
           (progn (ccl:process-wait-with-timeout
                   "with-max-time-waiting-process"
                   ,valsecs
                   #'(lambda () ,complet))
                  (values-list ,results))
        (ccl:process-kill ,computs))))))

  #+openmcl-native-threads
  (let ((seconds (gensym))
        (valsecs (gensym))
        (results (gensym))
        (sem (gensym))
        (computs (gensym))
        (mintime 0.02)) ; min. time period (>= scheduler granularity)
    (declare (type float mintime))

    `(let ((,seconds ,secs))
       (if (not (numberp ,seconds))
         (progn
           (warn ";;; WITH-MAX-TIME: Time arg. not a number.
;;; Leaving indefinite (infinite?).")
           ,@body)
    (let* ((,valsecs (- (realpart ,seconds) ,mintime))
            ,results
            (,sem (make-semaphore))
            (,computs (ccl:process-run-function
                       "with-max-time-active-process"
                       #'(lambda (*standard-input* *terminal-io* *query-io*)
                           (setq ,results (multiple-value-list (progn ,@body)))
                           (signal-semaphore ,sem))
                       *standard-input* *terminal-io* *query-io*)))
      (declare (type real ,valsecs) (type list ,results)
       (type ccl:semaphore ,sem) (type ccl:process ,computs))

      (when (minusp ,valsecs)
        (setq ,valsecs 0)
        (warn ";;; WITH-MAX-TIME: Time arg. negative or inferior to min. Assuming min."))

      (unwind-protect
           (progn (ccl:timed-wait-on-semaphore ,sem ,valsecs)
                  (values-list ,results))
        (ccl:process-kill ,computs))))))

  #+sb-thread
  (let ((seconds (gensym))
        (valsecs (gensym))
        (results (gensym))
        (computs (gensym))
        (fintime (gensym))
        (mintime 0.02)) ; min. time period (>= scheduler granularity)
    (declare (type float mintime))

    `(let ((,seconds ,secs))
       (if (not (numberp ,seconds))
         (progn
           (warn ";;; WITH-MAX-TIME: Time arg. not a number.
;;; Leaving indefinite (infinite?).")
           ,@body)
    (let ((,valsecs (truncate (* (- (realpart ,seconds) ,mintime)
                                   internal-time-units-per-second))))
      (declare (type integer ,valsecs))
      (when (minusp ,valsecs)
        (setq ,valsecs 0)
        (warn ";;; WITH-MAX-TIME: Time arg. negative or inferior to min. Assuming min."))
      (let* (,results
             (,fintime (+ ,valsecs (get-internal-real-time)))
             (,computs (sb-thread:make-thread
                        #'(lambda () (setq ,results (multiple-value-list
                                                     (progn ,@body)))))))
        (declare (type list ,results)
                 (type integer ,fintime)
                 (type sb-thread:thread ,computs)
                 (sb-ext:muffle-conditions sb-ext:compiler-note))

        (unwind-protect
             (loop while (and (sb-thread:thread-alive-p ,computs)
                              (< (get-internal-real-time) ,fintime))
                   finally (return (values-list ,results))
                   do (sleep ,mintime))
          (if (sb-thread:thread-alive-p ,computs)
            (sb-thread:terminate-thread ,computs))))))))

  #+(and scl pthread)
  (let ((seconds (gensym))
        (valsecs (gensym))
        (results (gensym))
        (computs (gensym))
        (complet (gensym))
        (fintime (gensym))
        (mintime 0.005)) ; min. time period (>= scheduler granularity)
    (declare (type float mintime))

    `(let ((,seconds ,secs))
       (if (not (numberp ,seconds))
         (progn
           (warn ";;; WITH-MAX-TIME: Time arg. not a number.
;;; Leaving indefinite (infinite?).")
           ,@body)
    (let ((,valsecs (truncate (* (- (realpart ,seconds) ,mintime)
                                   internal-time-units-per-second))))
      (declare (type integer ,valsecs))
      (when (minusp ,valsecs)
        (setq ,valsecs 0)
        (warn ";;; WITH-MAX-TIME: Time arg. negative or inferior to min. Assuming min."))
      (let* (,results
             ,complet
             (,fintime (+ ,valsecs (get-internal-real-time)))
             (,computs (thread:thread-create
                        #'(lambda ()
                            (setq ,results (multiple-value-list (progn ,@body))
                                  ,complet t))
                        :name "with-max-time-active-process"
                        :background-streams-p nil)))
        (declare (type list ,results)
                 (type boolean ,complet)
                 (type integer ,fintime)
                 (type thread:thread ,computs))

        (unwind-protect
             (loop while (and (not ,complet)
                              (< (get-internal-real-time) ,fintime))
                   finally (return (values-list ,results))
                   do (sleep ,mintime))
          (unless ,complet
            (ignore-errors (thread:destroy-thread ,computs)))))))))

  #-(or abcl xcl allegro (and clisp mt) cormanlisp (and ecl threads)
        genera (and lucid multitasking) lispworks (and mcl processes)
        openmcl-native-threads sb-thread (and scl pthread))
  (error ";;; WITH-MAX-TIME timeout not implemented for this Lisp.
It possibly lacks threads or multiprocessing."))


;;;  WITH-MIN-TIME is the obvious companion to with-max-time.
;;;  May seem stupid, but beware an adequate use of
;;;  with-min-time in caller threads or functions and
;;;  with-max-time in the callees avoids race conditions.
;;;  (Not recommended: Prefer to synchronize using locks, semaphores, etc.)

(defmacro with-min-time (secs &body body)
  "Delays delivery of results until at least the specified number of seconds.
Type :   Macro.
Package: (CL-USER)
Arguments:
  Arg 1: SECONDS = non-negative real, or complex with a non-negative real part,
         or some expression that evaluates to that.
         Execution time is guaranteed not to be any shorter,
         though it could be slightly greater than the real part of the value
         of the form supplied, always being at least 0.
  Rest : Forms to compute.
Example: (with-min-time (+ 3 2) (+ 3 4))  would deliver the result 7 after
         at least 5 seconds.
Results: The results of computing the forms.
Side effects:  Should not leave any -- just wasted time."

  (let ((seconds (gensym))
        (valsecs (gensym))
        (elapsed (gensym))
        (results (gensym)))
    `(let ((,seconds ,secs))
       (if (not (numberp ,seconds))
         (progn
           (warn ";;; WITH-MIN-TIME: Time arg. not a number. Assuming 0.")
           ,@body)
     (let* ((,valsecs (realpart ,seconds))
            (,elapsed (get-internal-real-time))
            (,results (multiple-value-list (progn ,@body))))
      (declare (type real ,valsecs ,elapsed) (type list ,results))

      (when (minusp ,valsecs)
        (setq ,valsecs 0)
        (warn ";;; WITH-MIN-TIME: Time arg. negative or inferior to min. Assuming 0."))

      (setq ,elapsed (/
                      (- (get-internal-real-time) ,elapsed)
                      internal-time-units-per-second))
      (if (< ,elapsed ,valsecs)
        (sleep (- ,valsecs ,elapsed)))

      (values-list ,results))))))


;;; WITH-TIMEOUT is a lisp machine legacy with a badly designed interface.
;;; Provided here as an exercise, and for compatibility :
;;; DO NOT USE IN NEW CODE !
#|| Example extracted from the MIT lispm manual:
  (with-timeout (300 (format *query-io* "...Yes") t) (y-or-n-p "Really do it? (Yes after five seconds) "))
  is a convenient way to ask a question and assume an answer if the user
  does not respond promptly. This is a good thing to do for queries
  likely to occur when the user has walked away from the terminal
  and expects an operation to finish without his attention.
||#
;;;(unless (or (functionp #'with-timeout) (macro-function 'with-timeout))
(unless (and (symbolp 'with-timeout) (fboundp 'with-timeout))
  ;; that is, if not yet defined inside the current environment :
  (defmacro with-timeout (timeout-and-else-forms &body body)
; This documentation string copied from the TI Explorer lispm:
"Execute BODY with a timeout set for DURATION 60'ths of a second from time of entry.
If the timeout elapses while BODY is still in progress,
the TIMEOUT-FORMS are executed and their values returned, and
whatever is left of BODY is not done, except for its UNWIND-PROTECTs.
If BODY returns, its values are returned and the timeout is cancelled."
; The TI Explorer (TM) documentation string continues:
;"The timeout is also cancelled if BODY throws out of the WITH-TIMEOUT."
; ... which is not desirable and I think is not possible in this threaded implementation.
    (let ((seconds (/ (eval (car timeout-and-else-forms)) 60.))
          (else-forms (cdr timeout-and-else-forms))
          (results (gensym)))
      `(let ((,results (multiple-value-list (with-max-time ,seconds ,@body))))
        (if ,results
          (values-list ,results)
          (progn ,@else-forms))))))

;------------------------------------------------------------------------------
