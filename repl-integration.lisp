(defpackage :qt-repl
  (:use :cl :qt)
  (:export #:start-gui-thread
           #:eval-in-gui-thread))

(in-package :qt-repl)

(named-readtables:in-readtable :qt)

(defvar *qapp*)
(defvar *notifier*)
(defvar *gui-thread*)
(defvar *executer*)
(defvar *in-gui-thread* nil)

(defclass repl-notifier ()
  ((pending-form :accessor pending-form)
   (emacs-connection :accessor emacs-connection)
   (form-result :accessor form-result)
   (old-package :accessor old-package)
   (new-package :accessor new-package))
  (:metaclass qt-class)
  (:qt-superclass "QObject")
  (:signals ("formReady()")))

(defmethod initialize-instance :after ((repl-notifier repl-notifier)
                                       &key &allow-other-keys)
  (new repl-notifier))

#+swank
(defvar *old-interrupt-worker-thread*
  (fdefinition 'swank::interrupt-worker-thread))

#+swank
(defun swank::interrupt-worker-thread (&rest args)
  "When Swank tries to interrupt repl-thread, and repl-thread
is using Qt signal (emit ...) and is currently waiting for
gui thread to finish, interrupt the gui thread instead where
the actual form is being evaluated"
  ;; old swank args are (id) and newer swank args are (connection id)
  (if (and (member :repl-thread args)
           (boundp '*in-gui-thread*)
           *in-gui-thread*)
      (bt:interrupt-thread *gui-thread*
                           (lambda () (error "Interrupt from Emacs")))
      (apply *old-interrupt-worker-thread* args)))

(defun notifier-do-eval (notifier)
  (flet ((doit ()
           (let ((*package* (old-package notifier)))
             (setf (form-result notifier)
                   (multiple-value-list
                    #+swank (swank::with-connection ((emacs-connection notifier)) 
                              (eval (pending-form notifier)))
                    #-swank (eval (pending-form notifier)))
                   (new-package notifier) *package*))))
    #-swank
    (doit)
    #+swank
    (let ((swank:*sldb-quit-restart* (find-restart 'abort)))
      (doit))))

(defclass repl-executer ()
  ((notifier :reader notifier :initarg :notifier))
  (:metaclass qt-class)
  (:qt-superclass "QObject")
  (:slots ("handleForm()" (lambda (this)
                            (notifier-do-eval (notifier this))))))

(defmethod initialize-instance :after ((repl-executer repl-executer)
                                       &key notifier &allow-other-keys)
  (assert notifier () "must specify notifier")
  (new repl-executer)
  (#_connect "QObject"
             notifier (QSIGNAL "formReady()")
             repl-executer (QSLOT "handleForm()")
             (#_BlockingQueuedConnection "Qt")))

(defun %eval-in-gui-thread (notifier form)
  (unwind-protect
       (progn
         (setf (pending-form notifier) form
               (old-package notifier) *package*)
         #+swank (setf (emacs-connection notifier) swank::*emacs-connection*)
         ;; We use global rather then per-thread binding, because
         ;; slime interrupt is not called from REPL thread but rather
         ;; from control thread
         (unwind-protect
             (progn (setf *in-gui-thread* t)
                    (emit-signal notifier "formReady()"))
           (setf *in-gui-thread* nil))
         (cond ((slot-boundp notifier 'form-result)
                (setf *package* (new-package notifier))
                (apply #'values (form-result notifier)))
               (t
                ;; FIXME: this is perhaps poor substitute
                (format *debug-io* ";; Evaluation aborted")
                (values))))
    (slot-makunbound notifier 'pending-form)
    (slot-makunbound notifier 'form-result)))

(defmacro eval-in-gui-thread (&body body)
  `(%eval-in-gui-thread *notifier*
                        ',(if (rest body)
                              `(progn ,@body)
                              (first body))))

;; print settings sometimes becomes skewed in the new thread

(defparameter *globals*
  '(*debug-io* *query-io* *terminal-io* *standard-output*
    *standard-input* *error-output* *trace-output*
    *print-array* *print-base* *print-radix*
    *print-case* *print-circle* *print-escape*
    *print-gensym* *print-level* *print-length*
    *print-lines* *print-miser-width* *print-pretty*
    *print-readably* *print-right-margin*
    *package*))

(defun start-gui-thread (&optional (install-repl-hook t) (expander #'identity))
  (unless (boundp '*gui-thread*)
    (ensure-smoke :qtcore)
    (ensure-smoke :qtgui)
    (setf *notifier* (make-instance 'repl-notifier))
    (setf 
     *gui-thread*
     (let ((global-values (mapcar #'symbol-value *globals*))
           (hooks (find-symbol "*SLIME-REPL-EVAL-HOOKS*" "SWANK")))
       (labels
           ((hook (form)
              (%eval-in-gui-thread *notifier* (funcall expander form)))
            (setup-and-exec ()
              (unwind-protect
                   (progn
                     (loop for var in *globals*
                           for value in global-values
                           do (setf (symbol-value var) value))
                     (when (and install-repl-hook (find-package "SWANK"))
                       (if hooks
                           (push #'hook (symbol-value hooks))
                           (warn "Cannot initialize *SLIME-REPL-EVAL-HOOKS*, use (eval-in-gui-thread ...) form.")))
                     (setf *qapp* (make-qapplication)
                           *executer* (make-instance 'repl-executer
                                       :notifier *notifier*))
                     (#_setQuitOnLastWindowClosed *qapp* nil)
                     (loop
                       #+swank
                       (let ((swank:*sldb-quit-restart* 'abort))
                         (#_exec *qapp*))
                       #-swank
                       (#_exec *qapp*)))
                (warn "QT Gui thread exited")
                (if hooks
                    (set hooks (delete #'hook (symbol-value hooks))))
                (makunbound '*gui-thread*)
                (#_delete *executer*)
                (#_delete *notifier*)
                (makunbound '*executer*)
                (makunbound '*notifier*))))
         #+(and darwin sbcl)
         (let ((initial-thread (find "initial thread"
                                     (bt:all-threads)
                                     :test #'string-equal
                                     :key #'bt:thread-name)))
           (if (eq initial-thread (bt:current-thread))
               (setup-and-exec)
               (bt:interrupt-thread initial-thread #'setup-and-exec))
           initial-thread)
         #+(and darwin (not sbcl))
         (error "sorry, don't know how to find the initial thread. FIXME.")
         #-darwin
         (bt:make-thread #'setup-and-exec :name "qt-repl-thread"))))))
