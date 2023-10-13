(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort :adopt :str :cl-ppcre :ttcon) :silent t))

(defpackage :projtt
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :projtt)

;; config - if any, goes here

;; errors
(define-condition user-error (error) ())

;; functionality

(defun sortprojs (nosrt lst)
  (if nosrt
      lst
      (sort lst #'string<= :key #'(lambda (proj) (ttu:alis-val proj :name)))))

(defun filter_external (projs)
  (loop for proj in projs
        when (not (ttu:alis-val proj :is-internal))
        collect proj))

(defun projtt (nosrt &key external)
  (let* ((projs (sortprojs nosrt (ttu:alis-val (ttt:list-active-projects (ttconf:config)) :values)))
         (projs (cond (external (filter_external projs))
                              (t projs))))
    (loop for proj in projs
          do
          (format t "[~3d] (~s): [~s] ~s~%"
                  (parse-integer (ttu:alis-val proj :number))
                  (if (ttu:alis-val proj :is-internal) "U" "F")
                  (ttu:alis-val proj :customer-name)
                  (ttu:alis-val proj :name)
                  ))))

;; run
(defun run (nosrt &key external)
  (progn
    (ttconf:setenv :environment :prod)
    (projtt nosrt :external external)))

;; user interface
(defparameter *option-help*
  (adopt:make-option 'help
                     :help "Display help and exit."
                     :long "help"
                     :short #\h
                     :reduce (constantly t)))

(adopt:defparameters (*option-debug* *option-no-debug*)
                     (adopt:make-boolean-options 'debug
                                                 :long "debug"
                                                 :short #\d
                                                 :help "Enable the lisp debugger."
                                                 :help-no "Disable the lisp debugger (default)."))

(adopt:defparameters (*option-sort* *option-no-sort*)
                     (adopt:make-boolean-options 'nosrt
                                                 :long "nosortproj"
                                                 :short #\s
                                                 :help "Disable sort of projects by name."
                                                 :help-no "Sort projects by name (default)."))

(adopt:defparameters (*option-ext* *option-all*)
                     (adopt:make-boolean-options 'ext
                                                 :long "externalproj"
                                                 :short #\f
                                                 :help "Include only external invoiceable projects."
                                                 :help-no "Include all projects (default)."))


(defparameter *ui*
  (adopt:make-interface
    :name "projtt"
    :usage "[OPTIONS]"
    :summary "list active projects in a defined format, based on data from tripletex"
    :help "Ask for help if you need it."
    :manual "For now, not much of a manual here"
    :contents (list
                *option-help*
                *option-debug* *option-no-debug*
                *option-sort* *option-no-sort*
                *option-ext* *option-all*
                )))

(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (sb-ext:exit :code 130))))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (args opts) (adopt:parse-options-or-exit *ui*)
      ;; handle options here
      (when (gethash 'debug opts)
        (sb-ext:enable-debugger)
        (format t "~a (len ~a)" args (length args)))
      (handler-case
        (cond
          ((gethash 'help opts) (adopt:print-help-and-exit *ui*))
          (t (run (gethash 'nosrt opts) :external (gethash 'ext opts))))
        (user-error (e) (adopt:print-error-and-exit e))))))
