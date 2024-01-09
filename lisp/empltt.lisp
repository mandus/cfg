(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort :adopt :str :cl-ppcre :ttcon) :silent t))

(defpackage :empltt
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :empltt)


;; config - if any, goes here

;; errors
(define-condition user-error (error) ())


;; functionality

(defun sortemps (nosrtp lst &key (field :last-name))
  (if nosrtp
      lst
      (sort lst #'string<= :key #'(lambda (emp) (ttu:alis-val emp field)))))


(defun emps_with_startday (emps conf)
  (loop for emp in emps
        collect
        (acons :start-date (ttt:get-employee-start-date emp conf) emp)))

(defun empltt (withextra nosrtp sort-field)
  (let* ((conf (ttconf:config))
         (emps (ttu:alis-val (ttt:get-active-employees conf) :values))
         (emps (if withextra (emps_with_startday emps conf) emps))
         (activeemps (sortemps nosrtp emps :field sort-field)))
    (loop for emp in activeemps
          for cnt = 1 then (+ 1 cnt)
          do
          (format t "[~2d] anr:~3A ~A, ~A [~A] (+47 ~A) ~@[(started ~A) born ~A pnr ~A~] ~%"
                  cnt
                  (ttu:alis-val emp :employee-number)
                  ;(ttu:alis-val emp :id)
                  (ttu:alis-val emp :last-name)
                  (ttu:alis-val emp :first-name)
                  (ttu:alis-val emp :email)
                  (ttu:av emp :phone-number-mobile)
                  (when withextra
                    (ttu:alis-val emp :start-date))
                  (when withextra
                    (ttu:alis-val emp :date-of-birth))
                  (when withextra
                    (ttu:av emp :national-identity-number))
                  ))))

;; run
(defun run (withextra nosrt &key (sort-field :last-name))
  (progn
    (ttconf:setenv :environment :prod)
    (empltt withextra nosrt sort-field)))

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

(adopt:defparameters (*option-extra* *option-no-extra*)
                     (adopt:make-boolean-options 'extra
                                                 :long "extrainfo"
                                                 :short #\x
                                                 :help "Include start date and date of birth."
                                                 :help-no "Disable extra dates in output (default)."))

(adopt:defparameters (*option-sort-sd* *option-no-sort-sd*)
                     (adopt:make-boolean-options 'srtsd
                                                 :long "sortstartdate"
                                                 :short #\t
                                                 :help "Sort on start-date, but only if extra info is enabled."
                                                 :help-no "Disable sort on start-date (default)."))

(adopt:defparameters (*option-sort* *option-no-sort*)
                     (adopt:make-boolean-options 'nosrt
                                                 :long "nosortemps"
                                                 :short #\s
                                                 :help "Disable sort of employees by last-name."
                                                 :help-no "Sort employees by last-name (default)."))


(defparameter *ui*
  (adopt:make-interface
    :name "empltt"
    :usage "[OPTIONS]"
    :summary "list employees in a defined format, based on data from tripletex"
    :help "Ask for help if you need it."
    :manual "For now, not much of a manual here"
    :contents (list
                *option-help*
                *option-debug* *option-no-debug*
                *option-extra* *option-no-extra*
                *option-sort* *option-no-sort*
                *option-sort-sd* *option-no-sort-sd*
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
          (t (run
               (gethash 'extra opts)
               (gethash 'nosrt opts)
               :sort-field (if (and (gethash 'extra opts) (gethash 'srtsd opts))
                               :start-date
                               :last-name))))
        (user-error (e) (adopt:print-error-and-exit e))))))
