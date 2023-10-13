(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort :adopt :cl-csv :str :cl-ppcre) :silent t))

(defpackage :parseemp
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :parseemp)


;; config - if any, goes here
(defparameter *fieldmap* `((:name "Navn" ,#'(lambda (val &rest _) 
                                              (or (str:join #\Space (subseq (str:split #\Space val) 1)) _)))
                           (:born "Fødselsnummer" ,#'(lambda (val &rest _) 
                                                             (or (let* 
                                                                   ((yy (parse-integer (subseq val 4 6)))
                                                                    (YYYY (if (> yy 70) (+ 1900 yy) (+ 2000 yy)))) 
                                                                   (format nil "~a-~a-~a" (subseq val 0 2) (subseq val 2 4) YYYY)) _)))
                           (:adr "Adresse" ,#'(lambda (val &rest _) (or (subseq val 0 (- (length val) (length ", Norge"))) _)))
                           (:email "E-post")
                           (:empl "E-post" ,#'(lambda (val &rest _) (or (car (str:split #\@ val)) _)))
                           (:prst "Stillingsprosent")
                           (:salary "Årslønn" ,#'(lambda (val row) 
                                                        (round (/ (* (parse-integer (val-field "Stillingsprosent" row)) 
                                                                     (parse-integer (subseq val 0 (- (length val) 3)))) 
                                                                  100))))
                           (:start "Ansettelsesdato")))

;; errors
(define-condition user-error (error) ())
(define-condition missing-file (user-error) ()
  (:report "Invalid file specified"))
(define-condition many-files (user-error) ()
  (:report "Only one file should be given"))

;; functionality
(defun row-field (field) 
  (second (assoc field *fieldmap*)))

(defun val-field (field data)
  (second (assoc field data :test #'string=)))

(defun row-trans (field)
  (or (third (assoc field *fieldmap*)) #'(lambda (val &rest _) (or val _))))

(defun get-field (data field)
  (let ((f (row-field field))
        (trans (row-trans field))) 
    (funcall trans (val-field f data) data)))

(defun parse-row-fnc (dumpfields)
  (let ((fields nil)) 
    (lambda (row) 
      (if fields
          ;; zip fields and the row together to get the association between header and value in the row
          (let ((afields (mapcar #'list fields row))) 
            (format t "~{~a~^;~}~%" (loop for f in dumpfields 
                                          collect (get-field afields f))))
          (setf fields row)))))
 
(defun parseemp (fp &optional (sep #\;))
  (cl-csv:read-csv fp :row-fn (parse-row-fnc '(:empl :name :born :email :adr :salary :prst :start)) :separator sep))

;; run
(defun run (strpath)
  (let ((fp (probe-file (uiop:native-namestring strpath)))) 
    (if fp 
        (parseemp fp)
        (error 'missing-file))))

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


(defparameter *ui*
  (adopt:make-interface
    :name "parseemp"
    :usage "[OPTIONS] file"
    :summary "list employees in a defined format, parsed from supplied file"
    :help "Ask for help if you need it."
    :manual "A manual string can go here"
    :contents (list
                *option-help*
                *option-debug*
                *option-no-debug*)))

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
        (format t "~a (len ~a)" args (length args))
        )
      (handler-case 
        (cond 
          ((gethash 'help opts) (adopt:print-help-and-exit *ui*))
          ((null args) (error 'missing-file))
          ((> (length args) 1) (error 'many-files))
          (t (run (car args))))
        (user-error (e) (adopt:print-error-and-exit e))))))
