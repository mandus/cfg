(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort :adopt :str :parse-float :ttcon) :silent t))

(defpackage :tthours
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :tthours)

;;; remove when happy
(declaim (optimize (speed 3) (safety 3) (debug 0)))

;; errors
(define-condition user-error (error) ())

;; functionality
(defun recent-proj (conf)
  (let ((projmap (make-hash-table :test #'equalp)))
    (loop for proj in
          (ttt:employee-recent-ts-projects conf)
          for pcnt = 1 then (1+ pcnt)
          do
          (progn
            (setf (gethash pcnt projmap) (pairlis `(:id :name :activities)
                                                  `(,(ttu:av proj :id) ,(ttu:av proj :name) ,(make-hash-table :test #'equalp))))
            (loop for act in (ttt:employee-recent-ts-project-activities conf (write-to-string (ttu:av proj :id)))
                  for acnt = 1 then (1+ acnt)
                  for actmap = (ttu:av (gethash pcnt projmap) :activities)
                  do
                  (setf (gethash acnt actmap) act))))
    projmap))

(defun display-recent (projmap)
 (loop for pcnt being the hash-key
          using (hash-value proj) of projmap
          for actmap = (ttu:av proj :activities)
          do
          (loop for acnt being the hash-keys of actmap
                for act being the hash-values of actmap
                do
                (format t "~a.~a: ~a / ~a~%" pcnt acnt (ttu:av proj :name) (ttu:av act :name)))))

; get timesheet entries for the current day for current user
(defun employee-recent-hours (conf employee-id)
  (loop for entry in
        (ttu:av (ttt:get-recent-project-timesheet-entry
                  conf
                  :from (ttu:today)
                  :to (ttu:day-offset 1)
                  :employeeid employee-id)
                :values)
        collect (pairlis `(:project :activity :entry :hours )
                         `(,(ttu:av (ttu:av entry :project) :id)
                            ,(ttu:av (ttu:av entry :activity) :id)
                            ,(ttu:av entry :id)
                            ,(ttu:av entry :hours)))))

(defun ts-entry-project-activity (timesheet project activity)
  "Find entry in timesheet list for given project and activity if any"
  (remove-if-not (lambda (al) (and (equalp project (ttu:av al :project)) (equalp activity (ttu:av al :activity)))) timesheet))

(defun parse-project (line)
  (parse-integer (first (str:split #\. line))))

(defun parse-activity (line)
  (let ((lst  (str:split #\. line)))
    (parse-integer (car (cdr lst)))))

(defun parse-input (inp projmap)
  (let* ((pcnt (parse-project inp))
         (acnt (parse-activity inp))
         (proj (gethash pcnt projmap))
         (pname (when proj (ttu:av proj :name)))
         (act (when proj (gethash acnt (ttu:av proj :activities))))
         (aname (when act (ttu:av act :name))))
    (values proj act pname aname)))

(defun add-hours-if-match (conf inp projmap)
  (multiple-value-bind (proj act pname aname) (parse-input inp projmap)
   (progn
      (format t "selected ~a: ~a / ~a~%" inp pname aname)
      (when (and proj act)
        (format t "hours: ")
        (finish-output)
        (let* ((project-id (ttu:av proj :id))
               (activity-id (ttu:av act :id))
               (hours (parse-float:parse-float (read-line) :junk-allowed t))
               (employee-id (write-to-string (ttu:av (ttt:get-whoami conf) :employee-id)))
               (entry (car (ts-entry-project-activity (employee-recent-hours conf employee-id) project-id activity-id))))
          (when hours
            (if entry
                (progn ;update existing entry
                  (format t "Update entry ~a for ~a / ~a with ~a h. (previous ~a h.)~%" (ttu:av entry :entry) pname aname hours (ttu:av entry :hours))
                  (ttt:update-timesheet-entry conf :id (ttu:av entry :entry) :hours hours))
                (progn ;create new entry
                  (format t "Add entry for ~a / ~a with ~a h.~%" pname aname hours)
                  (ttt:add-timesheet-entry conf
                    :project-id project-id :activity-id activity-id :employee-id employee-id
                    :date (ttu:today) :hours hours)))))))))

(defun main ()
  (let* ((conf (ttconf:config))
         (projmap (recent-proj conf)))
    (loop named main-loop
          do
          (progn
            (display-recent projmap)
            (format t "~%select project.activity [q if done]: ")
            (finish-output)
            (let ((inp (read-line)))
              (progn
                (if  (or (string= inp "d") (string= inp "q"))
                     (return-from main-loop)
                     (add-hours-if-match conf inp projmap))))))))

;; run
(defun run ()
  (progn
    (ttconf:setenv :environment :prod)
    (main)))

;; user interface
(defparameter *option-help*
  (adopt:make-option 'help
                     :help "No options defined yet."
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
    :name "tthours"
    :usage ""
    :summary "add hours today for recent project/activity in tripltex"
    :help "n/a"
    :manual "Just run the command"
    :contents (list *option-help*
                    *option-debug* *option-no-debug*)))

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
          (t (run)))
        (user-error (e) (adopt:print-error-and-exit e))))))
