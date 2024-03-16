(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort :adopt :str :parse-float :cl-charms :fuzzy-match :ttcon) :silent t))

(defpackage :tthours
  (:use :cl)
  (:local-nicknames (:ch :charms) (:fm :fuzzy-match))
  (:export :toplevel *ui*))

(in-package :tthours)

;;; optimize for speed and safety.
(declaim (optimize (speed 3) (safety 3) (debug 0)))

;; errors
(define-condition user-error (error) ())

;; functionality
(defun recent-proj (conf)
  (let ((projmap (make-hash-table :test #'equalp)) ; lookup for simplified userinput
        (mapproj (make-hash-table :test #'equalp)) ; lookup by project-id
        (mapacts (make-hash-table :test #'equalp))) ; lookup by activity-id
    (loop for proj in
          (ttt:employee-recent-ts-projects conf)
          for pcnt = 1 then (1+ pcnt)
          for pid = (ttu:av proj :id)
          do
          (progn
            (setf (gethash pcnt projmap) (pairlis `(:id :name :activities)
                                                  `(,pid ,(ttu:av proj :name) ,(make-hash-table :test #'equalp))))
            (setf (gethash pid mapproj) proj)
            (loop for act in (ttt:employee-recent-ts-project-activities conf (write-to-string (ttu:av proj :id)))
                  for aid = (ttu:av act :id)
                  for acnt = 1 then (1+ acnt)
                  for actmap = (ttu:av (gethash pcnt projmap) :activities)
                  do
                  (progn
                    (setf (gethash aid mapacts) act)
                    (setf (gethash acnt actmap) act)))))
    (values projmap mapproj mapacts)))

(defun display-recent (projmap)
  (loop for pcnt being the hash-key
        using (hash-value proj) of projmap
        for actmap = (ttu:av proj :activities)
        do
        (loop for acnt being the hash-keys of actmap
              for act being the hash-values of actmap
              do
              (format t "~a.~a: ~a / ~a~%" pcnt acnt (ttu:av proj :name) (ttu:av act :name)))))

(defun default-project-activity (projmap ts-entry)
  "Format the ts-entry as project.activity if present in the projmap"
  (let* ((epid (ttu:av (ttu:av ts-entry :project) :id))
         (eaid (ttu:av (ttu:av ts-entry :activity) :id))
         (result (car ; outer collect make list
                   (car ; inner collect make list
                     (loop
                       for pcnt being the hash-key using (hash-value proj) of projmap
                       for actmap = (ttu:av proj :activities)
                       when (eq  epid (ttu:av proj :id)) collect
                       (loop
                         for acnt being the hash-key using (hash-value act) of actmap
                         when (eq eaid (ttu:av act :id)) collect
                         (cons pcnt acnt)))))))
    (if result
        (format nil "~a.~a" (car result) (cdr result))
        "")))

(defun display-timesheet (timesheet projs acts)
  (progn
    (format t "~%--------- timeliste ----------~%")
    (loop for entry in timesheet
          do
          (let ((comment (ttu:av entry :comment)))
            (format t "~a / ~a: ~ah ~@[(~a)~]~%"
                    (ttu:av (gethash (ttu:av entry :project) projs) :name)
                    (ttu:av (gethash (ttu:av entry :activity) acts) :name)
                    (ttu:av entry :hours)
                    (when (string/= "" comment)
                      comment))))))

(defun employee-recent-hours (conf employee-id)
  "get timesheet entries for the current day for current user"
  (loop for entry in
        (ttu:av (ttt:get-recent-project-timesheet-entry
                  conf
                  :from (ttu:today)
                  :to (ttu:day-offset 1)
                  :employeeid employee-id)
                :values)
        collect (pairlis `(:project :activity :entry :hours :comment)
                         `(,(ttu:av (ttu:av entry :project) :id)
                            ,(ttu:av (ttu:av entry :activity) :id)
                            ,(ttu:av entry :id)
                            ,(ttu:av entry :hours)
                            ,(ttu:av entry :comment)))))

(defun last-hours-previous-week (conf employee-id)
  "get the last timesheet entry for the previous week, including the current date"
  (car (ttu:av (ttt:get-recent-project-timesheet-entry
                 conf
                 :from (ttu:day-offset -7)
                 :to (ttu:day-offset 1)
                 :employeeid employee-id)
               :values)))

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

(defun add-hours-with-comment (conf timesheet employee-id hours-default pname project-id aname activity-id)
  (format t "hours [~:a]: " hours-default)
  (finish-output)
  (let ((hours (or (parse-float:parse-float (read-line) :junk-allowed t)
                   (and hours-default (parse-float:parse-float hours-default :junk-allowed t)))))
    (format t "comment [⏎ for blank]: ")
    (finish-output)
    (let* (; Get comment from input:
           (comment (read-line))
           ; check if we already have an entry for same project / activity today:
           (entry (car (ts-entry-project-activity timesheet project-id activity-id))))
      (when hours
        (if entry
            (progn ;update existing entry
              (format t "Update entry ~a for ~a / ~a with ~a h. (previous ~a h.)~%" (ttu:av entry :entry) pname aname hours (ttu:av entry :hours))
              (ttt:update-timesheet-entry conf :id (ttu:av entry :entry) :hours hours :comment comment))
            (progn ;create new entry
              (format t "Add entry for ~a / ~a with ~a h.~%" pname aname hours)
              (ttt:add-timesheet-entry conf
                                       :project-id project-id :activity-id activity-id :employee-id employee-id
                                       :date (ttu:today) :hours hours :comment comment)))))))

(defun add-hours-if-match (conf employee-id hours inp projmap timesheet)
  (multiple-value-bind (proj act pname aname) (parse-input inp projmap)
    (progn
      (format t "selected ~a: ~a / ~a~%" inp pname aname)
      (when (and proj act)
        (let* ((project-id (ttu:av proj :id))
               (activity-id (ttu:av act :id)))
          (add-hours-with-comment conf timesheet employee-id hours pname project-id aname activity-id))))))


;;; Functions for manually searching for project/activity in a curses interface
;;;

(defun print-at (line str)
  (multiple-value-bind (w h) (ch:window-dimensions ch:*standard-window*)
    (declare (ignorable w))
    (ch:write-string-at-point ch:*standard-window* str 1 (- h line))))

; TODO: should check that we have enough lines in window, else curses will crash
(defun print-cust-proj (cust proj act displaynames)
  (loop named disp-loop
        for name in displaynames
        for i from 1
        do
        (progn
          (print-at (- 17 i) (format nil "~a: ~a" i name))
          ; just display at most 10 first
          (when (= i 10) (return-from disp-loop))))
  (print-at 5 (format nil "customer: ~a" cust))
  (print-at 4 (format nil "project:  ~a" proj))
  (print-at 3 (format nil "activity: ~a" act)))

(defun print-state (state)
  (print-at 2 (format nil "input> ~a" state)))

(defun get-projs (conf)
  (ttu:av (ttt:list-active-projects conf) :values))

(defun get-customer-names (projs)
  (remove-duplicates
    (loop for p in projs
          for cust = (ttu:av (ttu:av p :customer) :name)
          when cust collect cust) :test #'string=))

(defun get-project-names (projs &optional (customer nil))
  "Return a list of project names, but only for the given customer if customer is set"
  (if customer
      (loop for p in projs
        for cust = (ttu:av (ttu:av p :customer) :name)
        when (string= cust customer) collect (ttu:av p :name))
      (loop for p in projs collect (ttu:av p :name))
      ))

(defun get-project-id-by-cust-and-proj (projs cust proj)
  "Return the project id based on customer name and project name"
  (car (loop for p in projs
        for pname = (ttu:av p :name)
        for cname = (ttu:av (ttu:av p :customer) :name)
        when (and (string= cust cname) (string= proj pname)) collect (ttu:av p :id))))

(defun get-activities (conf pid)
  (let* ((acts (ttu:av (ttt:project-timesheet-activities conf (write-to-string pid))  :values))
         (actmap (make-hash-table :test #'equalp))
         (names (loop for a in acts
                      for aid = (ttu:av a :id)
                      for aname = (ttu:av a :name)
                      do (setf (gethash aname actmap) aid)
                      collect aname)))
    (values names actmap)))

(defun curses-search (conf)
  (let* ((allprojs (get-projs conf))
         (customers (get-customer-names allprojs))
         (searchnames customers))
    (ch:with-curses
    ()
    (ch:disable-echoing)
    (ch:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)

    (let
      ((state "")
       (cust-selected nil)
       (proj-selected nil)
       (cust "")
       (proj "")
       (pid nil)
       (projectnames nil)
       (act "")
       (actnames nil)
       (actmap nil))
      (loop named event-loop
          for c = (ch:get-char charms:*standard-window* :ignore-error t)
          do
          (progn

            ;; update state
            (when (and c (not (char= c #\Linefeed)))
              (progn
                (if (or (char= c #\Backspace) (char= c #\Rubout))
                    (when (< 0 (length state))
                      (setf state (subseq state 0 (1- (length state)))))
                    (setf state (concatenate 'string state (string c))))
                (cond ((and cust-selected proj-selected) (progn
                                                           (setf searchnames (fm:fuzzy-match state actnames))
                                                           (setf act (car searchnames))))
                      (cust-selected (progn
                                       (setf searchnames (fm:fuzzy-match state projectnames))
                                       (setf proj (car searchnames))))
                      (t (progn
                           (setf searchnames (fm:fuzzy-match state customers))
                           (setf cust (car searchnames))) ))))

            ;; redraw
            (charms:clear-window charms:*standard-window*)
            (print-cust-proj cust proj act searchnames)
            (print-state state)
            (charms:refresh-window charms:*standard-window*)

            (case c
              ((nil) nil)
              ((#\Linefeed #\Return) (cond ((and cust-selected proj-selected) (return-from event-loop))
                                           (cust-selected (progn
                                                            (setf state  "")
                                                            (setf pid (get-project-id-by-cust-and-proj allprojs cust proj))
                                                            (multiple-value-bind (an am)(get-activities conf pid)
                                                              (setf actnames an)
                                                              (setf actmap am))
                                                            (setf searchnames actnames)
                                                            (setf proj-selected t)))
                                           (t (progn
                                                (setf state "")
                                                (setf projectnames (get-project-names allprojs cust))
                                                (setf searchnames projectnames)
                                                (setf cust-selected t))))))))
      (values proj pid act (gethash act actmap))))))

(defun add-by-search (conf timesheet employee-id hours)
  (multiple-value-bind (pname project-id aname activity-id) (curses-search conf)
    (format t "selected ~a / ~a~%" pname aname)
    (add-hours-with-comment conf timesheet employee-id hours pname project-id aname activity-id)))

(defun get-project-activity (accept-default tsdefault projmap timesheet projs acts)
  (display-recent projmap)
  (display-timesheet timesheet projs acts)
  (format t "~%select project.activity [q exit|s search] [~a]: " tsdefault)
  (finish-output)
  (if accept-default
      (progn
        (format t "~a~%" tsdefault)
        tsdefault)
      (read-line)))

(defun main (hours &key (accept-project nil))
  (let* ((conf (ttconf:config))
         (employee-id (write-to-string (ttu:av (ttt:get-whoami conf) :employee-id)))
         (ts (employee-recent-hours conf employee-id)))
    (multiple-value-bind (projmap projs acts) (recent-proj conf)
      (let ((default (default-project-activity projmap (last-hours-previous-week conf employee-id))))
        (loop named main-loop
              ; update timesheet on each iteration to display most recent data
              for timesheet = ts then (employee-recent-hours conf employee-id)
              ; also update the default project.activity, in case it has changed
              for tsdefault = default then (default-project-activity projmap (last-hours-previous-week conf employee-id))
              do
              (progn
                (let ((inp (get-project-activity accept-project tsdefault projmap timesheet projs acts)))
                  (progn
                    (cond ((or (string= inp "d") (string= inp "q"))
                           (return-from main-loop))
                          ((string= inp "s") (progn
                                               (add-by-search conf timesheet employee-id hours)
                                               (multiple-value-bind (pm ps as) (recent-proj conf)
                                                 (setf projmap pm) (setf projs ps) (setf acts as))))
                          (t (progn
                               (if (string= inp "")
                                 (add-hours-if-match conf employee-id hours tsdefault projmap timesheet)
                                 (add-hours-if-match conf employee-id hours inp projmap timesheet))
                               (when accept-project (return-from main-loop)))))))))))))

;; run
(defun run (args &key (accept-project nil))
  (let ((hours (and args (car args))))
    (progn
      (ttconf:setenv :environment :prod)
      (main hours :accept-project accept-project))))

;; user interface
(defparameter *option-help*
  (adopt:make-option 'help
                     :help "No options defined yet."
                     :long "help"
                     :short #\h
                     :reduce (constantly t)))

(defparameter *option-debug*
  (adopt:make-option 'debug
                     :long "debug"
                     :short #\d
                     :help "Enable the lisp debugger."
                     :reduce (constantly t)))

(defparameter *option-accept-project*
  (adopt:make-option 'accept-project
                     :long "acceptproject"
                     :short #\p
                     :help "Accept the default project and activity. Quit after submitting one entry."
                     :reduce (constantly t)))

(defparameter *ui*
  (adopt:make-interface
    :name "tthours"
    :usage "tthours [hours]"
    :summary "add hours today for recent project/activity in tripltex. If hours given as argument, then that number will be used as default."
    :help "n/a"
    :manual "Just run the command"
    :contents (list *option-help*
                    *option-accept-project*
                    *option-debug*)))

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
        (format t "~a (len ~a)~%options:~%" args (length args))
        (maphash (lambda (k v) (format t "~a => ~a~%" k v)) opts))
      (handler-case
        (cond
          ((gethash 'help opts) (adopt:print-help-and-exit *ui*))
          (t (run args :accept-project (gethash 'accept-project opts))))
        (user-error (e) (adopt:print-error-and-exit e))))))
