;;Diary manager for linux implemented in CL
;;Author: Pocket7878 <poketo7878@gmail.com>
;;Date: 2011/09/10
;;
;;TODO 
;; * Create rc file

(in-package :nikki)

;;Diary home dir
(defvar *diary-dir* (merge-pathnames (user-homedir-pathname) "/nikki"))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)(princ a s))))

(defun multiple-connect-signal (signals-list obj handler)
     (loop for signal in signals-list
         do 
         (gobject:connect-signal obj signal handler)))
            
(defclass signal-group ()
  ((signals :initform nil :initarg :signals :accessor signals)))

(defmethod add-signal ((sg signal-group) new-sig)
  (setf (signals sg) (cons new-sig (signals sg))))

(defmethod connect-signal-group (obj (sg signal-group) handler)
  (multiple-connect-signal (signals sg) obj handler))

(defun get-time-string ()
  (multiple-value-bind (sec min hour d m y)
    (get-decoded-time)
    (declare (ignore sec min hour))
    (format nil "~A/~A/~A" y m d)))

(defvar *text-changed* nil)
(defvar *action-from-toolbar* nil)
(defvar *canceled* nil)

(defun run ()
  (gtk:within-main-loop
    (let* (;;time
           (current-date (local-time:now))
           (prev-date (local-time:now))
           ;;Gtk+ components
           (builder 
             (make-instance 'gtk:builder
                            :from-file "/home/masato/Desktop/nikki/nikki.glade"))
           (window (gtk:builder-get-object builder
                                           "mainWindow"))
           (calender (gtk:builder-get-object builder
                                             "calender"))
           (date-label (gtk:builder-get-object builder
                                               "dateLabel"))
           (main-text-view (gtk:builder-get-object builder
                                                   "mainTextView"))
           (post-text-view (gtk:builder-get-object builder
                                                   "postTextView"))
           (memo-text-view (gtk:builder-get-object builder
                                                   "memoTextView"))
           (title-entry (gtk:builder-get-object builder
                                                "titleEntry"))
	   (weather-entry (gtk:builder-get-object builder
						  "weatherEntry"))
           ;;Took bar buttons
           (quit-button (gtk:builder-get-object builder "quitButton"))
           (save-button (gtk:builder-get-object builder "saveButton"))
           (prev-year-button (gtk:builder-get-object builder "prevYearButton"))
           (prev-month-button (gtk:builder-get-object builder "prevMonthButton"))
           (prev-week-button (gtk:builder-get-object builder "prevWeekButton"))
           (prev-day-button (gtk:builder-get-object builder "prevDayButton"))
           (today-button (gtk:builder-get-object builder "todayButton"))
           (next-year-button (gtk:builder-get-object builder "nextYearButton"))
           (next-month-button (gtk:builder-get-object builder "nextMonthButton"))
           (next-week-button (gtk:builder-get-object builder "nextWeekButton"))
           (next-day-button (gtk:builder-get-object builder "nextDayButton"))
           ;;Define signal-group for text-view
           (calender-changed (make-instance 'signal-group
                                            :signals '("day-selected"))))
      ;;
      (gobject:connect-signal window "destroy"
                              (lambda (w)
                                (declare (ignore w))
                                (gtk:leave-gtk-main)))
      ;;Utility functions
      (labels ((show-calendar-info ()
                                   (format t "Calender: ~A/~A/~A~%" 
                                           (gtk:calendar-year calender)
                                           (1+ (gtk:calendar-month calender))
                                           (gtk:calendar-day calender)))
               (init-fields ()
                            (setf (gtk:label-label date-label) 
                                  (format nil "~A/~A/~A" 
                                          (gtk:calendar-year calender)
                                          (1+ (gtk:calendar-month calender))
                                          (gtk:calendar-day calender)))
                            ;;そのカレンダーに表示されている日付の日記の内容を表示する
                            (multiple-value-bind (title weather main post memo)
                              (read-diary-xml (gtk:calendar-year calender) 
                                              (1+ (gtk:calendar-month calender))
                                              (gtk:calendar-day calender))
                              (progn 
                                (setf (gtk:text-buffer-text (gtk:text-view-buffer main-text-view)) main)
                                (setf (gtk:text-buffer-text (gtk:text-view-buffer post-text-view)) post)
                                (setf (gtk:text-buffer-text (gtk:text-view-buffer memo-text-view)) memo)
                                (setf (gtk:entry-text title-entry) title)
                                (setf (gtk:entry-text weather-entry) weather)
                                ;テキストは未編集状態になっているはず
                                (setf *text-changed* nil))))
               (update-calender ()
                                ;;内部時刻にあわせてカレンダーを変更する
                                (psetf (gtk:calendar-day calender) (local-time:timestamp-day current-date)
                                       (gtk:calendar-month calender) (1- (local-time:timestamp-month current-date))
                                       (gtk:calendar-year calender) (local-time:timestamp-year current-date))
                                (init-fields))
               (sync-date ()
                          ;;内部時刻をカレンダーにあわせる
                          (setf current-date (local-time:parse-timestring (format nil "~A-~A-~A" 
                                                                                  (gtk:calendar-year calender)
                                                                                  (+  1 (gtk:calendar-month calender))
                                                                                  (gtk:calendar-day calender))))))
        ;;最初に表示されているタイトルバーを現在の日付にしておく
        (setf (gtk:label-label date-label)
              (local-time:format-timestring nil current-date :format '(:year #// :month #// :day)))
        ;;カレンダーが今日をさすようにしておく
        (update-calender)
        ;;今日の日記を表示する
        (init-fields)
        ;;Signal-handler for text-view in notebook
        (gobject:connect-signal
          (gtk:text-view-buffer main-text-view) "changed"
          (lambda (text-buffer)
            (declare (ignore text-buffer))
            (setf *text-changed* t)))
        (gobject:connect-signal
          (gtk:text-view-buffer post-text-view) "changed"
          (lambda (text-buffer)
            (declare (ignore text-buffer))
            (setf *text-changed* t)))
        (gobject:connect-signal
          (gtk:text-view-buffer memo-text-view) "changed"
          (lambda (text-buffer)
            (declare (ignore text-buffer))
            (setf *text-changed* t)))
        ;;Signal-handler for calenders
        (connect-signal-group
          calender calender-changed
          (lambda (calendar)
            (if *text-changed*
              (let ((dialog (make-instance 'gtk:message-dialog
                                           :text "日記が編集されています。編集内容を保存しますか？"
                                           :modal t
                                           :buttons :ok-cancel
                                           :message-type :warning)))
                (gobject:connect-signal dialog "response"
                                        (lambda (diag response-id)
                                          (if (equal response-id -5)
                                            (progn
                                              (output-diary-xml 
                                                (local-time:timestamp-year prev-date)
                                                (local-time:timestamp-month prev-date)
                                                (local-time:timestamp-day prev-date)
                                                (gtk:entry-text title-entry)
                                                (gtk:entry-text weather-entry)
                                                (gtk:text-buffer-text (gtk:text-view-buffer main-text-view))
                                                (gtk:text-buffer-text (gtk:text-view-buffer post-text-view))
                                                (gtk:text-buffer-text (gtk:text-view-buffer memo-text-view)))
                                              (init-fields))
                                            (init-fields))))
                (unwind-protect (gtk:dialog-run dialog) 
                  (gtk:object-destroy dialog)))
              (progn
                (when (not *action-from-toolbar*)
                  (sync-date))
                (init-fields)))))
        ;;Signal-handler for each toolBar button
        (gobject:connect-signal quit-button
                                "clicked"
                                (lambda (button)
                                  (declare (ignore button))
                                  (gtk:object-destroy window)))
        (gobject:connect-signal save-button
                                "clicked"
                                (lambda (button)
                                  (declare (ignore button))
                                  (setf *text-changed* nil)
                                  (output-diary-xml 
                                    (gtk:calendar-year calender)
                                    (1+ (gtk:calendar-month calender))
                                    (gtk:calendar-day calender)
                                    (gtk:entry-text title-entry)
				    (gtk:entry-text weather-entry)
                                    (gtk:text-buffer-text (gtk:text-view-buffer main-text-view))
                                    (gtk:text-buffer-text (gtk:text-view-buffer post-text-view))
                                    (gtk:text-buffer-text (gtk:text-view-buffer memo-text-view)))))
        ;;ツールバーの日付の変更ボタンが選択されたら、内部時刻を変更して、
        ;;それにカレンダーを同期させる（こうしないと一週間戻るとかが実装できない)
        ;;ツールバーからの操作であることを指定（これによって内部時刻の方が正確であることをしめす）
        (gobject:connect-signal prev-year-button
                                "clicked"
                                (lambda (button)
                                  (declare (ignore button))
                                  (setf current-date (local-time:timestamp- current-date 1 :year))
                                  (setf *action-from-toolbar* t)
                                  (update-calender)
                                  ))
        (gobject:connect-signal prev-month-button
                                "clicked"
                                (lambda (button)
                                  (declare (ignore button))
                                  (setf current-date (local-time:timestamp- current-date 1 :month))
                                  (setf *action-from-toolbar* t)
                                  (update-calender)
                                  ))
        (gobject:connect-signal prev-week-button
                                "clicked"
                                (lambda (button)
                                  (declare (ignore button))
                                  (setf current-date (local-time:timestamp- current-date 7 :day))
                                  (setf *action-from-toolbar* t)
                                  (update-calender)
                                  ))
        (gobject:connect-signal prev-day-button
                                "clicked"
                                (lambda (button)
                                  (declare (ignore button))
                                  (setf current-date (local-time:timestamp- current-date 1 :day))
                                  (setf *action-from-toolbar* t)
                                  (update-calender)
                                  ))
        (gobject:connect-signal today-button
                                "clicked"
                                (lambda (button)
                                  (declare (ignore button))
                                  (setf current-date (local-time:now))
                                  (setf *action-from-toolbar* t)
                                  (update-calender)
                                  ))
        (gobject:connect-signal next-year-button
                                "clicked"
                                (lambda (button)
                                  (declare (ignore button))
                                  (setf current-date (local-time:timestamp+ current-date 1 :year))
                                  (setf *action-from-toolbar* t)
                                  (update-calender)
                                  ))
        (gobject:connect-signal next-month-button
                                "clicked"
                                (lambda (button)
                                  (declare (ignore button))
                                  (setf current-date (local-time:timestamp+ current-date 1 :month))
                                  (setf *action-from-toolbar* t)
                                  (update-calender)
                                  ))
        (gobject:connect-signal next-week-button
                                "clicked"
                                (lambda (button)
                                  (declare (ignore button))
                                  (setf current-date (local-time:timestamp+ current-date 7 :day))
                                  (setf *action-from-toolbar* t)
                                  (update-calender)
                                  ))
        (gobject:connect-signal next-day-button
                                "clicked"
                                (lambda (button)
                                  (declare (ignore button))
                                  (setf current-date (local-time:timestamp+ current-date 1 :day))
                                  (setf *action-from-toolbar* t)
                                  (update-calender)
                                  ))
        (gtk:widget-show window)))))

(defun output-diary-xml (year month day title weather main post memo)
  (ensure-directories-exist *diary-dir*)
  (with-open-file (out (mkstr
                         *diary-dir*
                         "/"
                         year month day ".xml")
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
    (format *standard-output* "~A~%" out)
    (finish-output *standard-output*)
    (cxml:with-xml-output
      (cxml:make-octet-stream-sink out) ;:indentation 2 :canonical nil)
      (cxml:with-element "diary"
                         (cxml:with-element "info"
                                            (cxml:with-element "date"
                                                               (cxml:attribute "year" year)
                                                               (cxml:attribute "month" month)
                                                               (cxml:attribute "day" day))
                                            (cxml:with-element "weather"
                                                               (cxml:text weather)))
                         (cxml:with-element "contents"
                                            (cxml:attribute "title" title)
                                            (cxml:with-element "main" (cxml:text main))
                                            (cxml:with-element "post" (cxml:text post))
                                            (cxml:with-element "memo" (cxml:text memo)))))))

(defun read-diary-xml (year month day)
  (let ((title "")
        (weather "")
        (main "")
        (post "")
        (memo ""))
    (when (probe-file (mkstr *diary-dir* "/" year month day ".xml"))
      (let ((document (cxml:parse-file (mkstr *diary-dir* "/" year month day ".xml")
                                       (stp:make-builder))))
        (setf title (xpath:string-value (xpath:evaluate "/diary/contents/@title" document)))
        (setf weather (xpath:string-value (xpath:evaluate "/diary/info/weather" document)))
        (setf main (xpath:string-value (xpath:evaluate "/diary/contents/main" document)))
        (setf post (xpath:string-value (xpath:evaluate "/diary/contents/post" document)))
        (setf memo (xpath:string-value (xpath:evaluate "/diary/contents/memo" document)))))
    (values title weather main post memo)))

(defun nikki ()
  (progn
    (run)
    (gtk:join-gtk-main)))
