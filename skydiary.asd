(asdf:defsystem #:skydiary
  :serial t
  :depends-on (#:cl-gtk2-gtk
               #:cl-gtk2-glib
               #:local-time
               #:cxml)
  :components ((:file "package")
               (:file "skydiary")))
