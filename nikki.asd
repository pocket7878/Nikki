(asdf:defsystem #:nikki
  :serial t
  :depends-on (#:cl-gtk2-gtk
               #:cl-gtk2-glib
               #:local-time
               #:xpath
               #:cxml
               #:cxml-stp)
  :components ((:file "package")
               (:file "nikki")))
