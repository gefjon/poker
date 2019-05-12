(defpackage :poker.asd
  (:use :cl :asdf))

(defsystem "poker"
  :name "poker"
  :version "0.0.0"
  :author "Arthur Goldman"
  :license "MIT"
  :depends-on (:iterate)
  :serial t
  :components
  ((:file "card")
   (:file "score")
   (:file "poker")
   ))
