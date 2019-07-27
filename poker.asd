(defpackage :poker.asd
  (:use :cl :asdf))

(defsystem "poker"
  :name "poker"
  :version "0.0.0"
  :author "Arthur Goldman"
  :license "MIT"
  :depends-on (:iterate
                :sketch)
  :serial t
  :components
  ((:file "package")
   (:file "card")
   (:file "hand")
   (:file "score")
   (:file "deck")
   (:file "poker")))
