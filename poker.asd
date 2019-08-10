(defpackage :poker.asd
  (:use :cl :asdf))

(defsystem "poker"
  :name "poker"
  :version "0.0.0"
  :author "Arthur Goldman"
  :license "MIT"
  :depends-on (:iterate
                :sketch)
  :components
  ((:file :package)
   (:module :src
            :depends-on (:package)
            :components 
            ((:file :card)
             (:file :hand)
             (:file :score)
             (:file :deck)
             (:file :player)
             (:file :game)))))
