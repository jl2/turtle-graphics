;;;; turtle-graphics.asd
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(asdf:defsystem #:turtle-graphics
  :description "Simple turtle graphics for CLGL."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (#:clgl #:alexandria  #:3d-vectors  #:3d-matrices)
  :components ((:file "package")
               (:file "turtle-graphics")))
