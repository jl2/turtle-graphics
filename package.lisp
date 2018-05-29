;;;; package.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(defpackage #:turtle-graphics
  (:use #:cl #:clgl #:3d-vectors #:3d-matrices #:alexandria)
  (:nicknames :tg)
  (:export #:turtle
           #:forward
           #:backward
           #:right
           #:left
           #:goto
           #:setpos
           #:setheading
           #:home
           #:dot
           #:get-position
           #:towards
           #:pen-up
           #:pen-down
           #:pen-color
           #:set-pen-color
           #:reset
           #:l-system
           #:generate
           #:generate-l-system
           #:l-system-viewer
           ))
