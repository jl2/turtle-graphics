;;;; package.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(defpackage #:turtle-graphics
  (:use #:cl #:clgl #:3d-vectors)
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
           #:pen-size
           #:pen-color
           #:reset
           #:clear))
