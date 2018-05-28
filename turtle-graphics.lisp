;;;; turtle-graphics.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:turtle-graphics)

(defun add-vec (location distance angle)
  (v+ location (vec3 (* distance (cos angle)) (* distance (sin angle)) 0)))

(defclass turtle (clgl:primitives)
  ((current-position :initform (vec3 0 0 0))
   (current-angle :initform 0)
   (pen-down :initform t)
   (current-color :initform (vec4 0 0 1 1)))
  (:documentation "A turtle graphics context."))


(defun forward (tc distance)
  (with-slots (pen-down current-position current-angle current-color) tc
    (let ((next-position (add-vec current-position distance current-angle)))
      (when pen-down
        (add-line tc current-position next-position current-color))
      (setf current-position next-position))))

(defun backward (tc distance)
  (forward tc (- distance)))

(defun right (tc angle)
  (with-slots (current-angle) tc
    (incf current-angle angle)))

(defun left (tc angle)
  (with-slots (current-angle) tc
    (decf current-angle angle)))

(defun goto (tc next-position)
  (with-slots (pen-down current-position current-angle current-color) tc
    (when pen-down
      (add-line tc current-position next-position current-color))
    (setf current-position next-position)))

(defun setpos (tc next-position)
  (goto gc next-position))

(defun setheading (tc angle)
  (with-slots (current-angle) tc
    (setf current-angle angle)))

(defun home (tc)
  (with-slots (current-position) tc
    (when pen-down
      (add-line tc current-position (vec3 0 0 0) current-color))
    (setf current-position (vec3 0 0 0))))

(defun dot (tc )
  (with-slots (current-position current-color) tc
    (add-point tc current-position current-color)))

(defun get-position (tc)
  (slot-value 'current-position tc))

(defun towards (tc position)
  (with-slots (current-position current-angle) tc
    (let* ((vector-to-position (v- current-position position))
           (angle-to-position (atan (vy vector-to-position) (vx vector-to-position))))
      (setf current-angle angle-to-position))))

(defun pen-up (tc )
  (with-slots (pen-down) tc
    (setf pen-down nil)))

(defun pen-down (tc )
  (with-slots (pen-down) tc
    (setf pen-down t)))

(defun pen-color (tc)
  (slot-value 'current-color tc))

(defun set-pen-color (tc color)
  (with-slots (current-color) tc
    (setf current-color color)))

(defun reset (tc)
  (with-slots (current-position current-angle current-color) tc
    (setf current-position (vec3 0 0 0))
    (setf current-angle 0)
    (setf pen-down t)
    (setf current-color (vec4 0 0 1 1))))
