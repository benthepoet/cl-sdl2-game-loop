(in-package :cl-sdl2-game-loop)

(defclass animation ()
  ((name
    :initarg :name)
   (x-offset
    :initarg :x-offset)
   (y-offset
    :initarg :y-offset)
   (frame-width
    :initarg :frame-width)
   (frame-height
    :initarg :frame-height)
   (frame-total
    :initarg :frame-total)
   (frame-duration
    :initarg :frame-duration)
   (repeat
    :initarg :repeat)
   (frame-count
    :initform 0)
   (frame-timer
    :initform 0)))

(defclass sprite ()
  ((x-position
    :initform 0
    :initarg :x-position
    :accessor x-position)
   (y-position
    :initform 0
    :initarg :y-position
    :accessor y-position)
   (texture-key
    :initarg :texture-key
    :accessor texture-key)
   (animations
    :initarg :animations
    :accessor animations)))

(defclass game-state ()
  ((sprites
    :initarg :sprites
    :accessor sprites)))

(defun make-animation (name x-offset y-offset frame-width frame-height frame-total frame-duration repeat)
  (make-instance 'animation
                 :name name
                 :x-offset x-offset
                 :y-offset y-offset
                 :frame-width frame-width
                 :frame-height frame-height
                 :frame-total frame-total
                 :frame-duration frame-duration
                 :repeat repeat))

(defun make-player (x-position y-position)
  (make-instance 'sprite
                 :x-position x-position
                 :y-position y-position
                 :texture-key 'player
                 :animations (list (make-animation :walk 0 0 24 24 4 10 t))))

(defun make-game-state ()
  (make-instance 'game-state
                 :sprites (list (make-player 0 120)
                                (make-player 80 80))))
