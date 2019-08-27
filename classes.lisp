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
    :accessor texture-key)
   (animations
    :accessor animations)))

(defclass player (sprite)
  ((texture-key
    :initform 'player)
   (animations
    :initform (list (make-animation :idle 0 0 24 24 1 1 nil)
                    (make-animation :walk 0 0 24 24 4 10 t)))))

(defclass game-state ()
  ((renderer
    :initarg :renderer
    :accessor renderer)
   (sprites
    :initarg :sprites
    :accessor sprites)
   (textures
    :initform (make-hash-table)
    :accessor textures)))

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
  (make-instance 'player
                 :x-position x-position
                 :y-position y-position))

(defun make-game-state (renderer)
  (make-instance 'game-state
                 :renderer renderer
                 :sprites (list (make-player 0 120))))
