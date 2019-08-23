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
  ((animations
    :initarg :animations
    :accessor animations)
   (texture-path
    :initarg :texture-path
    :accessor texture-path)))

(defclass game-state ()
  ((renderer
    :initarg :renderer
    :accessor renderer)
   (sprites
    :initarg :sprites
    :accessor sprites)
   (textures
    :initform (make-hash-table :test #'equal)
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

(defun make-sprite (texture-path animations)
  (make-instance 'sprite
                 :texture-path texture-path
                 :animations animations))

(defun make-game-state (renderer)
  (make-instance 'game-state
                 :renderer renderer
                 :sprites (list
                           (make-sprite
                            "assets/player.bmp"
                            (list (make-animation 'run 0 0 24 24 4 10 t))))))

