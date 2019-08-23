(in-package :cl-sdl2-game-loop)

(defclass animation ()
  ((name
    :initarg :name)
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
   (texture-path
    :initarg :texture-path
    :accessor texture-path)
   (frame-count
    :initform 0)
   (frame-timer
    :initform 0)))

(defclass sprite ()
  ((animation
    :initarg :animation
    :accessor animation)))

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

(defun make-animation (name frame-width frame-height frame-total frame-duration repeat texture-path)
  (make-instance 'animation
                 :name name
                 :frame-width frame-width
                 :frame-height frame-height
                 :frame-total frame-total
                 :frame-duration frame-duration
                 :repeat repeat
                 :texture-path texture-path))

(defun make-sprite (animation)
  (make-instance 'sprite
                 :animation animation))

(defun make-game-state (renderer)
  (make-instance 'game-state
                 :renderer renderer
                 :sprites (list
                           (make-sprite
                            (make-animation "run" 24 24 4 10 t "assets/run.bmp")))))

