(in-package :cl-sdl2-game-loop)

(defclass animation ()
  ((x
    :initarg :x)
   (y
    :initarg :y)
   (w
    :initarg :w)
   (h
    :initarg :h)
   (total
    :initarg :total)
   (duration
    :initarg :duration)
   (texture-path
    :initarg :texture-path
    :accessor texture-path)
   (current
    :initform 0)
   (timer
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
    :initarg :textures
    :accessor textures)))

(defun make-animation (x y w h total duration texture-path)
  (make-instance 'animation
                 :x x
                 :y y
                 :w w
                 :h h
                 :total total
                 :duration duration
                 :texture-path texture-path))

(defun make-sprite (animation)
  (make-instance 'sprite
                 :animation animation))

(defun make-game-state (renderer)
  (make-instance 'game-state
                 :renderer renderer
                 :textures (make-hash-table :test #'equal)
                 :sprites (list
                           (make-sprite
                            (make-animation 0 0 24 24 4 10 "assets/run.bmp")))))

