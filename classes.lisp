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
   (current
    :initarg :current)
   (total
    :initarg :total)
   (duration
    :initarg :duration)
   (timer
    :initarg :timer)
   (texture-path
    :initarg :texture-path
    :accessor texture-path)))

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

