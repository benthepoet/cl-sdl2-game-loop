#|
  This file is a part of sdl-test
  (c) 2019 Ben Hanna (benpaulhanna@gmail.com)
  Author: Ben Hanna <benpaulhanna@gmail.com>
|#

(defpackage #:sdl-test
  (:use #:cl #:sdl2)
  (:export #:main))

(in-package #:sdl-test)

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
   (texture
    :initarg :texture)))

(defclass sprite ()
  ((animation
    :initarg :animation
    :accessor animation)))

(defclass game-state ()
  ((sprites
    :initarg :sprites
    :accessor sprites)))

(defvar *state*)
(defvar *next-tick*)
(defvar *skip-ticks* (float (/ 1000 60)))
(defvar *sleep-ticks*)

(defun initialize (renderer)
  (setf *state*
        (make-instance 'game-state
                       :sprites (list (make-instance 'sprite
                                            :animation (make-instance 'animation
                                                           :x 0
                                                           :y 0
                                                           :w 24
                                                           :h 24
                                                           :current 0
                                                           :total 4
                                                           :timer 0
                                                           :duration 10
                                                           :texture (sdl2:create-texture-from-surface renderer (sdl2:load-bmp "run.bmp")))))))
  (setf *next-tick* 0)
  (setf *sleep-ticks* 0))

(defun main ()
  (sdl2:with-init (:video)
    (sdl2:with-window (win :title "Demo" :w 640 :h 480 :flags '(:shown))
      (sdl2:with-renderer (renderer win)
        (initialize renderer)
        (sdl2:with-event-loop (:method :poll)
          (:keyup
           (:keysym keysym)
           (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
             (sdl2:push-event :quit)))
          (:idle
           ()
           (update)
           (draw renderer)
           (incf *next-tick* *skip-ticks*)
           (setf *sleep-ticks* (- *next-tick* (sdl2:get-ticks)))
           (if (> *sleep-ticks* 0)
               (sdl2:delay (floor *sleep-ticks*))))
          (:quit () t))))))

(defun draw (renderer)
  (sdl2:set-render-draw-color renderer 255 255 255 255)
  (sdl2:render-clear renderer)

  (loop for sprite in (sprites *state*) do
        (draw-sprite renderer sprite))
  
  (sdl2:render-present renderer))

(defun draw-sprite (renderer sprite)
  (with-slots (x y w h texture) (animation sprite)
    (let ((source-rect (make-rect x y w h))
          (dest-rect (make-rect 0 0 (scale w) (scale h))))
      (sdl2:render-copy renderer texture :source-rect source-rect :dest-rect dest-rect))))

(defun update ()
  (loop for sprite in (sprites *state*) do
        (update-sprite sprite)))

(defun update-sprite (sprite)
  (with-slots (x w current total duration timer) (animation sprite)
    (incf timer)
    (if (> timer duration)
        (progn
          (setf timer 0)
          (incf current)
          (if (= current total)
              (setf current 0))
          (setf x (* current w))))))

(defun scale (x)
  (* x 3))
