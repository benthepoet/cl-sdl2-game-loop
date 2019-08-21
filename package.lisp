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
                                                           :texture (sdl2:create-texture-from-surface renderer (sdl2:load-bmp "projects/cl-sdl2-game-loop/run.bmp"))))))))

(defmacro with-game-loop (&rest body)
  `(let ((next-tick 0)
        (sleep-ticks 0)
        (skip-ticks (float (/ 1000 60))))
    (sdl2:with-event-loop (:method :poll)
      (:keyup
       (:keysym keysym)
       (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
         (sdl2:push-event :quit)))
      (:idle
       ()
       ,@body
       (incf next-tick skip-ticks)
       (setf sleep-ticks (- next-tick (sdl2:get-ticks)))
       (if (> sleep-ticks 0)
           (sdl2:delay (floor sleep-ticks))))
      (:quit () t))))

(defun main ()
  (sdl2:with-init (:video)
    (sdl2:with-window (window :title "Demo" :w 640 :h 480 :flags '(:shown))
      (sdl2:with-renderer (renderer window)
        (initialize renderer)
        (with-game-loop
          (update)
          (draw renderer))))))

(defun clear-screen (renderer)
  (sdl2:set-render-draw-color renderer 255 255 255 255)
  (sdl2:render-clear renderer))

(defun draw (renderer)
  (clear-screen renderer)
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
        (update-animation sprite)))

(defmethod update-animation ((obj sprite))
  (with-slots (x w current total duration timer) (animation obj)
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
