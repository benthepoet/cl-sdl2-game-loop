#|
  This file is a part of sdl-test
  (c) 2019 Ben Hanna (benpaulhanna@gmail.com)
  Author: Ben Hanna <benpaulhanna@gmail.com>
|#

(defpackage #:sdl-test
  (:use #:cl #:sdl2)
  (:export #:main))

(in-package #:sdl-test)

(defvar *line-offset*)
(defvar *line-speed*)
(defvar *next-tick*)
(defvar *skip-ticks*)

(defun initialize ()
  (setf *line-offset* 0)
  (setf *line-speed* 2)
  (setf *next-tick* 0)
  (setf *skip-ticks* (float (/ 1000 30))))

(defun main ()
  (initialize)
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Demo" :w 640 :h 480 :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:keyup
           (:keysym keysym)
           (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
             (sdl2:push-event :quit)))
          (:idle
           ()
           (draw renderer)
           (update))
          (:quit () t))))))

(defun draw (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer)
  (sdl2:set-render-draw-color renderer 255 255 255 255)
  (sdl2:render-draw-line renderer *line-offset* 256 (+ 128 *line-offset*) 256)
  (sdl2:render-present renderer))

(defun update ()
  (if (> (sdl2:get-ticks) *next-tick*)
      (progn
        (incf *line-offset* *line-speed*)
        (incf *next-tick* *skip-ticks*))))
