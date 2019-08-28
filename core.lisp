#|
  This file is a part of sdl-test
  (c) 2019 Ben Hanna (benpaulhanna@gmail.com)
  Author: Ben Hanna <benpaulhanna@gmail.com>
|#
(in-package :cl-sdl2-game-loop)

(defvar *renderer*)
(defvar *textures*)

(defmacro with-game-loop (&rest body)
  `(let ((next-tick 0)
         (sleep-ticks 0)
         (skip-ticks (float (/ 1000 +frame-rate+))))
    (sdl2:with-event-loop (:method :poll)
      (:keyup
       (:keysym keysym)
       (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
         (sdl2:push-event :quit)))
      (:idle
       ()
       (setf next-tick (+ (sdl2:get-ticks) skip-ticks))
       ,@body
       (setf sleep-ticks (- next-tick (sdl2:get-ticks)))
       (when (> sleep-ticks 0)
           (sdl2:delay (floor sleep-ticks))))
      (:quit
       ()
       (setf *textures* nil)
       t))))

(defun main ()
  (sdl2:with-init (:video)
    (sdl2:with-window (window :title +game-title+ :w +screen-width+ :h +screen-height+ :flags '(:shown))
      (sdl2:with-renderer (renderer window)
        (setf *renderer* renderer *textures* (make-hash-table))
        (let ((game-state (make-game-state)))
          (with-game-loop
              (update game-state)
              (draw game-state)))))))

(defun get-texture (texture-key)
  (unless (gethash texture-key *textures*)
    (let ((surface (sdl2:load-bmp (texture-path texture-key))))
      (setf (gethash texture-key *textures*)
            (sdl2:create-texture-from-surface *renderer* surface))
      (sdl2:free-surface surface)))
  (gethash texture-key *textures*))

(defun texture-path (texture-key)
  (format nil "~aassets/~a.bmp" +base-path+ (string-downcase texture-key)))

(defun clear-screen ()
  (sdl2:set-render-draw-color *renderer* 255 255 255 255)
  (sdl2:render-clear *renderer*))

(defmethod draw ((obj game-state))
  (clear-screen)
  (loop for sprite in (sprites obj)
        do (draw-sprite sprite))
  (sdl2:render-present *renderer*))

(defmethod draw-sprite ((obj sprite))
  (with-slots (frame-count frame-width frame-height) (car (animations obj))
    (let ((source-rect (make-rect (* frame-count frame-width) 0 frame-width frame-height))
          (dest-rect (make-rect (scale (x-position obj)) (scale (y-position obj)) (scale frame-width) (scale frame-height)))
          (texture (get-texture (texture-key obj))))
      (sdl2:render-copy *renderer* texture :source-rect source-rect :dest-rect dest-rect))))

(defmethod update ((obj game-state))
  (loop for sprite in (sprites obj) do
        (update-animation (car (animations sprite)))))

(defmethod update-animation ((obj animation))
  (with-slots (frame-count frame-total frame-duration frame-timer repeat) obj
    (when (> (incf frame-timer) frame-duration)
        (setf frame-timer 0)
        (when (= (incf frame-count) frame-total)
          (if repeat
              (setf frame-count 0)
              (decf frame-count))))))

(defun scale (x)
  (* x +scale-factor+))
