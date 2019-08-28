#|
  This file is a part of sdl-test
  (c) 2019 Ben Hanna (benpaulhanna@gmail.com)
  Author: Ben Hanna <benpaulhanna@gmail.com>
|#
(in-package :cl-sdl2-game-loop)

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
      (:quit () t))))

(defun main ()
  (sdl2:with-init (:video)
    (sdl2:with-window (window :title +game-title+ :w +screen-width+ :h +screen-height+ :flags '(:shown))
      (sdl2:with-renderer (renderer window)
        (let ((game-state (make-game-state renderer)))
          (with-game-loop
              (update game-state)
              (draw game-state)))))))

(defmethod get-texture ((obj game-state) texture-key)
  (with-slots (renderer textures) obj
    (unless (gethash texture-key textures)
      (let ((surface (sdl2:load-bmp (texture-path texture-key))))
        (setf (gethash texture-key textures)
              (sdl2:create-texture-from-surface renderer surface))
        (sdl2:free-surface surface)))
    (gethash texture-key textures)))

(defun texture-path (texture-key)
  (format nil "~aassets/~a.bmp" +base-path+ (string-downcase texture-key)))

(defmethod draw ((obj game-state))
  (with-slots (renderer sprites) obj
    (sdl2:set-render-draw-color renderer 255 255 255 255)
    (sdl2:render-clear renderer)

    (loop for sprite in (sprites obj)
          do (draw-sprite obj sprite))

    (sdl2:render-present renderer)))

(defmethod draw-sprite ((obj game-state) sprite)
  (with-slots (frame-count frame-width frame-height) (car (animations sprite))
    (let ((source-rect (make-rect (* frame-count frame-width) 0 frame-width frame-height))
          (dest-rect (make-rect (scale (x-position sprite)) (scale (y-position sprite)) (scale frame-width) (scale frame-height))))
      (render-texture obj (texture-key sprite) source-rect dest-rect))))

(defmethod render-texture ((obj game-state) texture-key source-rect dest-rect)
  (sdl2:render-copy (renderer obj)
                    (get-texture obj texture-key)
                    :source-rect source-rect
                    :dest-rect dest-rect))

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
