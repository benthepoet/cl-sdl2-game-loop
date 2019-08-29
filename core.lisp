#|
  This file is a part of sdl-test
  (c) 2019 Ben Hanna (benpaulhanna@gmail.com)
  Author: Ben Hanna <benpaulhanna@gmail.com>
|#
(in-package :cl-sdl2-game-loop)

(defmacro with-game-loop ((game-sym renderer-sym) &rest forms)
  `(let ((next-tick 0)
         (sleep-ticks 0)
         (skip-ticks (float (/ 1000 +frame-rate+)))
         (,game-sym (make-game ,renderer-sym)))
    (sdl2:with-event-loop (:method :poll)
      (:keyup
       (:keysym keysym)
       (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
         (sdl2:push-event :quit)))
      (:idle
       ()
       (setf next-tick (+ (sdl2:get-ticks) skip-ticks))
       ,@forms
       (setf sleep-ticks (- next-tick (sdl2:get-ticks)))
       (when (> sleep-ticks 0)
           (sdl2:delay (floor sleep-ticks))))
      (:quit () t))))

(defun main ()
  (sdl2:with-init (:video)
    (sdl2:with-window (window :title +game-title+ :w +screen-width+ :h +screen-height+ :flags '(:shown))
      (sdl2:with-renderer (renderer window)
        (with-game-loop (game renderer)
          (update game)
          (draw game))))))

(defmethod get-texture ((obj game) texture-key)
  (with-slots (renderer textures) obj
    (unless (gethash texture-key textures)
      (let ((surface (sdl2:load-bmp (texture-path texture-key))))
        (setf (gethash texture-key textures)
              (sdl2:create-texture-from-surface renderer surface))
        (sdl2:free-surface surface)))
    (gethash texture-key textures)))

(defun texture-path (texture-key)
  (format nil "~aassets/~a.bmp" +base-path+ (string-downcase texture-key)))

(defmethod draw ((obj game))
  (with-slots (renderer sprites) obj
    (sdl2:set-render-draw-color renderer 255 255 255 255)
    (sdl2:render-clear renderer)

    (dolist (sprite sprites)
      (draw-sprite obj sprite))

    (sdl2:render-present renderer)))

(defmethod draw-sprite ((obj game) sprite)
  (with-slots (x-offset y-offset frame-count frame-width frame-height) (get-animation sprite)
    (let ((source-rect (make-rect (+ x-offset (* frame-count frame-width)) y-offset frame-width frame-height))
          (dest-rect (make-rect
                      (scale (sprite-x-position sprite))
                      (scale (sprite-y-position sprite))
                      (scale frame-width)
                      (scale frame-height))))
      (render-texture obj (sprite-texture-key sprite) source-rect dest-rect))))

(defmethod render-texture ((obj game) texture-key source-rect dest-rect)
  (sdl2:render-copy (game-renderer obj)
                    (get-texture obj texture-key)
                    :source-rect source-rect
                    :dest-rect dest-rect))

(defmethod update ((obj game))
  (dolist (sprite (game-sprites obj))
    (update-animation sprite)))

(defmethod get-animation ((obj sprite))
  (cdr (car (sprite-animations obj))))

(defmethod change-animation ((obj sprite) key)
  (with-slots (animations) obj
    (let ((item (assoc key animations)))
      (when item
        (setf animations (cons item (remove item animations)))))))

(defmethod update-animation ((obj sprite))
  (with-slots (frame-count frame-total frame-duration frame-timer repeat) (get-animation obj)
    (when (> (incf frame-timer) frame-duration)
        (setf frame-timer 0)
        (when (= (incf frame-count) frame-total)
          (if repeat
              (setf frame-count 0)
              (decf frame-count))))))

(defun scale (x)
  (* x +scale-factor+))
