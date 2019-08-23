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
        (let ((state (make-game-state renderer)))
          (load-textures state)
          (with-game-loop
              (update state)
              (draw state)))))))

(defun load-textures (state)
  (loop for sprite in (sprites state)
        do (let ((texture-path (texture-path (animation sprite)))
                 (textures (textures state)))
             (unless (gethash texture-path textures)
               (setf (gethash texture-path textures)
                     (sdl2:create-texture-from-surface (renderer state)
                                                       (sdl2:load-bmp (format nil "~a~a" +base-path+ texture-path))))))))

(defun clear-screen (renderer)
  (sdl2:set-render-draw-color renderer 255 255 255 255)
  (sdl2:render-clear renderer))

(defun draw (state)
  (let ((renderer (renderer state)))
    (clear-screen renderer)
    (loop for sprite in (sprites state)
          do (draw-sprite state sprite))
    (sdl2:render-present renderer)))

(defun draw-sprite (state sprite)
  (with-slots (frame-count frame-width frame-height texture-path) (animation sprite)
    (let ((source-rect (make-rect (* frame-count frame-width) 0 frame-width frame-height))
          (dest-rect (make-rect 0 0 (scale frame-width) (scale frame-height)))
          (renderer (renderer state))
          (texture (gethash texture-path (textures state))))
      (sdl2:render-copy renderer texture :source-rect source-rect :dest-rect dest-rect))))

(defun update (state)
  (loop for sprite in (sprites state) do
        (update-animation (animation sprite))))

(defmethod update-animation ((obj animation))
  (with-slots (frame-width frame-count frame-total frame-duration frame-timer repeat) obj
    (when (> (incf frame-timer) frame-duration)
        (setf frame-timer 0)
        (unless (= frame-count frame-total)
          (incf frame-count)))
    (when (= frame-count frame-total)
      (if repeat
          (setf frame-count 0)
          (decf frame-count)))))

(defun scale (x)
  (* x +scale-factor+))
