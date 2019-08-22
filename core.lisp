#|
  This file is a part of sdl-test
  (c) 2019 Ben Hanna (benpaulhanna@gmail.com)
  Author: Ben Hanna <benpaulhanna@gmail.com>
|#
(in-package :cl-sdl2-game-loop)

(defun make-state (renderer)
  (make-instance 'game-state
                 :renderer renderer
                 :textures (make-hash-table :test #'equal)
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
                                                                                :texture-path "assets/run.bmp")))))

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
       (setf next-tick (+ (sdl2:get-ticks) skip-ticks))
       ,@body
       (setf sleep-ticks (- next-tick (sdl2:get-ticks)))
       (when (> sleep-ticks 0)
           (sdl2:delay (floor sleep-ticks))))
      (:quit () t))))

(defun main ()
  (sdl2:with-init (:video)
    (sdl2:with-window (window :title "Demo" :w 640 :h 480 :flags '(:shown))
      (sdl2:with-renderer (renderer window)
        (let ((state (make-state renderer)))
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
  (with-slots (x y w h texture-path) (animation sprite)
    (let ((source-rect (make-rect x y w h))
          (dest-rect (make-rect 0 0 (scale w) (scale h)))
          (renderer (renderer state))
          (texture (gethash texture-path (textures state))))
      (sdl2:render-copy renderer texture :source-rect source-rect :dest-rect dest-rect))))

(defun update (state)
  (loop for sprite in (sprites state) do
        (update-animation sprite)))

(defmethod update-animation ((obj sprite))
  (with-slots (x w current total duration timer) (animation obj)
    (when (> (incf timer) duration)
        (setf timer 0)
        (when (= (incf current) total)
          (setf current 0))
        (setf x (* current w)))))

(defun scale (x)
  (* x 3))
