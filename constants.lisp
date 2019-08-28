(in-package :cl-sdl2-game-loop)

(defparameter +base-path+ (directory-namestring (asdf:system-source-directory :cl-sdl2-game-loop)))
(defparameter +frame-rate+ 60)
(defparameter +game-title+ "Demo")
(defparameter +scale-factor+ 3)
(defparameter +screen-width+ 640)
(defparameter +screen-height+ 480)
