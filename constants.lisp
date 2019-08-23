(in-package :cl-sdl2-game-loop)

(defconstant +base-path+ (directory-namestring (asdf:system-source-directory :cl-sdl2-game-loop)))
(defconstant +frame-rate+ 60)
(defconstant +game-title+ "Demo")
(defconstant +scale-factor+ 3)
(defconstant +screen-width+ 640)
(defconstant +screen-height+ 480)
