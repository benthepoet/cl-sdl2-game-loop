#|
 This file is a part of sdl-test
 (c) 2019 Ben Hanna (benpaulhanna@gmail.com)
 Author: Ben Hanna <benpaulhanna@gmail.com>
|#

(in-package #:cl-user)
(asdf:defsystem sdl-test
  :version "0.0.0"
  :license "BSD-3"
  :author "Ben Hanna <benpaulhanna@gmail.com>"
  :maintainer "Ben Hanna <benpaulhanna@gmail.com>"
  :description ""
  :serial T
  :components ((:file "package"))
  :depends-on (:sdl2))
