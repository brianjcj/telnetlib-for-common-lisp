;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;; Copyright (C) 2007  Brian Jiang

;; Author: Brian Jiang <brianjcj@gmail.com>
;; Version: 1.0

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You can receive a copy of the GNU Lesser General Public License from
;; http://www.gnu.org/

(in-package :cl-user)

(defpackage :telnetlib-asd
  (:use :cl :asdf))

(in-package :telnetlib-asd)


(defsystem :telnetlib
  :serial t
  :version "0.1"
  :components ((:file "packages")
               (:file "telnetconst")
               (:file "telnetlib"))
  :depends-on (:cl-ppcre
               #+sbcl :sb-bsd-sockets
               #-(or :lispworks :allegro :sbcl :clisp) :usocket
               #-(or :lispworks :allegro :sbcl :clisp) :flexi-streams))
