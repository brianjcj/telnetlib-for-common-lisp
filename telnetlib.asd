;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

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
               #-(or :lispworks :allegro :sbcl :clisp) :usocket
               #-(or :lispworks :allegro :sbcl :clisp) :flexi-streams))