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

#+:sbcl (require :sb-bsd-sockets)

(defpackage :telnetlib
  (:use :cl #+:sbcl :sb-bsd-sockets)
  (:export :with-telnet-session
           :open-telnet-session
           :close-telnet-session
           :read-available-data
           :peek-available-data
           :read-until
           :read-until-2
           :read-until-2-ind
           :expect
           :format-tn
           :write-ln
           :write-ln-crlr
           :set-telnet-session-option
           :peek-sbdata
           :eof-tn
           :telnet-eof ;; Signal when eof has been read from the socket
                       ;; stream and no data bufferred in the telnet object.
           :telnet-read-timeout
           ))

