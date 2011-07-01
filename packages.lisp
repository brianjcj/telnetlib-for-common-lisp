
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

