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

;;; Commentary:
;;
;; Based on RFC 854: TELNET Protocol Specification, by J. Postel and
;; J. Reynolds

;; In our ordinary work, we often write some automatically scripts
;; which login in the server to run some commands and performce some
;; actions based on the result of the commands. In this case, a telnet
;; client library will be very helpful. Such a library is very handy
;; in Perl, Python and Ruby. But I googled it a lot and failed to find
;; one in Common Lisp. So I decided to port the Telnetlib from Python
;; to Common Lisp. Why port from Python? Because I am more familiar
;; with Python and have use its TelnetLib before :-)

;; The functionality of this library is almost the same as Python's
;; one.  But the interface is a little different.


(in-package :telnetlib)


(defclass Telnet ()
  ((host :initarg :host
         :accessor host)
   (port :initarg :port
         :accessor port
         :initform +TELNET-PORT+)
   (sock-stream :initarg :sock-stream
         :accessor sock-stream)
   (cookedq :accessor cookedq
            :initform (make-array 4096
                                  :element-type 'character
                                  :fill-pointer 0
                                  :adjustable t))
   (eof :accessor eof
        :initform nil)
   (iacseq :accessor iacseq
           :initform (make-array 3
                                 :element-type 'character
                                 :fill-pointer 0))
   (sb :accessor sb
       :initform 0)
   (sbdataq :accessor sbdataq
            :initform (make-array 64
                                  :element-type 'character
                                  :fill-pointer 0
                                  :adjustable t))
   (option-callback :initarg :option-callback
                    :accessor option-callback
                    ;; :initform #'default-option-callback%)
                    :initform nil)
   (sb-option-callback :initarg :sb-option-callback
                       :accessor sb-option-callback
                       ;; :initform #'default-sb-option-callback%)
                       :initform nil)
   (char-callback :initarg :char-callback
                  :accessor char-callback
                  :initform #'(lambda (c s)
                                     (declare (ignore s))
                                     (princ c)))

   (remove-return-char :initarg :remove-return-char
                       :accessor remove-return-char
                       :initform t)
   
   (debug-on :accessor debug-on :initform nil)

   ))

(defvar *tn-internal-debug* nil)

(defun open-telnet-session (host &optional port)
  (let* ((port (or port +TELNET-PORT+))
         #+:sbcl (sock (make-instance
                        'inet-socket
                        :type :stream :protocol :tcp))
         (tn (make-instance 'Telnet
                            :host host
                            :port port

                            :sock-stream
                            #-(or allegro lispworks sbcl clisp)
                            (usocket:socket-stream
                             (usocket:socket-connect host port
                                                     :element-type '(unsigned-byte 8)
                                                     ))
                            
                            #+:allegro (socket:make-socket :remote-host host :remote-port port)
                            #+:lispworks (comm:open-tcp-stream host port)
                            #+:sbcl (socket-make-stream
                                     sock :input t :output t :buffering :full
                                     :element-type 'character :external-format :iso-8859-1)
                            #+:clisp (socket:socket-connect
                                      port host
                                      :element-type 'character
                                      :external-format charset:iso-8859-1
                                      :buffered t)
                            )))

    (if *tn-internal-debug*
        (set-telnet-session-option tn :option-callback #'default-option-callback%
                                   :sb-option-callback #'default-sb-option-callback%
                                   ))

    #-(or allegro lispworks sbcl clisp)
    (with-slots (sock-stream) tn
      (setf sock-stream (flexi-streams:make-flexi-stream sock-stream
                                                         :external-format
                                                         :latin-1
                                                         )))
    #+:sbcl
    (socket-connect
     sock (car (host-ent-addresses (get-host-by-name host)))
     port)
    
    tn))

(defun close-telnet-session (tn)
  (when (and tn
             (sock-stream tn))
    (close (sock-stream tn))
    (if (debug-on tn)
        (format t "~%Telnet stream closed.~%"))
    ))


(defmacro with-telnet-session ((tn host &optional port) &body body)
  `(let ((,tn (open-telnet-session ,host ,port)))
     (unwind-protect
          (progn
            ,@body)
       (close-telnet-session ,tn))))


(defun set-telnet-session-option (tn
                                  &key (remove-return-char nil r-r-c-p)
                                  (debug-on nil debug-on-p)
                                  (char-callback nil char-callback-p)
                                  (option-callback nil option-callback-p)
                                  (sb-option-callback nil sb-option-callback-p))
  (when r-r-c-p
    (setf (remove-return-char tn) remove-return-char))
  (when debug-on-p
    (setf (debug-on tn) debug-on))
  (when char-callback-p
    (setf (char-callback tn) char-callback))
  (when option-callback-p
    (setf (option-callback tn) option-callback))
  (when sb-option-callback-p
    (setf (sb-option-callback tn) sb-option-callback)))

(defun send-sub-terminal-type-is% (s &optional (ttype "UNKNOWN"))
  (format s "~C~C~C~C~A~C~C"
          +IAC+ +SB+ +TTYPE+ (code-char 0) ttype +IAC+ +SE+)
  (format t "~%(~d)(~d)(~d)(~d)~A(~d)(~d)"
          (char-code +IAC+) (char-code +SB+) (char-code +TTYPE+) 0 ttype (char-code +IAC+) (char-code +SE+))
  (force-output s)
  )
;; NETWORK-VIRTUAL-TERMINAL

(defun default-option-callback% (s cmd code)
  ""
  (cond ((char= cmd +DO+)
         (cond ((char= code +TTYPE+)
                (format s "~C~C~C" +IAC+ +WILL+ +TTYPE+)
                ;; brian
                (format t "send back: WILL!~%")
                (force-output s)
                (return-from default-option-callback% nil)
                ))
         ))
  (let (cc ok)
    (cond ((or (char= cmd +WILL+)
               (char= cmd +WONT+))
           (setq cc +DONT+)
           (format t "DONT ")
           (setq ok t)
           )
          ((or (char= cmd +DO+)
               (char= cmd +DONT+))
           (setq cc +WONT+)
           (format t "WONT ")
           (setq ok t)))
    
    (if ok
        (progn (format s "~C~C~C" +IAC+ cc code)
               (format t "Send back!~%")
               (force-output s))
        (progn
          (format t "IAC ~d not recognized" (char-code cmd))
          ))))

(defun default-sb-option-callback% (s sbdata)
  ""
  (if (and (char= (aref sbdata 0) +TTYPE+)
           (char= (aref sbdata 1) (code-char 1)))
      (send-sub-terminal-type-is% s)
      ))

(define-condition telnet-eof (simple-condition)())
(define-condition telnet-read-timeout (simple-condition)())

(defun process-sock-stream% (tn &optional (block-read nil))
  "Internal procedure to read all the available data from the socket stream
to cookedq.
Return :no-data when no data filled the cookeq.
Return :old-data when no new data read from socket stream but cookeq has old data.
Return :new-data when new data are read from socket stream to cookeq."
  (with-slots (sock-stream
               sb iacseq sbdataq eof
               cookedq option-callback char-callback
               debug-on remove-return-char
               sb-option-callback) tn
    (let (c
          cmd opt
          (len (length cookedq)))

      (unless eof
        (if block-read
            (progn
              (setq c (read-char sock-stream nil :eof))
              (when (eq c :eof)
                #+:lispworks
                (progn
                  (if debug-on (print "*** EOF ***? check whether it is eof or timeout"))
                  (setq c (read-char-no-hang sock-stream nil :eof))
                  (cond ((NULL c)
                         ;; Not eof. Timeout
                         (signal 'telnet-read-timeout)
                         )
                        ((eq c :eof)
                         ;; real eof
                         (setf eof t))
                        (t
                         ;; a char available now? not likely the call can fall down here.
                         )))
                
                #-:lispworks
                (setf eof t)
                ))
            (progn
              (setq c (read-char-no-hang sock-stream nil :eof))
              (when (eq c :eof)
                (setf eof t)
                ))))
      
      (when (or eof (NULL c))
        (if (= 0 len)
            (progn
              (if eof (signal 'telnet-eof))
              (return-from process-sock-stream% :no-data)
              )
            (return-from process-sock-stream% :old-data)))

      (loop do
           (case (length iacseq)
             ((0) ;; length of iacseq
              (cond 
                ((char= c +theNULL+))
                ((char= c (code-char 21)))
                ((char= c +IAC+)
                 ;; option start...
                 (vector-push-extend c iacseq)
                 )
                (t
                 (if (= sb 0)
                     (unless (and remove-return-char
                                  (char= c #\Return))
                       (if char-callback
                           (funcall char-callback c sock-stream))
                       (vector-push-extend c cookedq 1024))
                     (vector-push-extend c sbdataq 64))
                 )))
             ((1) ;; length of iacseq
              (if (find c (list +DO+ +DONT+ +WILL+ +WONT+))
                  (vector-push-extend c iacseq 1024)
                  ;;else
                  (progn
                    (setf (fill-pointer iacseq) 0)
                    (cond
                      ;; +IAC+ +IAC+
                      ((char= c +IAC+)
                       (if (= sb 0)
                           (vector-push-extend c cookedq 1024)
                           (vector-push-extend c sbdataq 64))
                       )
                      ;; +IAC+ +SB+
                      ((char= c +SB+)
                       (if debug-on (format t "~%........SB........~%"))
                       (setf sb 1)
                       (setf (fill-pointer sbdataq) 0)
                       )
                      ;; +IAC+ +SE+
                      ((char= c +SE+)
                       (setf sb 0)
                       (if (and debug-on
                                (/= 0 (length sbdataq)))
                           (format t "sbdta: ~A" sbdataq))

                       (if sb-option-callback
                           (funcall sb-option-callback sock-stream sbdataq))
                       
                       (if debug-on (format t "~%........SE........~%"))
                       )
                      (t
                       (if option-callback
                           (funcall option-callback sock-stream c +NOOPT+)
                           (if debug-on
                               (format t "IAC ~d not recognized" (char-code c)))
                           ))))))
             ((2) ;; length of iacseq
              (setq cmd (aref iacseq 1))
              (setf (fill-pointer iacseq) 0)
              (setq opt c)
              (cond
                ((or (char= cmd +DO+)
                     (char= cmd +DONT+))
                 (if debug-on
                     (format t "IAC ~s ~d~%" (if (char= cmd +DO+) "DO" "DONT") (char-code opt)))
                 (if option-callback
                     (funcall option-callback sock-stream cmd opt)
                     (progn
                       (format sock-stream "~C~C~C" +IAC+ +WONT+ opt)
                       (force-output sock-stream)
                       )))
                ((or (char= cmd +WILL+)
                     (char= cmd +WONT+))
                 (if debug-on
                     (format t "IAC ~s ~d~%" (if (char= cmd +WILL+) "WILL" "WONT") (char-code opt)))
                 (if option-callback
                     (funcall option-callback sock-stream cmd opt)
                     (progn 
                       (format sock-stream "~C~C~C" +IAC+ +DONT+ opt)
                       (force-output sock-stream)
                       ))))))
           (setq c (read-char-no-hang sock-stream nil :eof))
           (when (eq c :eof)
             (setf eof t)
             (return))
           until (NULL c)
           )
      
      (if (/= len (length cookedq))
          :new-data
          (if (= 0 len)
              (progn
                (if eof (signal 'telnet-eof))
                :no-data)
              :old-data)))))

(defun peek-available-data (tn &optional block-read)
  "Just take a look at the available data."
  (with-slots (cookedq) tn
    (process-sock-stream% tn block-read)
    (copy-seq cookedq)
    ))

(defun read-available-data (tn &optional block-read)
  "Read all the available data. If block-read is not nil, block
the reading until at least a char arrives at the socket stream."
  (with-slots (cookedq) tn
    (process-sock-stream% tn block-read)
    (prog1
        (copy-seq cookedq)
      (setf (fill-pointer cookedq) 0))
    ))

(defun peek-sbdata (tn)
  ""
  (with-slots (sbdataq) tn
    (when (> (length sbdataq) 0)
      (copy-seq sbdataq)
    )))

(defun eof-tn (tn)
  (and (eof tn)
       (= 0 (length (cookedq tn)))))

(defun read-cookedq% (tn end-pos)
  "Read the cookedq vector from 0 to end-pos."
  (with-slots (cookedq) tn
    (let (ret-str)
      (setq ret-str (subseq cookedq 0 end-pos))
      (loop
         with j = 0
         for i from end-pos to (1- (length cookedq))
         do
         (setf (aref cookedq j) (aref cookedq i))
         (incf j)
         finally (setf (fill-pointer cookedq) j))
      ret-str)))

(defun char-equal-case-insensitive% (c1 c2)
  (or (char= c1 c2)
      (let ((code1 (logior 32 (char-code c1)))
            (code2 (logior 32 (char-code c2))))
        (and (>= code1 (char-code #\a))
             (<= code1 (char-code #\z))
             (= code1 code2)))))


(defun wait-until-readable% (stream &optional (timeout 600) debug-on)
  (let (ret)
    #+:sbcl
    (setq ret (sb-sys:wait-until-fd-usable (sb-sys:fd-stream-fd stream)
                                 :input timeout))
    #+:clisp
    (setq ret (socket:socket-status (cons stream :input) timeout))


    #+:allegro
    (progn
      (sys:with-timeout (timeout (if debug-on (print "timeout allegro"))
                                 (setq ret nil)
                                 (signal 'telnet-read-timeout)
                                 )
        (setq ret (peek-char nil stream nil :eof))
        ))

    (when debug-on
      (print timeout)
      (format t "~%*** Wait result (~A) ***~%" ret))

    (cond ((eq ret :eof)
           (signal 'telnet-eof))
          (;; actually cannot tell it is time out or eof from ret
           ;; in sbcl.
           (NULL ret)
           (signal 'telnet-read-timeout)))
    ret
    ))


(defun read-until (tn str &key (timeout 600) case-insensitive-mode)
  "Read until a given string is encountered or until timeout.
When no match is found, return nil with a :eof or :timeout, and
the all the received data will still stay in the cookedq vector."
  (with-slots (cookedq eof sock-stream debug-on) tn
    (let* (start-time
           elapsed
           (block-read (if timeout nil t))
           pos
           ;; Don't block the fist time of process-sock-stream%,
           ;; since there may be some data ready in the cookedq.
           (read-status (process-sock-stream% tn nil))
           data-len
           (search-start 0)
           (str-len (length str))
           (char-test (if case-insensitive-mode #'char-equal-case-insensitive% #'eql))
           )
      (if timeout (setq start-time (get-universal-time)))

      (loop
         do
         (setq data-len (length cookedq))
         (when (>= (- data-len search-start) str-len)
           (if (setq pos (search str cookedq :start2 search-start :test char-test))
               (progn
                 (setq pos (+ pos (length str)))
                 (return-from read-until (values (read-cookedq% tn pos) :ok)))
               ;; Save the position that we will start searching from it.
               (setq search-start (+ 2 (- data-len 1 str-len)))
               ))
        
         (when eof
           (print "eof!!")
           (return-from read-until (values nil :eof)))
           
         ;; loop to wait for the new data...
         (loop do

              (when timeout
                (setq elapsed (- (get-universal-time) start-time))
                (when (>= elapsed timeout)
                  (print "Timeout!")
                  (return-from read-until (values nil :timeout)))

                #+:lispworks
                (progn (setf (stream:stream-read-timeout sock-stream) (- timeout elapsed))
                       (setq block-read t)
                       (print (- timeout elapsed)))
                
                #+(or sbcl clisp allegro)
                (case (wait-until-readable% sock-stream (- timeout elapsed) debug-on)
                  ((:eof)
                   (setf eof t)
                   (return-from read-until (values nil :eof)))
                  ((nil) (return-from read-until (values nil :timeout)))
                  )

                #-(or lispworks sbcl clisp allegro)
                (sleep 0.25)
                )
              
              (setq read-status (process-sock-stream% tn block-read))
              (when (eq read-status :new-data)                    
                (return))
              (when eof
                (print "eof!")
                (return-from read-until (values nil :eof)))              
              
              )))))

(defun read-until-2 (tn strings &key (timeout 600) case-insensitive-mode)
  "Read until one from a list of strings is encountered or until timeout.
When no match is found, return nil with a :eof or :timeout, and
the all the received data will still stay in the cookedq vector."

  (if (= 1 (length strings))
      (read-until tn (elt strings 0) :timeout timeout :case-insensitive-mode case-insensitive-mode)
 
      (with-slots (cookedq eof sock-stream debug-on) tn
        (let* (start-time
               elapsed
               (block-read (if timeout nil t))
               (char-test (if case-insensitive-mode #'char-equal-case-insensitive% #'eql))
               ;; Don't block the fist time of process-sock-stream%,
               ;; since there may be some data ready in the cookedq.
               (read-status (process-sock-stream% tn nil))
               (work-list (loop for s in strings collect (list 0 s (length s))))
               data-len
               ;; match-start
               match-end match-ind)

          (if timeout (setq start-time (get-universal-time)))
      
          (loop do
               (setq data-len (length cookedq))
               (loop
                  with j = 0
                  with pos-s
                  with pos-e
                  for (search-start str len) in work-list
                  for item in work-list
                  do
                  (when (>= (- data-len search-start) len)
                    (if (setq pos-s (search str cookedq :start2 search-start :test char-test))
                        (progn
                          (setq pos-e (+ pos-s (length str)))
                          

                          (when (or (NULL match-end)
                                    (< pos-e match-end))
                            
                            ;; If no match before and the match end of current match is less
                            ;; than the previous match, then update the match result using
                            ;; current match.
                            (setq match-end pos-e
                                  ;; match-start pos-s
                                  match-ind j)))
                        
                        ;; If no match for current string, save the position that
                        ;; we will start searching from it for this string next time.
                        (setf (car item) (+ 2 (- data-len 1 len)))))
                    
                  (incf j))

               (when match-end
                 ;; Match!
                 (return-from read-until-2 (values (read-cookedq% tn match-end) :ok match-ind)))
               
               (when eof
                 (return-from read-until-2 (values nil :eof)))

             ;; Wait for new data....
               (loop do

                    (when timeout
                      (setq elapsed (- (get-universal-time) start-time))
                      (when (>= elapsed timeout)
                        (return-from read-until-2 (values nil :timeout)))

                      #+:lispworks
                      (progn (setf (stream:stream-read-timeout sock-stream) (- timeout elapsed))
                             (setq block-read t)
                             (print (- timeout elapsed)))
                
                      #+(or sbcl clisp allegro)
                      (case (wait-until-readable% sock-stream (- timeout elapsed) debug-on)
                        ((:eof)
                         (setf eof t)
                         (return-from read-until-2 (values nil :eof)))
                        ((nil) (return-from read-until-2 (values nil :timeout)))
                        )

                      #-(or lispworks sbcl clisp allegro)
                      (sleep 0.25)
                      )

                    (setq read-status (process-sock-stream% tn block-read))
                    (when (eq read-status :new-data)
                      (return))
                    (when eof
                      (return-from read-until-2 (values nil :eof)))

                    ))))))

(defun read-until-2-ind (tn strings &key (timeout 600) case-insensitive-mode)
  "Sometimes, we more care about which string is matched."
  (multiple-value-bind (str res index)
      (read-until-2 tn strings :timeout timeout :case-insensitive-mode case-insensitive-mode)
    (values index str res)
    ))


(defun expect (tn regexp &optional (timeout 600))
  "Read until one from a list of a regular expressions matches or until timeout.
When no match is found, return nil with a :eof or :timeout, and
the all the received data will still stay in the cookedq vector."
  (with-slots (cookedq eof sock-stream debug-on) tn
    (let* (start-time
           elapsed
           (block-read (if timeout nil t))
           ;; Don't block the fist time of process-sock-stream%,
           ;; since there may be some data ready in the cookedq.
           (read-status (process-sock-stream% tn nil))
           mat-start
           mat-end
           mat-text)
      (if timeout (setq start-time (get-universal-time)))
      (unless (eq read-status :no-data)
        (when (multiple-value-setq (mat-start mat-end) (cl-ppcre:scan regexp cookedq))
          (setq mat-text (subseq cookedq mat-start mat-end))
          (return-from expect (values (read-cookedq% tn mat-end) :ok mat-text))
          ))
      (loop do
           (when eof
             (return-from expect (values nil :eof)))
           (when timeout
             (setq elapsed (- (get-universal-time) start-time))
             (when (>= elapsed timeout)
               (return-from expect (values nil :timeout)))

             #+:lispworks
             (progn (setf (stream:stream-read-timeout sock-stream) (- timeout elapsed))
                    (setq block-read t)
                    (print (- timeout elapsed)))
                
             #+(or sbcl clisp allegro)
             (case (wait-until-readable% sock-stream (- timeout elapsed) debug-on)
               ((:eof)
                (setf eof t)
                (return-from expect (values nil :eof)))
               ((nil) (return-from expect (values nil :timeout)))
               )

             #-(or lispworks sbcl clisp allegro)
             (sleep 0.25)
             )
           
           (setq read-status (process-sock-stream% tn block-read))
           (when (eq read-status :new-data)
             (when (multiple-value-setq (mat-start mat-end) (cl-ppcre:scan regexp cookedq))
               (setq mat-text (subseq cookedq mat-start mat-end))
               (return-from expect (values (read-cookedq% tn mat-end) :ok mat-text))
               ))))))

(defun format-tn (tn control-string &rest format-arguments)
  (apply #'format (sock-stream tn) control-string format-arguments)
  (force-output (sock-stream tn)))

(defun write-ln (tn str)
  (format (sock-stream tn) "~a~%" str)
  (force-output (sock-stream tn))
  ;; (finish-output (sock-stream tn))
  )

(defun write-ln-crlr (tn str)
  (format-tn tn "~A~C~C" str (code-char 13) #\Newline))


