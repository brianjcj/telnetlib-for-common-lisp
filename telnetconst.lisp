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

(in-package :telnetlib)


;; Telnet protocol defaults
(defconstant +TELNET-PORT+ 23)

;; Telnet protocol characters (don't change)
(defconstant +IAC+ (code-char 255)) ;;  "Interpret As Command"
(defconstant +DONT+ (code-char 254))
(defconstant +DO+ (code-char 253))
(defconstant +WONT+ (code-char 252))
(defconstant +WILL+ (code-char 251))
(defconstant +theNULL+ (code-char 0))

(defconstant +SE+ (code-char 240))  ;;  Subnegotiation End
(defconstant +NOP+ (code-char 241))  ;;  No Operation
(defconstant +DM+ (code-char 242))  ;;  Data Mark
(defconstant +BRK+ (code-char 243))  ;;  Break
(defconstant +IP+ (code-char 244))  ;;  Interrupt process
(defconstant +AO+ (code-char 245))  ;;  Abort output
(defconstant +AYT+ (code-char 246))  ;;  Are You There
(defconstant +EC+ (code-char 247))  ;;  Erase Character
(defconstant +EL+ (code-char 248))  ;;  Erase Line
(defconstant +GA+ (code-char 249))  ;;  Go Ahead
(defconstant +SB+ (code-char 250))  ;;  Subnegotiation Begin


;; Telnet protocol options code (don't change)
;; These ones all come from arpa/telnet.h
(defconstant +BINARY+ (code-char 0)) ;;  8-bit data path
(defconstant +ECHO+ (code-char 1)) ;;  echo
(defconstant +RCP+ (code-char 2)) ;;  prepare to reconnect
(defconstant +SGA+ (code-char 3)) ;;  suppress go ahead
(defconstant +NAMS+ (code-char 4)) ;;  approximate message size
(defconstant +STATUS+ (code-char 5)) ;;  give status
(defconstant +TM+ (code-char 6)) ;;  timing mark
(defconstant +RCTE+ (code-char 7)) ;;  remote controlled transmission and echo
(defconstant +NAOL+ (code-char 8)) ;;  negotiate about output line width
(defconstant +NAOP+ (code-char 9)) ;;  negotiate about output page size
(defconstant +NAOCRD+ (code-char 10)) ;;  negotiate about CR disposition
(defconstant +NAOHTS+ (code-char 11)) ;;  negotiate about horizontal tabstops
(defconstant +NAOHTD+ (code-char 12)) ;;  negotiate about horizontal tab disposition
(defconstant +NAOFFD+ (code-char 13)) ;;  negotiate about formfeed disposition
(defconstant +NAOVTS+ (code-char 14)) ;;  negotiate about vertical tab stops
(defconstant +NAOVTD+ (code-char 15)) ;;  negotiate about vertical tab disposition
(defconstant +NAOLFD+ (code-char 16)) ;;  negotiate about output LF disposition
(defconstant +XASCII+ (code-char 17)) ;;  extended ascii character set
(defconstant +LOGOUT+ (code-char 18)) ;;  force logout
(defconstant +BM+ (code-char 19)) ;;  byte macro
(defconstant +DET+ (code-char 20)) ;;  data entry terminal
(defconstant +SUPDUP+ (code-char 21)) ;;  supdup protocol
(defconstant +SUPDUPOUTPUT+ (code-char 22)) ;;  supdup output
(defconstant +SNDLOC+ (code-char 23)) ;;  send location
(defconstant +TTYPE+ (code-char 24)) ;;  terminal type
(defconstant +EOR+ (code-char 25)) ;;  end or record
(defconstant +TUID+ (code-char 26)) ;;  TACACS user identification
(defconstant +OUTMRK+ (code-char 27)) ;;  output marking
(defconstant +TTYLOC+ (code-char 28)) ;;  terminal location number
(defconstant +VT3270REGIME+ (code-char 29)) ;;  3270 regime
(defconstant +X3PAD+ (code-char 30)) ;;  X.3 PAD
(defconstant +NAWS+ (code-char 31)) ;;  window size
(defconstant +TSPEED+ (code-char 32)) ;;  terminal speed
(defconstant +LFLOW+ (code-char 33)) ;;  remote flow control
(defconstant +LINEMODE+ (code-char 34)) ;;  Linemode option
(defconstant +XDISPLOC+ (code-char 35)) ;;  X Display Location
(defconstant +OLD_ENVIRON+ (code-char 36)) ;;  Old - Environment variables
(defconstant +AUTHENTICATION+ (code-char 37)) ;;  Authenticate
(defconstant +ENCRYPT+ (code-char 38)) ;;  Encryption option
(defconstant +NEW_ENVIRON+ (code-char 39)) ;;  New - Environment variables
;; the following ones come from
;; http://www.iana.org/assignments/telnet-options
;; Unfortunately, that document does not assign identifiers
;; to all of them, so we are making them up
(defconstant +TN3270E+ (code-char 40)) ;;  TN3270E
(defconstant +XAUTH+ (code-char 41)) ;;  XAUTH
(defconstant +CHARSET+ (code-char 42)) ;;  CHARSET
(defconstant +RSP+ (code-char 43)) ;;  Telnet Remote Serial Port
(defconstant +COM_PORT_OPTION+ (code-char 44)) ;;  Com Port Control Option
(defconstant +SUPPRESS_LOCAL_ECHO+ (code-char 45)) ;;  Telnet Suppress Local Echo
(defconstant +TLS+ (code-char 46)) ;;  Telnet Start TLS
(defconstant +KERMIT+ (code-char 47)) ;;  KERMIT
(defconstant +SEND_URL+ (code-char 48)) ;;  SEND-URL
(defconstant +FORWARD_X+ (code-char 49)) ;;  FORWARD_X
(defconstant +PRAGMA_LOGON+ (code-char 138)) ;;  TELOPT PRAGMA LOGON
(defconstant +SSPI_LOGON+ (code-char 139)) ;;  TELOPT SSPI LOGON
(defconstant +PRAGMA_HEARTBEAT+ (code-char 140)) ;;  TELOPT PRAGMA HEARTBEAT
(defconstant +EXOPL+ (code-char 255)) ;;  Extended-Options-List
(defconstant +NOOPT+ (code-char 0))

