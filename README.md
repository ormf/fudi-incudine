### OVERVIEW

fudi is a common lisp package which establishes a bidirectional
connection to pd for incudine using the FUDI protocol
(see: https://en.wikipedia.org/wiki/FUDI).

(c) by Orm Finnendahl 2016, released under the GPL v2.0, see file
LICENSE no warranties whatsoever.

### PREREQUISITES

- sbcl Common Lisp (http://www.sbcl.org/) installed.
- Quicklisp (https://www.quicklisp.org/beta/) installed.
- Pure Data (http://msp.ucsd.edu/software.html) installed.
- incudine (http://incudine.sourceforge.net/) installed.


### INSTALL

1. Link the folder "fudi-incudine" into "~/quicklisp/local-projects/".


### USAGE

1. Load the project: 

```
(ql:quickload "fudi")
```

2. Start the pd patch "incudine-fudi-test.pd".

3. Start inbound and outbound connections:

```
(in-package :incudine.scratch)

(defvar *fudi-in* (fudi:open :port 3015))
(defvar *fudi-out-udp* (fudi:open :port 3008 :protocol :udp :direction :output))
(defvar *fudi-out-tcp* (fudi:open :port 3012 :direction :output))
```

Click on the `disconnect, connect localhost 3015` Messagebox in the pd
patch.

Sending to pd (observe the output in the Main Pd window):


```
;;; udp:
(fudi:send *fudi-out-udp* '(1 2 3 4 5))

;;; tcp:
(fudi:send *fudi-out-tcp* '(1 2 3 "Hallo" 4 5))
(fudi:send *fudi-out-tcp* '("Hello" "World!"))
```

Receiving from pd:

```
(recv-start *fudi-in*)

(defvar *fudi-responder*
  (incudine::make-fudi-responder
   *fudi-in*
   (lambda (msg)
     (format *debug-io* "~a~%" msg))))
```

(Send messages to lisp by clicking in the message boxes in the pd patch)

Additional responders can be added like an echo responder:

```
(defvar *fudi-responder2*
  (incudine::make-fudi-responder
   *fudi-in*
   (lambda (msg)
     (fudi:send *fudi-out-tcp* msg))))
```

Sending messages to lisp by clicking in the message boxes in the pd
patch should diplay output in the lisp REPL and in Pd's Main Window.

You can start and stop (suspend) the receiver without losing the
connection:

```
(incudine:recv-stop *fudi-in*)
(incudine:recv-start *fudi-in*)

```
Responders can get removed selectively:

```
(incudine:remove-responder *fudi-responder*)
```

```
(incudine:remove-responder *fudi-responder2*)
```

Closing the streams will automatically remove all receivers and
responders.

```
(fudi:close *fudi-in*)
(fudi:close *fudi-out*)
```
