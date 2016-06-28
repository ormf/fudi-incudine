;;;; 1. load the project: 

;;; (ql:quickload "fudi")

;;; 2. start the pd patch.
;;; 3. start inbound and outbound connection:

(in-package :incudine.scratch)

(defvar *fudi-in* nil)
(defvar *fudi-out-udp* nil)
(defvar *fudi-out-tcp* nil)

(setf *fudi-in* (fudi:open :port 3015))
(setf *fudi-out-udp* (fudi:open :port 3008 :protocol :udp :direction :output))
(setf *fudi-out-tcp* (fudi:open :port 3012 :direction :output))

(fudi:send *fudi-out-udp* '(1 2 3 4 5))
(fudi:send *fudi-out-tcp* '(1 2 3 "Hallo" 4 5))
(fudi:send *fudi-out-tcp* '("Hallo" "Welt!"))

(recv-start *fudi-in*)

(defvar *fudi-responder*
  (incudine::make-fudi-responder
   *fudi-in*
   (lambda (msg)
     (format *debug-io* "~a~%" msg))))


(incudine:remove-responder *fudi-responder2*)

(fudi:close *fudi-in*)
(fudi:close *fudi-out*)
