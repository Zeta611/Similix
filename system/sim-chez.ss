; Similix load file for Scm
; Copyright (C) 1993 Anders Bondorf
; Please see the file README for copyright notice, license and disclaimer.


;----------------------------------------------------------------------------

;****************************************************************************
;**** at this point: insert path name of the path where this file is located
(define **similix-path** "/users/jay/developer/similix/system/")
;			  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;****************************************************************************



;============================================================================
;======== scheme system dependent definitions begin here: ===================

(define **similix-source-suffix** ".ss")
(define **similix-compiled-suffix** ".so")
(define **similix-compiled-sim-suffix** ".sim")


(define _sim-flush-output-port flush-output-port)

(define (_sim-remove-file file)
  (system (string-append "rm " (_sim-string-eval file))))

(define _sim-compile-file compile-file)

(define _sim-garbage-collect collect)

(define (_sim-ntimes suspension n)
  (_sim-garbage-collect)
  (time (begin
	  (let loop ((i 1))
	    (if (< i n)
		(begin (suspension)
		       (loop (+ i 1)))))
	  (suspension))))

(define _sim-pretty-print pretty-print)

(define _sim-error error)

;-----------------------------------------------------------------------------

;======== scheme system dependent definitions end here ======================
;============================================================================



(load (string-append **similix-path** "loadsysf" **similix-compiled-suffix**))

;----------------------------------------------------------------------------
