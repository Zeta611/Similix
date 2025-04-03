; Similix 'load system-files' file
; Copyright (C) 1993 Anders Bondorf
; Please see the file README for copyright notice, license and disclaimer.


;-----------------------------------------------------------------------------

(newline)
(display "Welcome to Similix 5.0")
(newline)
(display "Copyright (C) 1993 Anders Bondorf")
(newline)
(display  "Contributions by Olivier Danvy and Jesper Joergensen")
(newline)
(newline)


;----------------------------------------------------------------------------
; Temporary file used by Similix:

(define **similix-tmp-file** "_simtmp")

;-----------------------------------------------------------------------------

(define **similix-library** **similix-path**)

;-----------------------------------------------------------------------------

(display "util ")
(_sim-flush-output-port)
(load (string-append
       **similix-path** "util" **similix-compiled-suffix**))

(display "langext ")
(_sim-flush-output-port)
(load (string-append
       **similix-path** "langext" **similix-compiled-suffix**))

(display "abssyn ")
(_sim-flush-output-port)
(load (string-append
       **similix-path** "abssyn" **similix-compiled-suffix**))

(display "miscspec ")
(_sim-flush-output-port)
(load (string-append
       **similix-path** "miscspec" **similix-compiled-suffix**))

(display "runtime ")
(_sim-flush-output-port)
(load (string-append
       **similix-path** "runtime" **similix-compiled-suffix**))

(display "front ")
(_sim-flush-output-port)
(load (string-append
       **similix-path** "front" **similix-compiled-suffix**))

(display "lam-lift ")
(_sim-flush-output-port)
(load (string-append
       **similix-path** "lam-lift" **similix-compiled-suffix**))

(display "bt-eod ")
(_sim-flush-output-port)
(load (string-append
       **similix-path** "bt-eod" **similix-compiled-suffix**))

(display "sp ")
(_sim-flush-output-port)
(load (string-append
       **similix-path** "sp" **similix-compiled-suffix**))

(display "oc ")
(_sim-flush-output-port)
(load (string-append
       **similix-path** "oc" **similix-compiled-suffix**))

(display "rl ")
(_sim-flush-output-port)
(load (string-append
       **similix-path** "rl" **similix-compiled-suffix**))

(display "post ")
(_sim-flush-output-port)
(load (string-append
       **similix-path** "post" **similix-compiled-suffix**))

(display "spec ")
(newline)
(load (string-append
       **similix-path** "spec" **similix-compiled-sim-suffix**))

;----------------------------------------------------------------------------
