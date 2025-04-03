; Similix redundant let-elimination analysis
; Copyright (C) 1993 Anders Bondorf
; Please see the file README for copyright notice, license and disclaimer.


;------------------------------------------------------------------------------
(define (_sim-rl-analyse! pgm)
  (define (rl! e)
    (cond
      ((_sim-islift? e)
       (rl! (_sim-fetch-lift-arg e)))
      ((_sim-iscst? e)
       "nothing")
      ((_sim-isvar? e)
       "nothing here either")
      ((_sim-iscond? e)
       (rl! (_sim-fetch-cond-test e))
       (rl! (_sim-fetch-cond-then e))
       (rl! (_sim-fetch-cond-else e)))
      ((_sim-islet? e)
       (rl! (_sim-fetch-let-actual e))
       (rl! (_sim-fetch-let-body e)))
      ((_sim-isbegin? e)
       (rl! (_sim-fetch-begin-fst e))
       (rl! (_sim-fetch-begin-snd e)))
      ((_sim-isprimop? e)
       (rl*! (_sim-fetch-primop-args e)))
      ((_sim-iscstr? e)
       (rl*! (_sim-fetch-cstr-args e)))
      ((_sim-issel? e)
       (rl! (_sim-fetch-sel-arg e)))
      ((_sim-ispred? e)
       (rl! (_sim-fetch-pred-arg e)))
      ((_sim-ispcall? e)
       (rl*! (_sim-fetch-pcall-args e)))
      ((_sim-isabs? e)
       (_sim-set-abs-body!
	e
	(eliminate-lets! (_sim-fetch-abs-body e)
			 (_sim-fetch-abs-formals e))))
      ((_sim-isapp? e)
       (rl! (_sim-fetch-app-exp e))
       (rl*! (_sim-fetch-app-args e)))
      (else
       (_sim-error 'rl! "unknown syntactic form: ~s" e))))
  
  (define (rl*! e*)
    (if (not (null? e*))
	(begin (rl! (car e*)) (rl*! (cdr e*)))))
  
  (define (eliminate-lets! e formal*)
    (cond
      ((_sim-islift? e)
       (begin
	 (_sim-set-lift-arg!
	  e (eliminate-lets! (_sim-fetch-lift-arg e) formal*))
	 e))
      ((possibly-inserted-let? e formal*)
       (let ((e1 (eliminate-lets! (_sim-fetch-let-body e) formal*)))
	 (if (_sim-islet-unfoldable? e)
	     e1
	     (begin (_sim-set-let-body! e e1) e))))
      (else
       (begin (rl! e) e))))
  
  (define (possibly-inserted-let? e formal*)
    (and (_sim-islet? e)
	 (let ((formal (_sim-fetch-let-formal e))
	       (actual (_sim-fetch-let-actual e)))
	   (and (_sim-isvar? actual)
		(equal? formal (_sim-fetch-var-varname actual))
		(member formal formal*)))))
  
  ;--------------------------------------------------------------------------
  ; main function:
  
  (let ((udp (_sim-fetch-pgm-udp pgm)))
    (begin
      (_sim-vector-for-each
       (lambda (d)
	 (let* ((e (_sim-fetch-def-exp d))
		(formal* (_sim-fetch-def-pars d))
		(new-e (eliminate-lets! e formal*)))
	   (begin (_sim-set-def-exp! d new-e)
		  (_sim-debruijn! new-e formal*))))
       udp)
      udp)))

;------------------------------------------------------------------------------
