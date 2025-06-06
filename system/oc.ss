; Similix abstract occurrence counting analysis
; Copyright (C) 1991 Anders Bondorf and Olivier Danvy
; Please see the file README for copyright notice, license and disclaimer.


;------------------------------------------------------------------------------
(define (_sim-oc-analyse! pgm)
  
  ; abstract arithmetic of the counting analysis:
  
  ; domain:	       any-hash
  ;		       /      \
  ;		  one-hash   zero-hash
  ;		       \      /
  ;		      bottom-hash  (never used in practice)
  
  (let ((udp (_sim-fetch-pgm-udp pgm))
	(one-hash 'one-hash)
	(zero-hash 'zero-hash)
	(any-hash 'any-hash)
	(bottom-hash 'bottom-hash))
    
    (define (plus-hash x y)
      (cond
	((or (equal? x bottom-hash)
	     (equal? y bottom-hash))
	 bottom-hash)
	((equal? x zero-hash)
	 y)
	((equal? y zero-hash)
	 x)
	(else
	 any-hash)))
    
    (define (lub x y)
      (cond
	((equal? x bottom-hash)
	 y)
	((equal? y bottom-hash)
	 x)
	((or (equal? x any-hash)
	     (equal? y any-hash)
	     (not (equal? x y)))
	 any-hash)
	(else
	 x)))
    
    ; ---------------------------------------------------------------------
    ; oc-exp abstractly counts the occurrences of variable i in expression e.
    ; caution: because i comes from a list of parameters it is uninterned,
    ; i.e., it is a scheme symbol.
    (define (oc-exp i e)
      (let count
	  ((e e))
	(cond
	  ((_sim-islift? e)
	   (count (_sim-fetch-lift-arg e)))
	  ((_sim-iscst? e)
	   zero-hash)
	  ((_sim-isvar? e)
	   (if (equal? (_sim-fetch-var-varname e) i)
	       one-hash
	       zero-hash))
	  ((_sim-iscond? e)
	   (let* ((test-exp (_sim-fetch-cond-test e))
		  (then-part (count (_sim-fetch-cond-then e)))
		  (else-part (count (_sim-fetch-cond-else e)))
		  (max-then-else (lub then-part else-part)))
	     (if (_sim-isdynamic? test-exp)
		 (plus-hash (count test-exp)
			    (lub (plus-hash then-part else-part)
				 max-then-else))
		 max-then-else)))
	  ((_sim-islet? e)
	   (plus-hash
	    (count (_sim-fetch-let-actual e))
	    (if (equal? (_sim-fetch-let-formal e) i)
		zero-hash
		(count (_sim-fetch-let-body e)))))
	  ((_sim-isbegin? e)
	   (plus-hash (count (_sim-fetch-begin-fst e))
		      (count (_sim-fetch-begin-snd e))))
	  ((_sim-isprimop? e)
	   (map-plus-hash count (_sim-fetch-primop-args e)))
	  ((_sim-iscstr? e)
	   (map-plus-hash count (_sim-fetch-cstr-args e)))
	  ((_sim-issel? e)
	   (count (_sim-fetch-sel-arg e)))
	  ((_sim-ispred? e)
	   (count (_sim-fetch-pred-arg e)))
	  ((_sim-ispcall? e)
	   (map-plus-hash count (_sim-fetch-pcall-args e)))
	  ((_sim-isabs? e)
	   (if (_sim-occurs-free? i (_sim-fetch-abs-free-variables e))
	       any-hash
	       zero-hash))
	  ((_sim-isapp? e)
	   (plus-hash
	    (count (_sim-fetch-app-exp e))
	    (map-plus-hash count (_sim-fetch-app-args e))))
	  (else
	   (_sim-error 'oc-exp "unknown syntactic form: ~s" e)))))
    
    (define (map-plus-hash f l)
      (if (null? l)
	  zero-hash
	  (plus-hash (f (car l)) (map-plus-hash f (cdr l)))))
    
    
    ; oc-strap looks for internal let-expressions
    ; it then oc-exp the body of the let-expression.
    (define (oc-strap e)
      (cond
	((_sim-islift? e)
	 (oc-strap (_sim-fetch-lift-arg e)))
	((_sim-iscst? e)
	 "nothing here")
	((_sim-isvar? e)
	 "nothing here either")
	((_sim-iscond? e)
	 (oc-strap (_sim-fetch-cond-test e))
	 (oc-strap (_sim-fetch-cond-then e))
	 (oc-strap (_sim-fetch-cond-else e)))
	((_sim-islet? e)
	 (let* ((actual (_sim-fetch-let-actual e))
		(body (_sim-fetch-let-body e)))
	   (begin
	     (if (or (_sim-eod-imperative? (_sim-fetch-eod-tag actual))
		     (and (_sim-isdynamic? actual)
			  (not (equal? (oc-exp (_sim-fetch-let-formal e) body)
				       one-hash))))
		 (_sim-raise-let-unfoldability! e))
	     (oc-strap actual)
	     (oc-strap body))))
	((_sim-isbegin? e)
	 (oc-strap (_sim-fetch-begin-fst e))
	 (oc-strap (_sim-fetch-begin-snd e)))
	((_sim-isprimop? e)
	 (for-each oc-strap (_sim-fetch-primop-args e)))
	((_sim-iscstr? e)
	 (for-each oc-strap (_sim-fetch-cstr-args e)))
	((_sim-issel? e)
	 (oc-strap (_sim-fetch-sel-arg e)))
	((_sim-ispred? e)
	 (oc-strap (_sim-fetch-pred-arg e)))
	((_sim-ispcall? e)
	 (for-each oc-strap (_sim-fetch-pcall-args e)))
	((_sim-isabs? e)
	 (oc-strap (_sim-fetch-abs-body e)))
	((_sim-isapp? e)
	 (begin (oc-strap (_sim-fetch-app-exp e))
		(for-each oc-strap (_sim-fetch-app-args e))))
	(else
	 (_sim-error 'oc-strap "unknown syntactic form: ~s" e))))
    
    ;----------------------------------------------------------------------
    ; main function:
    
    (begin
      (_sim-vector-for-each
       (lambda (d) (oc-strap (_sim-fetch-def-exp d)))
       udp)
      udp)))

;-----------------------------------------------------------------------------
