;{{{ Stream by delay / force
(define-syntax stream-cons
  (syntax-rules ()
    [(_ a b)
     (cons a (delay b))]))
(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))
;}}}
;{{{ Coroutine by call/cc
(define-syntax coroutine
  (lambda (x)
    (syntax-case x ()
      [(name x body ...)
       (datum->syntax #'name (syntax->datum
         #'(letrec ([+local-control-state (lambda (x) body ...)]
                    [resume
                      (lambda (c v)
                        (call/cc
                          (lambda (k)
                            (set! +local-control-state k)
                            (c v))))])
             (lambda (v)
               (+local-control-state v)))))])))
;}}}

;{{{ Calculation coroutine
;;{{{ Newton iteration
(define (find-minimal-n l u)
  (let* ([Ll (- (log l))] [C (/ (+ u l) (- 1 l))] [x (- 1. (/ (log Ll) Ll))]
         [F (lambda (x) (let ([e (expt l (- x))])
                          (/ (+ (* e (1- (* x Ll))) C) (1- (* e Ll)))))])
    (let loop ([x x] [y (F x)])
      (if (< (abs (- x y)) 1e-8) (exact (ceiling y))
        (loop y (F y))))))
;;}}}
;;{{{ Factor-Coeff Transformer
(define (fct factor coeff)
  (let* ([t2 (* factor factor)] [/1+t2 (/ (1+ t2))] [f (* 2 t2 /1+t2)]
         [c (* coeff factor /1+t2)])
    (values f c)))
;;}}}
(define (make-calc-coroutine caller b factor coeff)
  (let-values ([(factor coeff) (fct factor coeff)])
    (let* ([p (numerator factor)] [q (denominator factor)]
           [P (numerator coeff)] [Q (denominator coeff)]
           [2q (* q 2)] [l (/ factor 2)] [1-l (- 1 l)] [Ll (- (log l))]
           [B (expt 10 b)] [u (1+ (exact (ceiling (* b (log 10) (/ Ll)))))])
      (let-values ([(A c) (if (< coeff 1-l) (values u (/ (1+ u) 1-l))
          (values (+ u (max (exact (ceiling (/ (- (log coeff) (log 1-l)) Ll)))
                            (find-minimal-n l u))) 2))])
        (cons (cons b (+ 2 (ceiling (/ (1- (/ (* c p) 1-l 1-l)) Q)))) (coroutine nil
          (let loop ([i 0] [f (make-list A P)])
            (let loop ([j i] [f f] [d 0] [c u] [b (* (1- (+ A (* i u))) p)]
                       [g (* (1- (* (+ A (* i u)) 2)) q)])
              (cond [(zero? j)
                     (if (null? (cdr f))
                       (let-values ([(x y) (div-and-mod (+ (* (car f) B) d) Q)])
                         (set-car! f y)
                         (resume caller x))
                       (let-values ([(x y) (div-and-mod (+ (* (car f) B) d) g)])
                         (set-car! f y)
                         (loop j (cdr f) (* x b) c (- b p) (- g 2q))))]
                    [(positive? c)
                     (let-values ([(x y) (div-and-mod (+ (* (car f) B) d) g)])
                       (set-car! f y)
                       (loop j (cdr f) (* x b) (1- c) (- b p) (- g 2q)))]
                    [else (loop (1- j) f (* d B) u b g)]))
            (loop (1+ i) (append! (make-list u P) f)))))))))
;}}}
;{{{ Print coroutine
;;{{{ Carry stream
(define (stream-carry nums base lbound ubound)
  (let-values ([(aq ar) (div-and-mod (stream-car nums) base)])
    (if (< (1- lbound) ar (- base ubound))
      (stream-cons aq
        (let ([n (stream-carry (stream-cdr nums) base lbound ubound)])
          (stream-cons (+ (stream-car n) ar) (stream-cdr n))))
      (let* ([n (stream-carry (stream-cdr nums) base lbound ubound)]
             [c (stream-car n)])
        (let-values ([(aqd ar) (div-and-mod (+ ar c) base)])
          (stream-cons (+ aq aqd) (stream-cons ar (stream-cdr n))))))))
;;}}}
;;{{{ Print with dot & negative sign
(define-syntax printf-dot-neg
  (syntax-rules ()
    [(_ dot neg b num)
     (let ([bv b] [numv num])
       (if dot
         (begin
           (when (negative? numv)
             (set! neg (1- (expt 10 b)))
             (set! numv (1+ numv))
             (if (zero? numv) (printf "-")))
           (printf "~d." numv)
           (set! dot #f))
         (printf "~v,'0d" bv
                 (if neg (- neg numv) numv)))
       (flush-output-port))]))
;;}}}
(define (make-print-coroutine . calc-list)
  (let* ([len (length calc-list)] [b #f] [lbound 0] [ubound -1] [crl '()] [snl '()]
         [pint? (lambda (x) (and (integer? x) (positive? x)))]
         [zpint? (lambda (x) (or (zero? x) (pint? x)))])
    (if (not (and
        (positive? len) (odd? len)
        (for-all
          (let ([f #f] [s '+])
            (lambda (x) (set! f (not f)) (if f
                (and (pair? x) (pair? (car x))
                     (if b (eqv? b (caar x))
                       (begin (set! b (caar x)) (pint? b)))
                     (zpint? (cdar x))
                     (begin
                       (case s
                         [(-)
                          (set! lbound (+ lbound (cdar x) 1))
                          (set! snl (cons -1 snl))]
                         [(+)
                          (set! ubound (+ ubound (cdar x) 1))
                          (set! snl (cons 1 snl))])
                       (set! crl (cons (cdr x) crl))
                       #t))
                (begin (set! s x) (or (eq? s '+) (eq? s '-))))))
          calc-list)
        (< (+ lbound ubound) (expt 10 b))))
      (error 'make-print-coroutine "invalid argument"))
    (coroutine nil
      (letrec* ([raw-digits (lambda ()
          (stream-cons
            (apply + (map (lambda (c s) (* (resume c 'nil) s))
                          crl snl))
            (raw-digits)))]
                [digits (stream-carry (raw-digits) (expt 10 b) lbound ubound)])
        (let ([dot #t] [neg #f])
          (let loop ([digits digits])
            (printf-dot-neg dot neg b (stream-car digits))
            (loop (stream-cdr digits))))))))
;}}}

;{{{ π = 4 arctan 1
#;(let ([b 100])
  (letrec* ([pr (lambda (x) (printer x))]
            [calc (make-calc-coroutine pr b 1 4)]
            [printer (make-print-coroutine calc)])
    (printer 'nil)))
;}}}
;{{{ π = 4 arctan (1 / 2) + 4 arctan (1 / 3)
#;(let ([b 100])
  (letrec* ([pr (lambda (x) (printer x))]
            [calc1 (make-calc-coroutine pr b 1/2 4)]
            [calc2 (make-calc-coroutine pr b 1/3 4)]
            [printer (make-print-coroutine calc1 '+ calc2)])
    (printer 'nil)))
;}}}
;{{{ π = 4 arctan (1 / 7) + 4 arctan (3 / 4)
#;(let ([b 100])
  (letrec* ([pr (lambda (x) (printer x))]
            [calc1 (make-calc-coroutine pr b 1/7 4)]
            [calc2 (make-calc-coroutine pr b 3/4 4)]
            [printer (make-print-coroutine calc1 '+ calc2)])
    (printer 'nil)))
;}}}
;{{{ π = 20 arctan (1 / 7) + 8 arctan (3 / 79)
(let ([b 1000])
  (letrec* ([pr (lambda (x) (printer x))]
            [calc1 (make-calc-coroutine pr b 1/7 20)]
            [calc2 (make-calc-coroutine pr b 3/79 8)]
            [printer (make-print-coroutine calc1 '+ calc2)])
    (printer 'nil)))
;}}}
;{{{ π = 16 arctan (1 / 5) - 4 arctan (1 / 239)
#;(let ([b 1000])
  (letrec* ([pr (lambda (x) (printer x))]
            [calc1 (make-calc-coroutine pr b 1/5 16)]
            [calc2 (make-calc-coroutine pr b 1/239 4)]
            [printer (make-print-coroutine calc1 '- calc2)])
    (printer 'nil)))
;}}}
;{{{ π = 48 arctan (1 / 18) + 32 arctan (1 / 57) - 20 arctan (1 / 239)
#;(let ([b 1000])
  (letrec* ([pr (lambda (x) (printer x))]
            [calc1 (make-calc-coroutine pr b 1/18 48)]
            [calc2 (make-calc-coroutine pr b 1/57 32)]
            [calc3 (make-calc-coroutine pr b 1/239 20)]
            [printer (make-print-coroutine calc1 '+ calc2 '- calc3)])
    (printer 'nil)))
;}}}
;{{{ π = 32 arctan (1 / 10) - 4 arctan (1 / 239) - 16 arctan (1 / 515)
#;(let ([b 1000])
  (letrec* ([pr (lambda (x) (printer x))]
            [calc1 (make-calc-coroutine pr b 1/10 32)]
            [calc2 (make-calc-coroutine pr b 1/239 4)]
            [calc3 (make-calc-coroutine pr b 1/515 16)]
            [printer (make-print-coroutine calc1 '- calc2 '- calc3)])
    (printer 'nil)))
;}}}
