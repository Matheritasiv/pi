;{{{ Macro
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

(define-syntax printf-dot
  (syntax-rules ()
    [(_ dot b num)
     (let ([bv b] [numv num])
       (if dot
         (begin
           (printf "~d." numv)
           (set! dot #f))
         (printf "~v,'0d" bv numv))
       (flush-output-port))]))
;}}}
;{{{ Calculation coroutine
;{{{ Factor-Coeff Transformer
(define (fct factor coeff)
  (let* ([t2 (* factor factor)] [/1+t2 (/ (1+ t2))] [f (* 2 t2 /1+t2)]
         [c (* coeff factor (denominator f) /1+t2)])
    (values f c)))
;}}}
(define (make-calc-coroutine caller b factor coeff)
  (coroutine nil
    (let-values ([(factor coeff) (fct factor coeff)])
      (let* ([numer (numerator factor)]
             [denom (denominator factor)] [2denom (* 2 denom)]
             [h (denominator coeff)] [a (expt 10 b)]
             [u (exact (floor
                  (/ (+ (* (1+ b) (log 10)) (log coeff))
                     (- (log 2denom) (log numer)))))])
        (let loop ([i 0] [f (list 0)])
          (let ([f (append! (make-list (* u 2) (numerator coeff)) f)])
            (let loop ([f
                (let loop ([j i] [f (list-tail f u)])
                  (if (zero? j) f (begin
                    (let loop ([f f] [d 0] [j (* u 3)] [b (* (+ i j 1) u)]
                               [g (* denom (1- (* 2 (+ i j 1) u)))])
                      (if (<= j 1) (set-car! f (+ (car f) (* d b numer)))
                        (let-values ([(x y) (div-and-mod
                            (+ (* (car f) (if (> j u) a 1)) (* d b numer)) g)])
                          (set-car! f y)
                          (loop (cdr f) x (1- j) (1- b) (- g 2denom)))))
                    (loop (1- j) (list-tail f u)))))]
                [d 0] [b (* (1+ i) u)] [g (* denom (1- (* 2 (1+ i) u)))])
              (if (null? (cdr f))
                (let-values ([(x y) (div-and-mod
                    (+ (* (car f) a) d) h)])
                  (set-car! f y)
                  (resume caller x))
                (let-values ([(x y) (div-and-mod
                    (+ (* (car f) a) (* d b numer)) g)])
                  (set-car! f y)
                  (loop (cdr f) x (1- b) (- g 2denom)))))
            (loop (1+ i) f)))))))
;}}}
;{{{ Print coroutine
(define (make-print-coroutine b comb . calc-list)
  (coroutine nil
    (let ([a (expt 10 b)] [dot #t])
      (let loop ([next 0] [e #f])
        (let ([l (map (lambda (c) (resume c 'nil)) calc-list)])
          (if e
            (let-values ([(s t) (div-and-mod next a)])
              (printf-dot dot b (+ s e))
              (loop (apply comb l) t))
            (loop (apply comb l) 0)))))))
;}}}

;{{{ π = 4 arctan 1
#;(let ([b 1000])
  (letrec* ([pr (lambda (x) (printer x))]
            [calc (make-calc-coroutine pr b 1 4)]
            [printer (make-print-coroutine b values calc)])
    (printer 'nil)))
;}}}
;{{{ π = 4 arctan (1 / 2) + 4 arctan (1 / 3)
#;(let ([b 1000])
  (letrec* ([pr (lambda (x) (printer x))]
            [calc1 (make-calc-coroutine pr b 1/2 4)]
            [calc2 (make-calc-coroutine pr b 1/3 4)]
            [printer (make-print-coroutine b + calc1 calc2)])
    (printer 'nil)))
;}}}
;{{{ π = 4 arctan (1 / 7) + 4 arctan (3 / 4)
#;(let ([b 1000])
  (letrec* ([pr (lambda (x) (printer x))]
            [calc1 (make-calc-coroutine pr b 1/7 4)]
            [calc2 (make-calc-coroutine pr b 3/4 4)]
            [printer (make-print-coroutine b + calc1 calc2)])
    (printer 'nil)))
;}}}
;{{{ π = 20 arctan (1 / 7) + 8 arctan (3 / 79)
(let ([b 1000])
  (letrec* ([pr (lambda (x) (printer x))]
            [calc1 (make-calc-coroutine pr b 1/7 20)]
            [calc2 (make-calc-coroutine pr b 3/79 8)]
            [printer (make-print-coroutine b + calc1 calc2)])
    (printer 'nil)))
;}}}
;{{{ π = 16 arctan (1 / 5) - 4 arctan (1 / 239)
#;(let ([b 1000])
  (letrec* ([pr (lambda (x) (printer x))]
            [calc1 (make-calc-coroutine pr b 1/5 16)]
            [calc2 (make-calc-coroutine pr b 1/239 4)]
            [printer (make-print-coroutine b - calc1 calc2)])
    (printer 'nil)))
;}}}
;{{{ π = 48 arctan (1 / 18) + 32 arctan (1 / 57) - 20 arctan (1 / 239)
#;(let ([b 1000])
  (letrec* ([pr (lambda (x) (printer x))]
            [calc1 (make-calc-coroutine pr b 1/18 48)]
            [calc2 (make-calc-coroutine pr b 1/57 32)]
            [calc3 (make-calc-coroutine pr b 1/239 20)]
            [printer (make-print-coroutine b
                       (lambda (x y z) (- (+ x y) z))
                       calc1 calc2 calc3)])
    (printer 'nil)))
;}}}
