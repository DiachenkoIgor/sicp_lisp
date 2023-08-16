(define (make-interval a b) (cons a b))

(define (add-interval x y)
(make-interval (+ (lower-bound x) (lower-bound y))
(+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
(make-interval (- (lower-bound x) (lower-bound y))
(- (upper-bound x) (upper-bound y))))

(define (width i)(/ (- (upper-bound i) (lower-bound i)) 2))

(define (mul-interval x y)
(let ((p1 (* (lower-bound x) (lower-bound y)))
(p2 (* (lower-bound x) (upper-bound y)))
(p3 (* (upper-bound x) (lower-bound y)))
(p4 (* (upper-bound x) (upper-bound y))))
(make-interval (min p1 p2 p3 p4)
(max p1 p2 p3 p4))))

(define (div-interval x y)
(define (inner-mul)(mul-interval
x
(make-interval (/ 1.0 (upper-bound y))
(/ 1.0 (lower-bound y)))))
  (if (OR (= (upper-bound y) 0) (= (lower-bound y) 0))
      (error "wrong interval")
      (inner-mul)
    )

)

(define (make-interval a b) (cons a b))

(define upper-bound cdr)
(define lower-bound car)

(define (make-center-width c w)
(make-interval (- c w) (+ c w)))

(define (center i)
(/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
(/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent mid-point perc)
  (let 
    ((width (* mid-point perc)))
    (make-interval (- mid-point width) (+ mid-point width))
    )
)

(make-center-percent 3.5 0.042857143)

(define (percent i)
(/ (width i) (center i))
)

(define A (make-center-percent 3.5 0.015))
(define B (make-center-percent 10 0.01))
(define ONE (make-interval 1 1))


(define (par1 r1 r2)
(div-interval (mul-interval r1 r2)
(add-interval r1 r2)))

(define (par2 r1 r2)
(let ((one (make-interval 1 1)))
(div-interval
one (add-interval (div-interval one r1)
(div-interval one r2)))))

(mul-interval (div-interval A B) B)





