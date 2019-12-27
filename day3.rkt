#lang racket
(require
  (only-in net/url call/input-url string->url get-pure-port)
  (only-in threading ~>)
  (only-in racket/hash hash-union))

(module+ test
  (require rackunit))

(define DAY2-SESSION-COOKIE "53616c7465645f5f8116a80bce7c23d7df91d8f7fd2714ec1987ff706becd7441ef83276b16c27ad25cd17c9c1b73040")
(define TURN 'turn)
(define HORIZONTAL 'horizontal)
(define VERTICAL 'vertical)
(define BOTH 'both)

(define (solve-day3)
  (define wire-paths
    (~>
     (call/input-url
      (string->url "https://adventofcode.com/2019/day/3/input")
      get-pure-port
      port->lines
      (list (format "Cookie: session=~a" DAY2-SESSION-COOKIE)))
     (map (lambda (line) (string-split line ",")) _)))
  (displayln
   (shortest-manhattan-distance-from-origin
    (find-crossings wire-paths))))

(module+ test
  (define paths1
    (list
     (list "R8" "U5" "L5" "D3")
     (list "U7" "R6" "D4" "L4")))
  (check-equal? (sort (find-crossings paths1) < #:key car)
                (sort (list (cons 3 3) (cons 6 5)) < #:key car)))
(define (find-crossings paths)
  (foldl (lambda (coord-orientation crossings)
           (define coord (car coord-orientation))
           (define coord-x (car coord))
           (define coord-y (cdr coord))
           (define orientation (cdr coord-orientation))
           (if (eq? orientation BOTH)
               (cons (cons coord-x coord-y) crossings)
               crossings))
         '()
         (hash->list (count-wires paths))))

(module+ test
  (define paths2 (list (list "R2" "U3") (list "U2" "R3")))
  (check-equal? (count-wires paths2)
                (hash
                 (cons 1 0) HORIZONTAL (cons 2 0) TURN
                 (cons 2 1) VERTICAL (cons 2 2) BOTH (cons 2 3) TURN
                 (cons 0 1) VERTICAL (cons 0 2) TURN
                 (cons 1 2) HORIZONTAL (cons 3 2) TURN)))
(define (count-wires paths)
  (define wire-counts (hash))
  (define coord-x 0)
  (define coord-y 0)
  (for ([path paths])
    (for ([step path])
      (define direction (string-ref step 0))
      (define amount (string->number (substring step 1)))
      (define-values
        (path-orientations new-x new-y)
        (count-wire coord-x coord-y direction amount))
      (set! wire-counts
            (hash-union
             wire-counts
             path-orientations
             #:combine/key
             (lambda (_ orientation1 orientation2)
               (cond
                 [(or (eq? orientation1 TURN) (eq? orientation2 TURN)) TURN]
                 [(eq? orientation1 orientation2) orientation1]
                 [else BOTH]))))
      (set! coord-x new-x)
      (set! coord-y new-y))
    (set! coord-x 0)
    (set! coord-y 0))
  wire-counts)

(module+ test
  (require (only-in adjutor values->list))
  (define-syntax (check-values-equal? stx)
    (syntax-case stx ()
      [(_ values-expr1 values-expr2 test-name)
       #'(begin
           (define list1 (values->list values-expr1))
           (define list2 (values->list values-expr2))
           (check-equal? list1 list2 test-name))]))
  
  (check-values-equal?
   (count-wire 0 0 #\U 5)
   (values
    (hash-set
     (for*/hash ([x (in-range 0 1)]
                 [y (in-range 1 5)])
       (values (cons x y) VERTICAL))
     (cons 0 5)
     TURN)
    0
    5)
   "count-wire up")

  (check-values-equal?
   (count-wire 0 0 #\R 5)
   (values
    (hash-set
     (for*/hash ([x (in-range 1 5)]
                 [y (in-range 0 1)])
       (values (cons x y) HORIZONTAL))
     (cons 5 0)
     TURN)
    5
    0)
   "count-wire right")

  (check-values-equal?
   (count-wire 0 5 #\D 5)
   (values
    (for*/hash ([x (in-range 0 1)]
                [y (in-range 1 5)])
      (values (cons x y) VERTICAL))
    0
    0)
   "count-wire down")

  (check-values-equal?
   (count-wire 5 0 #\L 5)
   (values
    (for*/hash ([x (in-range 1 5)]
                [y (in-range 0 1)])
      (values (cons x y) HORIZONTAL))
    0
    0)
   "count-wire left"))

(define (count-wire x y direction amount)
  (cond
    [(char=? direction #\U) (go-up x y amount)]
    [(char=? direction #\D) (go-down x y amount)]
    [(char=? direction #\R) (go-right x y amount)]
    [else (go-left x y amount)]))

(define (go-up x start-y amount)
  (define end-y (+ start-y amount))
  (define wires
    (hash-set
     (for*/hash ([y (in-range start-y end-y)]
                 #:unless (and (= x 0) (= y 0)))
       (values (cons x y) VERTICAL))
     (cons x end-y)
     TURN))
  (values wires x end-y))

(define (go-down x start-y amount)
  (define end-y (- start-y amount))
  (define wires
    (for*/hash ([y (in-range (+ end-y 1) start-y)]
                #:unless (and (= x 0) (= y 0)))
      (values (cons x y) VERTICAL)))
  (unless (and (= x 0) (= end-y 0))
    (set! wires (hash-set wires (cons x end-y) TURN)))
  (values wires x end-y))

(define (go-right start-x y amount)
  (define end-x (+ start-x amount))
  (define wires
    (hash-set
     (for*/hash ([x (in-range start-x end-x)]
                 #:unless (and (= x 0) (= y 0)))
       (values (cons x y) HORIZONTAL))
     (cons end-x y)
     TURN))
  (values wires end-x y))

(define (go-left start-x y amount)
  (define end-x (- start-x amount))
  (define wires
    (for*/hash ([x (in-range (+ end-x 1) start-x)]
                #:unless (and (= x 0) (= y 0)))
      (values (cons x y) HORIZONTAL)))
  (unless (and (= end-x 0) (= y 0))
    (set! wires (hash-set wires (cons end-x y) TURN)))
  (values wires end-x y))
    
;  (define-values (x-min x-max final-x y-min y-max final-y orientation)
;    (cond
;      [(char=? direction #\U)
;       (values x (+ x 1) x (+ y 1) (+ y amount) (+ y amount) VERTICAL)]
;      [(char=? direction #\D)
;       (values x
;               (+ x 1)
;               x
;               (max 0 (+ (- y amount) 1))
;               y
;               (max 0 (+ (- y amount) 1))
;               VERTICAL)]
;      [(char=? direction #\R)
;       (values (+ x 1) (+ x amount) (+ x amount) y (+ y 1) y HORIZONTAL)]
;      [(char=? direction #\L)
;       (values (max 0 (+ (- x amount) 1))
;               x
;               (max 0 (+ (- x amount) 1))
;               y
;               (+ y 1)
;               y
;               HORIZONTAL)]))
;  (define orientations
;    (for*/hash ([x (in-range x-min x-max)]
;                [y (in-range y-min y-max)]
;                #:unless (and (= x 0) (= y 0)))
;      (values (cons x y) orientation)))
;  (set! orientations
;        (hash-set orientations
;                  (if (eq? orientation VERTICAL)
;                      (cons x-min y-max)
;                      (cons x-max y-min))
;                  TURN))
;  (values orientations final-x final-y))

(module+ test
  (define coords '((3 . 3) (3 . 4) (4 . 4)))
  (check-equal? (shortest-manhattan-distance-from-origin coords) 6))
(define (shortest-manhattan-distance-from-origin coords)
  (foldl (lambda (coord min-dist)
           (define dist-x (abs (car coord)))
           (define dist-y (abs (cdr coord)))
           (define distance (+ dist-x dist-y))
           (cond
             [(equal? min-dist '()) distance]
             [(< distance min-dist) distance]
             [else min-dist]))
         '()
         coords))

(module+ main
  (solve-day3))

#;(match-lambda
    [(list (list (cons coord-x coord-y) wire-count) crossings)
     (if (> wire-count 1)
         (cons (cons coord-x coord-y) crossings)
         crossings)])