#lang racket
(require
  (only-in net/url call/input-url string->url get-pure-port)
  (only-in threading ~>)
  (only-in racket/hash hash-union))

(module+ test
  (require rackunit))

(define DAY2-SESSION-COOKIE "53616c7465645f5f8116a80bce7c23d7df91d8f7fd2714ec1987ff706becd7441ef83276b16c27ad25cd17c9c1b73040")

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
  (foldl (lambda (coord-wires crossings)
           (define coord (car coord-wires))
           (define coord-x (car coord))
           (define coord-y (cdr coord))
           (define wires (cdr coord-wires))
           (if (list? wires)
               (cons (cons coord-x coord-y) crossings)
               crossings))
         '()
         (hash->list (count-wires paths))))

(module+ test
  (define paths2 (list (list "R2" "U3") (list "U2" "R3")))
  (check-equal? (count-wires paths2)
                (hash
                 (cons 1 0) 0 (cons 2 0) 0
                 (cons 2 1) 0 (cons 2 2) (list 0 1) (cons 2 3) 0
                 (cons 0 1) 1 (cons 0 2) 1
                 (cons 1 2) 1 (cons 3 2) 1)))
(define (count-wires wires)
  (define wire-counts (hash))
  (define coord-x 0)
  (define coord-y 0)
  (for ([wire-idx (in-naturals)]
        [wire wires])
    (for ([step wire])
      (define direction (string-ref step 0))
      (define amount (string->number (substring step 1)))
      (define-values
        (wire-count new-x new-y)
        (count-wire wire-idx coord-x coord-y direction amount))
      (set! wire-counts
            (hash-union
             wire-counts
             wire-count
             #:combine/key (lambda (_ wire1 wire2)
                             (if (= wire1 wire2)
                                 wire1
                                 (list (min wire1 wire2)
                                       (max wire1 wire2))))))
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
   (count-wire 1 0 0 #\U 5)
   (values
    (for*/hash ([x (in-range 0 1)]
                [y (in-range 1 6)])
      (values (cons x y) 1))
    0
    5)
   "count-wire up")

  (check-values-equal?
   (count-wire 2 0 0 #\R 5)
   (values
    (for*/hash ([x (in-range 1 6)]
                [y (in-range 0 1)])
      (values (cons x y) 2))
    5
    0)
   "count-wire right")

  (check-values-equal?
   (count-wire 2 0 5 #\D 5)
   (values
    (for*/hash ([x (in-range 0 1)]
                [y (in-range 1 6)])
      (values (cons x y) 2))
    0
    0)
   "count-wire down")

  (check-values-equal?
   (count-wire 1 5 0 #\L 5)
   (values
    (for*/hash ([x (in-range 1 6)]
                [y (in-range 0 1)])
      (values (cons x y) 1))
    0
    0)
   "count-wire left"))

(define (count-wire wire-number x y direction amount)
  (cond
    [(char=? direction #\U) (go-up wire-number x y amount)]
    [(char=? direction #\D) (go-down wire-number x y amount)]
    [(char=? direction #\R) (go-right wire-number x y amount)]
    [else (go-left wire-number x y amount)]))

(define (go-up wire-number x start-y amount)
  (define end-y (+ start-y amount))
  (define wires
    (for*/hash ([y (in-range start-y (+ end-y 1))]
                #:unless (and (= x 0) (= y 0)))
      (values (cons x y) wire-number)))
  (values wires x end-y))

(define (go-down wire-number x start-y amount)
  (define end-y (- start-y amount))
  (define wires
    (for*/hash ([y (in-range end-y (+ start-y 1))]
                #:unless (and (= x 0) (= y 0)))
      (values (cons x y) wire-number)))
  (values wires x end-y))

(define (go-right wire-number start-x y amount)
  (define end-x (+ start-x amount))
  (define wires
    (for*/hash ([x (in-range start-x (+ end-x 1))]
                #:unless (and (= x 0) (= y 0)))
      (values (cons x y) wire-number)))
  (values wires end-x y))

(define (go-left wire-number start-x y amount)
  (define end-x (- start-x amount))
  (define wires
    (for*/hash ([x (in-range end-x (+ start-x 1))]
                #:unless (and (= x 0) (= y 0)))
      (values (cons x y) wire-number)))
  (values wires end-x y))

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