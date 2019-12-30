#lang racket
(require
  (only-in net/url call/input-url string->url get-pure-port)
  (only-in threading ~>)
  (only-in racket/hash hash-union))

(module+ test
  (require rackunit))

(define DAY2-SESSION-COOKIE "53616c7465645f5f8116a80bce7c23d7df91d8f7fd2714ec1987ff706becd7441ef83276b16c27ad25cd17c9c1b73040")

(define (solve-day3-part2)
  (define wire-paths
    (~>
     (call/input-url
      (string->url "https://adventofcode.com/2019/day/3/input")
      get-pure-port
      port->lines
      (list (format "Cookie: session=~a" DAY2-SESSION-COOKIE)))
     (map (lambda (line) (string-split line ",")) _)))
  (define wire-path1 (first wire-paths))
  (define wire-path2 (second wire-paths))
  (define wire-coords1 (path->coords wire-path1))
  (define wire-coords2 (path->coords wire-path2))
  (displayln
   (shortest-distance-to-intersection wire-coords1 wire-coords2)))

(module+ test
  (define path1 (list "R2" "U3"))
  (define path2 (list "U2" "R3"))
  (check-equal? (path->coords path1)
                (hash
                 (list 1 0) 1 (list 2 0) 2
                 (list 2 1) 3 (list 2 2) 4 (list 2 3) 5))
  (check-equal? (path->coords path2)
                (hash
                 (list 0 1) 1 (list 0 2) 2
                 (list 1 2) 3 (list 2 2) 4 (list 3 2) 5)))
(define (path->coords path)
  (define coords (hash))
  (define-values (coord-x coord-y distance-traveled) (values 0 0 0))
  (for ([movement path])
    (define direction (string-ref movement 0))
    (define amount (string->number (substring movement 1)))
    (define-values
      (movement-coords new-x new-y new-distance)
      (move coord-x coord-y direction amount distance-traveled))
    (set! coords
          (hash-union
           coords
           movement-coords
           #:combine/key
           ; distance1 is always <= distance2
           (lambda (_ distance1 distance2) distance1)))
    (set!-values (coord-x coord-y distance-traveled)
                 (values new-x new-y new-distance)))
  coords)

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
   (move 0 0 #\U 5 0)
   (values
    (for/hash ([distance (in-range 1 6)]
               [y (in-range 1 6)])
      (values (list 0 y) distance))
    0
    5
    5)
   "move up")

  (check-values-equal?
   (move 0 0 #\R 5 1)
   (values
    (for/hash ([distance (in-range 2 7)]
               [x (in-range 1 6)])
      (values (list x 0) distance))
    5
    0
    6)
   "move right")

  (check-values-equal?
   (move 0 5 #\D 5 0)
   (values
    (for/hash ([distance (in-range 0 5)]
               [y (in-range 5 0 -1)])
      (values (list 0 y) distance))
    0
    0
    5)
   "move down")

  (check-values-equal?
   (move 5 0 #\L 5 2)
   (values
    (for/hash ([distance (in-range 2 7)]
               [x (in-range 5 0 -1)])
      (values (list x 0) distance))
    0
    0
    7)
   "move left"))
(define (move x y dir amount distance)
  (define fn
    (cond [(char=? dir #\U) go-up]
          [(char=? dir #\D) go-down]
          [(char=? dir #\R) go-right]
          [else             go-left]))
  (fn x y amount distance))

(define (go-up x start-y amount start-distance)
  (define-values (end-y end-distance)
    (values (+ start-y amount) (+ start-distance amount)))
  (define coords
    (for/hash ([distance (in-range start-distance (+ end-distance 1))]
               [y (in-range start-y (+ end-y 1))]
               #:unless (and (= x 0) (= y 0)))
      (values (list x y) distance)))
  (values coords x end-y end-distance))

(define (go-down x start-y amount start-distance)
  (define-values (end-y end-distance)
    (values (- start-y amount) (+ start-distance amount)))
  (define coords
    (for/hash ([distance (in-range start-distance (+ end-distance 1))]
               [y (in-range start-y ( - end-y 1) -1)]
               #:unless (and (= x 0) (= y 0)))
      (values (list x y) distance)))
  (values coords x end-y end-distance))

(define (go-right start-x y amount start-distance)
  (define-values (end-x end-distance)
    (values (+ start-x amount) (+ start-distance amount)))
  (define coords
    (for/hash ([distance (in-range start-distance (+ end-distance 1))]
               [x (in-range start-x (+ end-x 1))]
               #:unless (and (= x 0) (= y 0)))
      (values (list x y) distance)))
  (values coords end-x y end-distance))

(define (go-left start-x y amount start-distance)
  (define-values (end-x end-distance)
    (values (- start-x amount) (+ start-distance amount)))
  (define coords
    (for/hash ([distance (in-range start-distance (+ end-distance 1))]
               [x (in-range start-x (- end-x 1) -1)]
               #:unless (and (= x 0) (= y 0)))
      (values (list x y) distance)))
  (values coords end-x y end-distance))

(module+ test
  (define-values (coords1 coords2)
    (values (hash (list 1 0) 5 (list 8 3) 12 (list 2 2) 5)
            (hash (list 1 0) 3 (list 8 3) 6  (list 2 2) 18)))
  (check-equal? (shortest-distance-to-intersection coords1 coords2)
                8))
(define (shortest-distance-to-intersection coords1 coords2)
  (for/fold ([shortest-distance #f])
            ([(coord distance1) (in-hash coords1)])
    (define distance2 (hash-ref coords2 coord #f))
    (cond
      [(equal? distance2 #f) shortest-distance]
      [else
       (define distance (+ distance1 distance2))
       (if (or (equal? shortest-distance #f)
               (< distance shortest-distance))
           distance
           shortest-distance)])))


(module+ main
  (solve-day3-part2))