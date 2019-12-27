#lang racket
(require net/url)
(require threading)

(define DAY2-SESSION-COOKIE "53616c7465645f5f8116a80bce7c23d7df91d8f7fd2714ec1987ff706becd7441ef83276b16c27ad25cd17c9c1b73040")

(define (solve-day2)
  (define initial-program
    (~>
     (call/input-url
      (string->url "https://adventofcode.com/2019/day/2/input")
      get-pure-port
      port->lines
      (list (format "Cookie: session=~a" DAY2-SESSION-COOKIE)))
     (list-ref _ 0)
     (string-split _ ",")
     (map string->number _)
     list->vector))
  (define program (make-vector (vector-length initial-program)))
  (vector-copy! program 0 initial-program)
  (displayln (solve-part1 program))
  (displayln (solve-part2 program initial-program)))

(define (solve-part1 program)
  (vector-set! program 1 12)
  (vector-set! program 2 2)
  (vector-ref (execute program) 0))

(define (solve-part2 program initial-program)
  (for*/last ([val1 (in-range 100)]
              [val2 (in-range 100)])
    (displayln (list val1 val2))
    (vector-copy! program 0 initial-program)
    (vector-set! program 1 val1)
    (vector-set! program 2 val2)
    (define result
      (with-handlers ([exn:fail? (lambda (_) 0)])
        (vector-ref (execute program) 0)))
    #:final (= result 19690720)
    (+ (* 100 val1) val2)))

(define (execute program)
  (define (iter index)
    (if (>= index (- (vector-length program) 3))
        program
        (let ([opcode (vector-ref program index)]
              [index+1 (+ index 1)]
              [index+2 (+ index 2)]
              [index+3 (+ index 3)])
          (cond
            [(= opcode 1)
             (add program index+1 index+2 index+3)
             (iter (+ index 4))]
            [(= opcode 2)
             (multiply program index+1 index+2 index+3)
             (iter (+ index 4))]
            [(= opcode 99) program]
            [else (error "Unknown opcode encountered.")]))))
  (iter 0))

(define (op func program index1 index2 index3)
  (vector-set! program
               (vector-ref program index3)
               (func (vector-ref program (vector-ref program index1))
                     (vector-ref program (vector-ref program index2)))))

(define (add program index1 index2 index3)
  (op + program index1 index2 index3))

(define (multiply program index1 index2 index3)
  (op * program index1 index2 index3))

(module+ main
  (solve-day2))