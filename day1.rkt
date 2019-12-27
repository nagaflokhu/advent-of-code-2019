#lang racket
#;(define exponent
    (for/last ([i (in-naturals)])
      (displayln i)
      (define sum
        (for/sum ([j (in-range i)])
          (expt 32 j)))
      (displayln sum)
      #:final (>= sum (expt 2 30))
      i))
;(displayln exponent)

;(displayln (+ 1 (/ (log (expt 2 30)) (log (expt 2 5)))))

(define input-path (expand-user-path "~/Downloads/AOCDay1In.txt"))

(define (calc-required-fuel mass)
  (- (quotient mass 3) 2))

(define (calc-required-fuel-rec mass)
  (define (iter total mass)
    (define required-fuel (calc-required-fuel mass))
    (if (<= required-fuel 0)
        total
        (iter (+ total required-fuel) required-fuel)))
  (iter 0 mass))

(define (solve-day1-part calc-func)
  (with-input-from-file input-path
    (thunk
     (for/sum ([str (in-lines)])
       (define num (string->number str))
       (calc-func num)))))

(define (solve-day1-part1) (solve-day1-part calc-required-fuel))

(define (solve-day1-part2) (solve-day1-part calc-required-fuel-rec))