#lang racket/base

(provide ∆ avg v v-x v-y manhattan-distance manhattan-distance/half-x font-size/whcl)

(require racket/math)

(define (∆ a b) (- b a))
(define (avg a b) (/ (+ a b) 2))

(define (v x y) (vector-immutable x y))
(define (v-x v) (vector-ref v 0))
(define (v-y v) (vector-ref v 1))

(define (manhattan-distance a b)
  (for/sum ([ax (in-vector a)] [bx (in-vector b)])
    (abs (∆ ax bx))))

(define (manhattan-distance/half-x a b)
  (for/sum ([ax (in-vector a)] [bx (in-vector b)] [i (in-naturals)])
    (define d (abs (∆ ax bx)))
    (cond
      [(and (zero? i) (< 1/2 d)) +inf.0]
      [else                      d])))

(define (font-size/whcl w h c l)
  (exact-floor (min (/ h (max 1 l)) (* w (/ 2 (max 1 c))))))
