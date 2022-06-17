#lang racket
(require 2htdp/image)
(require racket/math)

(define-struct point [x y] #:transparent)

(define R 100)

(define (get-vertex-set vertex-count)
  "Return list of nodes"
  (map (λ (x) (string->symbol (string-append "v" (number->string x))))
       (build-list vertex-count values)))

(define circle1 (get-vertex-set 10))


(define index (map (λ  (x y) (list x (* (/ 360 (length circle1)) y))) circle1 (build-list (length circle1) values)))

(define gindex (map (λ (x) (list (first x)
                                 (make-point 
                                  (round (* R (cos (* (/ pi 180) (second x)))))
                                  (round (* R (sin (* (/ pi 180) (second x))))))))
                    index ))


(define (draw-game xs)
  (foldl (λ (x s)
           (if (not x)
               s
               (overlay/offset
                (overlay
                 (circle (/ (* 2 pi R) (length xs) 2) 'outline 'blue)
                 (text (symbol->string (first x)) 12 'black))
                (point-x (second x)) (point-y (second x))
                s)))
         (empty-scene  (* 10 R) (* 4 R))
         xs))





