#lang racket

(define-struct game-struct [rings vertex transitions])

(define  game-state (list
                     (list 'circle1 '(r r r b r r r r r b r r r))
                     (list 'circle1 '(b b b b b b b b b b b b b))))


(define (get-vertex-set vertex-count)
  "Return list of nodes"
  (map (λ (x) (string->symbol (string-append "v" (number->string x))))
       (build-list vertex-count values)))

(define (generate-transitions vertex)
  "list of transition for vertex"
  (map (λ (x y) (list x y))
       vertex
       (append (rest vertex) (list (first vertex)))))

(define (forward-transitions vertex)
  "normal transition (forward move)"
  (generate-transitions vertex))

(define (backward-transitions vertex)
  "reverse transition (backward move)"
  (generate-transitions (reverse vertex)))



(define (forward-move current-state circel)
  (error "todo"))

(module+ test
  (require rackunit)
  (check-equal? (get-vertex-set 2) (list 'v0 'v1) )
  (check-equal? (forward-transitions (get-vertex-set 3)) '((v0 v1) (v1 v2) (v2 v0)))
  (check-equal? (backward-transitions (get-vertex-set 3)) '((v2 v1) (v1 v0) (v0 v2)) )

  ) 