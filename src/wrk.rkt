#lang racket

(define-struct game-struct [rings vertex transitions])

(define  game-state (list
                     (list 'circle1 '(r r r b r r r r r b r r r))
                     (list 'circle1 '(b b b b b b b b b b b b b))))