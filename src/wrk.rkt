#lang racket
(require 2htdp/image)

(define R 100)

(define-struct game-struct [draw-command vertex rings forward-transitions]
  #:transparent)

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



(define game-db (list->vector
                 (list 
                  (let [(circles (make-hash
                                  (list
                                   (cons 'circle1 '(v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22))
                                   (cons 'circle2 '(v23 v24 v25 v26 v27 v28 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40 v41 v42 v43 v44 v45)))))]
                    (make-game-struct
                     (list (list ':draw 'circle1) (list ':to-right R 'circle2))
                     (get-vertex-set 43)
                     circles
                     (make-hash
                      (list (cons 'forward (make-hash (hash-map  circles (λ (x y) (cons x (forward-transitions y))))))
                            (cons 'backward (make-hash (hash-map  circles (λ (x y) (cons x (backward-transitions y)))))))))))))


(define-struct game-state-struct  (game-info state) #:transparent)


(define game (make-game-state-struct
              (vector-ref  game-db 0)
              (make-hash
               (list
                (cons 'circle1 '(b b b v y y y y y y y y y y y b b b b v b b b))
                (cons 'circle2 '(b b b b b b b b b b b b b b b b b b b b b b b))))))


(define (draw-ring xs)
  (let ([r (/ (* 2 R) (length xs))]
        [step (/ 360 (length xs))])
           
  (foldr (λ (x s)
           (rotate step (if (not x) s
                          (overlay/offset
                           (circle r 'outline (ring-color x))
                           0 R
                           s))))
         (circle R 'outline 'black)
         xs)))


(define (draw-game game )
  (let [(commands (game-struct-draw-command (game-state-struct-game-info game)))
        (rings  (game-state-struct-state game))]
    (foldl
     (λ (x y)
       (if (not x) y
           (case (car x)
             [':draw (draw-ring (hash-ref rings (second x)))]
             [':to-right  (overlay/offset
                           (draw-ring  (hash-ref rings (third x)))
                           (second x) 0
                           y)])))
     
     (square R 'solid 'black)
     commands)))



(define (draw-game-telega commands rings)
    (foldl
     (λ (x y)
       (if (not x) y
           (case (car x)
             [':draw (draw-ring (hash-ref rings (second x)))]
             [':to-right  (overlay/offset
                           (draw-ring  (hash-ref rings (third x)))
                           (second x) 0
                           y)])))
     
     (square R 0 'black)
     commands))



(module+ test
  (require rackunit)
  (check-equal? (get-vertex-set 2) (list 'v0 'v1) )
  (check-equal? (forward-transitions (get-vertex-set 3)) '((v0 v1) (v1 v2) (v2 v0)))
  (check-equal? (backward-transitions (get-vertex-set 3)) '((v2 v1) (v1 v0) (v0 v2)) )

  ) 




