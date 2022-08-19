#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(provide (all-defined-out))


(define R 100)


(define-struct game-info-struct [board ;; описание конфигурации
                                 trans ;; переходы для каждого кольца веперед и назад
                                 names ;; имена кружков (для отладки)
                                 target-state ;; целевая конфигурация
                                 ] #:transparent)


(define game-info
  (let [(nodes (vector
                (list 250 150 ) (list 240.0 193.0 ) (list 212.0 228.0 ) (list 172.0 247.0 )
                (list 128.0 247.0 ) (list 88.0 228.0 ) (list 60.0 193.0 ) (list 50.0 150.0 )
                (list 60.0 107.0 ) (list 88.0 72.0 ) (list 128.0 53.0 ) (list 172.0 53.0 )
                (list 212.0 72.0 ) (list 240.0 107.0 ) (list 374 150 ) (list 364.0 193.0 )
                (list 336.0 228.0 ) (list 296.0 247.0 ) (list 252.0 247.0 ) (list 184.0 193.0 )
                (list 174.0 150.0 ) (list 184.0 107.0 ) (list 252.0 53.0 ) (list 296.0 53.0 )
                (list 336.0 72.0 ) (list 364.0 107.0 )))]
    (make-game-info-struct
     nodes
     (make-hash 
      (list
       (cons 'circle1
             (make-hash
              (list
               (cons 'forward (cons 13 (vector 13 00 01 02 03 04 05 06 07 08 09 10 11 12 14 15 16 17 18 19 20 21 22 23 24 25)))
               (cons 'backward (cons 0 (vector 01 02 03 04 05 06 07 08 09 10 11 12 13 00 14 15 16 17 18 19 20 21 22 23 24 25))))))
       (cons 'circle2
             (make-hash
              (list
               ;;                              00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
               (cons 'forward (cons 14 (vector 00 01 18 03 04 05 06 07 08 09 10 11 21 13 25 14 15 16 17 02 19 20 12 22 23 24)))
               (cons 'backward (cons 0 (vector 00 01 19 03 04 05 06 07 08 09 10 11 22 13 15 16 17 18 02 20 21 12 23 24 25 14))))))))

     (vector 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)
     (vector
      'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue
      'red 'red 'red 'red 'red 'red 'red 'red 'red 'red 'red 'red
      ))))

(define-struct game-struct [info state names selected moves])

(define game (make-game-struct game-info
                               (game-info-struct-target-state game-info)
                               (game-info-struct-names game-info) 0
                               (list)))

(define (init-game game)
  "Инициализация головоломки"
  (let* [(circle-vector (list->vector (hash-keys (game-info-struct-trans (game-struct-info game))))) 
         (circle-count (vector-length circle-vector))
         (move-count (* 10 (random 100)))
         (command (map (λ (x) (let ([circle (modulo  (random 100) circle-count)]
                                    [move-to (if (= 0 (modulo (random 100) 2)) 'forward 'backward)])
                                (list circle move-to)))
                       (build-list move-count values)))]
    (let loop [(local game) (xs command)]
      (if (null? xs)
          (struct-copy game-struct local
                 [moves (list)])
          (let* [(f (first xs))
                 (circle-index (first f))
                 (direction  (second f))]
            (loop (one-move local (vector-ref circle-vector circle-index) direction)
                  (rest xs)))))))
                 

(define (draw-game game)
  "Отрисовка головоломки"
  (define (describe-move circle direction)
    (format "~a ~a" (symbol->string circle) (if (eq? 'forward direction) #\u2192 #\u2190)))

  (let* (
         [escene (rectangle (* 4 R) (* 3 R) 'solid 'white)]
         [state (game-struct-state game)]
         [info (game-struct-info game)]
         [target-state (game-info-struct-target-state info)]
         [tagret-count (count (λ (x y) (eq? x y)) (vector->list state) (vector->list target-state))]
         [xs (game-info-struct-board info)]
         [r (+ 5 (/ (* 4 R) (vector-length  xs)))]
         [step (/ 360 (vector-length  xs))]
         [count (vector-length  xs)]
         [target-percent (truncate (* 100 (/ tagret-count count)))]
         [names (game-struct-names game)]
         [rings (foldr (λ (x s)
                         (let [(pos (vector-ref xs x))
                               (color (vector-ref state x))]
                           (place-images
                            (list
                             (text  (number->string (vector-ref names x)) 14 'white)
                             (circle r 'solid color)
                             (if (= x (game-struct-selected game))
                                 (circle (+ r 3) 'outline color)
                                 (circle r 'outline color)
                                 ))
                            (list
                             (make-posn (first pos) (second pos))
                             (make-posn (first pos) (second pos))
                             (make-posn (first pos) (second pos)))
                            s)))
                       escene
                       (build-list count values))]
         [stack (above
                 (text (format "solved : ~a%" target-percent) 14 'black)
                 (text "moves:" 12 'black)
                       (foldr (λ (x s)
                                (above
                                 (text (describe-move (first x) (second x)) 12 'black)
                                 s))
                              empty-image
                              (let [(moves-list (game-struct-moves game))]
                              (take  moves-list (min (length moves-list) 10)))))]
         [info (foldr (λ (x s )
                        (above
                         (text x 12 'black)
                         s))
                      (empty-scene 0 0)
                      (list "1 - forward circle1"
                            "2 - backward circle1" 
                            "3 - forward circle2"
                            "4 - backward circle2"
                            "z - undo last move (todo)"
                            ))])


    (beside
     rings
     (above info stack))
    ))



(define (one-move game circle direction)
  "Поворачивает выбранный круг в выбранном направлении"
  (let* (
         [state (game-struct-state game)]
         [names (game-struct-names game)]
         [info (game-struct-info game)]
         [trans (hash-ref (hash-ref (game-info-struct-trans info) circle) direction)]
         [new-state (vector-map (λ (x) (vector-ref state x))  (cdr trans))]
         [new-names (vector-map (λ (x) (vector-ref names x))  (cdr trans))]
         [new-stack (game-struct-moves game)]
         [new-moves (append (list (list circle direction))  new-stack  ) ]
         )
    (struct-copy game-struct game
                 [state new-state]
                 [names new-names]
                 [moves new-moves])))


(define (move-forward game circle)
  (one-move game circle 'forward))

(define (move-backward game circle)
  (one-move game circle 'backward))


(define (keyboard-event-handler w a-key)
  (cond
    [(key=? a-key "1")  (move-forward w 'circle1)]
    [(key=? a-key "2")  (move-backward w 'circle1)]
    [(key=? a-key "3")  (move-forward w 'circle2)]
    [(key=? a-key "4")  (move-backward w 'circle2)]
    [else w]))


(define (mouse-event-handler w pos-x pos-y mouse-event)
  (cond
    [(string=? mouse-event "button-down") (select-circle w pos-x pos-y )]
    [else w]
    )
  )


(define (select-circle game pos-x pos-y )
  
  (let* [
         (board (game-info-struct-board (game-struct-info game)))
         [r (+ 5 (/ (* 4 R) (vector-length  board)))]
         [new-list (filter (λ (x) (not (= -1 x)))
                           (map (λ (x y) (let ([px-min (- (first x) r)]
                                               [px-max (+ (first x) r)]
                                               [py-min (- (second x) r)]
                                               [py-max (+ (second x) r)])
                                           (if (and (< px-min pos-x) (> px-max pos-x)
                                                    (< py-min pos-y) (> py-max pos-y))
                                               y
                                               -1)))
                                (vector->list board)
                                (build-list (vector-length board) values)))]
         [new-selected (if (empty? new-list )
                           -1
                           (first new-list)
                    
  
                           )]]
    (struct-copy game-struct game
                 [selected new-selected])
    ))


(big-bang (init-game game)
  (to-draw draw-game)
  (on-key keyboard-event-handler)
  (on-mouse mouse-event-handler)
  )

