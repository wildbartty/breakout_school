#lang racket
(require syntax/parse)
(require syntax/parse/define)
(require framework)
(require racket/gui)
(require pict)



;A breakout game for SDD11

(collect-garbage 'incremental)



;(define (make-get-set list) (push list '()))

(define main-pipe (current-thread))

(define game-size 5)

(struct ball ([radius] [speed #:mutable] [xy #:mutable #:auto])
  #:auto-value 0+0i)

(struct paddle (width position) #:mutable)

(define-syntax between
  (syntax-rules ()
    ((_ num x y)
     (< x num y))
    ((_ num x)
     (< 0 num x))))

(define-syntax paddle-draw
  (syntax-rules ()
    ((_ paddle dc)
     (when (paddle? paddle)
       (let ((y (- (cadr game-size) 20)))
       (send dc draw-rectangle (paddle-position paddle) y (paddle-width paddle) 10))))))

(define-syntax paddle-move
  (syntax-rules ()
    ((_ paddle num)
     (when (paddle? paddle)
       (set-paddle-position! paddle (+ (paddle-position paddle) num))))))

(define-syntax ball-draw
  (syntax-rules ()
    ((_ ball x y dc)
     (when (ball? ball)
       (let* ((radius (* 2 (ball-radius ball))))
         (send dc draw-ellipse x y radius radius))))
    ((_ ball dc)
     (when (ball? ball)
       (let* ((radius (* 2 (ball-radius ball))) (x (real-part (ball-xy ball))) (y (imag-part (ball-xy ball))))
         (send dc draw-ellipse x y radius radius))))))

(define-syntax ball-move
  (syntax-rules ()
    ((_ ball)
     (when (ball? ball)
       (let ((speed (ball-speed ball)))
         (set-ball-xy! ball (+ (ball-xy ball) speed)))))))


(define-syntax incf
  (syntax-rules ()
    ((_ num)
     (set! num (+ num 1)))
    ((_ num n ...)
     (set! num (+ num n ...)))))

(define-syntax decf
  (syntax-rules ()
    ((_ num)
     (set! num (- num 1)))
    ((_ num n ...)
     (set! num (- num n ...)))))

(define-syntax pop
  (syntax-rules ()
    ((_ lst)
     (begin
       (let ((ret (car lst)))
       (set! lst (cdr lst))
         ret)
       ))))

(define bob (ball 20 4+2i))

(define phil (paddle 60 10))


(define start-frame (new frame% [label "hi"] [min-height 240] [min-width 320]))


(define game-canvas%
  (class canvas%
    (inherit get-width get-height refresh)

    ;; direction : one of #f, 'left, 'right, 'up, 'down
    (field [direction #f])
    
    (define/override (on-char ke)
      (case (send ke get-key-code)
        [(left)
         (set! direction (send ke get-key-code))
         (set-paddle-position! phil (+ (paddle-position phil) -5))
         (refresh)]
        [(right)
         (set! direction (send ke get-key-code))
         (set-paddle-position! phil (+ (paddle-position phil) 5))
         (refresh)]
        [else (void)]))

    

    (super-new)))

(define res (cons
             (new combo-field%
                  [label "Resoulution"] [choices (list "240 x 320" "320 x 480" "360 x 640" "720 x 1280" "768 x 1366" "1080 x 1920")]
                  [parent start-frame] [init-value "320 x 480"])
             (delay (new button% [parent start-frame] [label "Start"]
                         [callback
                          (λ (button evt)
                            (let* ((str (parse-n-nums (send (car res) get-value)))
                                  (num (if (<= 2 (length str))
                                              (list (car str) (cadr str))
                                              (cons 320 (cons 480 null))))
                                  (game game-frame))
                              (send start-frame show #f)
                              (send game show #t)
                              (send game resize (cadr num) (car num))
                              (send game min-height (car num))
                              (send game min-width (cadr num))
                              (send game stretchable-height #f)
                              (send game stretchable-width #f)
                              (set! game-size (list (cadr num) (car num)))
                              (define ticker (new timer% [interval 20]
                                                  [notify-callback (λ ()
                                                                     (check-bounds)
                                                                     (send game-dc clear)
                                                                     (ball-draw bob game-dc)
                                                                     (ball-move bob)
                                                                     (paddle-draw phil game-dc))]))
                              7
                              
                              ;(thread-resume game-thread)
                              ;(send (car res) set-label (string-append (number->string (car num)) (number->string (cadr num))))
                              ))
                              ]
                         ))))

(force (cdr res))

(define (check-bounds)
  (let ((wid (- (car game-size) (* 2 (ball-radius bob)))) (hei (- (cadr game-size) (* 2 (ball-radius bob))))
                               (x (real-part (ball-xy bob)))
                               (y (imag-part (ball-xy bob))))
    (if (and (between x wid) (between y hei))
        null
        (begin
          (when (not (< 0 x wid)) (set-ball-speed! bob (make-rectangular (- (real-part (ball-speed bob))) (imag-part (ball-speed bob)))))
          (when (not (< 0 y  hei)) (set-ball-speed! bob (make-rectangular (real-part (ball-speed bob)) (- (imag-part (ball-speed bob))))))
          (when (>= 0 x) (set-ball-xy! bob (make-rectangular 0 y)))
          (when (<= wid x) (set-ball-xy! bob (make-rectangular wid y)))
          (when (>= 0 y) (set-ball-xy! bob (make-rectangular x 0)))
          (when (<= hei y) (set-ball-xy! bob (make-rectangular x hei)))
      )
      )))

;(define drawboard (new canvas% [parent start-frame] [label "1234"] [enabled #t]))

;(define drawb-dc (send drawboard get-dc))

(define quit-but (new button% [label "quit"] [parent start-frame] [callback (λ (button evt) (exit:exit))]))

(send start-frame show #t)

(define (parse-n-nums str (acc null) (tem null))
  (cond
    ((null? str) (if (null? acc)
                     (list (string->number (list->string tem)))
                     (append (reverse acc) (cons (string->number (list->string tem)) null))))
    ((list? str)
     (if  (eq? (char-general-category (car str)) 'nd)
          (if (null? tem)
              (parse-n-nums (cdr str) acc (list (car str)))
              (parse-n-nums (cdr str) acc (append tem (cons (car str) null))))
          (let ((test (string->number (list->string tem))))
            (if (number? test)
                (parse-n-nums (cdr str) (cons (string->number (list->string tem)) acc))
                (parse-n-nums (cdr str) acc)))))
    ((number? str) str)
    ((string? str) (parse-n-nums (string->list str)))
    ((symbol? str) (parse-n-nums (symbol->string str)))
    ((#t) null)))

(define game-frame (new frame% [label "BREAKOUT"]))
(define game-draw (new game-canvas% [parent game-frame]))
(define game-dc (send game-draw get-dc))

(define bill (list 1 2 3 4))

#|(define worker (thread (lambda ()
                         (let loop ()
                           (displayln "Working...")
                           (sleep 0.2)
                           (loop)))))|#

#|(define game-thread (thread (λ ()
                               (let loop ()
                                 (ball-draw bob game-dc)

;                                 (when (let ((x (real-part (ball-xy bob))))
;                                         (or (= 0 x) (=
                                 
                                 (ball-move bob)
                                 (sleep (/ 1 30))
                                 (loop)))))
(thread-suspend game-thread)

(define joe (delay (let-values ([(x y) (send game-dc get-size)])
              (list x y))))|#
                               
                              
;(parse-n-nums "1234cds546hello martin 56 78 9")
;(parse-n-nums  "420")