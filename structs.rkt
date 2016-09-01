#lang racket
(provide (struct-out event)
         (struct-out date)
         (struct-out event/results)
         (struct-out stage)
         (struct-out player)
         (struct-out racetime)
         (struct-out race-result)
         date<
         (rename-out [my-current-date current-date])
         event<
         date->string
         year/month->string
         time<
         string->time
         time->string
         millis->time
         time->millis
         time-diff)

(require racket/date)

(struct date (year month day) #:prefab)

(define (my-current-date)
  (match (current-date)
    [(date* _ _ _ d m y _ _ _ _ _ _) (date y m d)]))

(define (year/month->string year month)
  (format "~a-~a"
          (~a year #:width 4 #:align 'right #:pad-string "0")
          (~a month #:width 2 #:align 'right #:pad-string "0")))

(struct event (cat date id) #:prefab)
(struct event/results (event results) #:prefab)
(struct stage (name info results) #:prefab)
(struct race-result (player vehicle time) #:prefab)
(struct player (platform name id) #:prefab)
(struct racetime (hour min sec milli) #:prefab)

(define (date< a b)
  (match (list a b)
    [(list #f #f) #f]
    [(list _ #f) #t]
    [(list #f _) #f]
    [(list (date a-y a-m a-d)
           (date b-y b-m b-d))
     (cond [(< a-y b-y) #t]
           [(< b-y a-y) #f]
           [(< a-m b-m) #t]
           [(< b-m a-m) #f]
           [else (< a-d b-d)])]))

(define (event< a b)
  (define (find s)
    (define order
      '(("daily" "daily2")
        ("weekly" "weekly2")
        ("monthly")
        ("promotional")))
    (memf (λ (l) (member s l)) order))
  (match (list a b)
    [(list (event a-cat a-date _) (event b-cat b-date _))
     (define al (find a-cat))
     (define a-pos (member a-cat al))
     (define bl (find b-cat))
     (define b-pos (member b-cat bl))
  
     (cond [(not (equal? al bl)) (< (length al) (length bl))]
           [(or (date< a-date b-date) (date< b-date a-date)) (date< a-date b-date)]
           [(not (equal? a-pos b-pos)) (> (length a-pos) (length b-pos))]
           [else #f])]))

(define (date->string d)
  (match d
    [#f "??"]
    [(date y m d) (format "~a-~a-~a"
                          (~a y #:width 4 #:align 'right #:pad-string "0")
                          (~a m #:width 2 #:align 'right #:pad-string "0")
                          (~a d #:width 2 #:align 'right #:pad-string "0"))]))

(define (time< a b)
  (match (list a b)
    [(list #f #f) #f]
    [(list _ #f) #t]
    [(list #f _) #f]
    [(list (racetime a-hour a-min a-sec a-milli)
           (racetime b-hour b-min b-sec b-milli))
     (cond [(< a-hour b-hour) #t]
           [(< b-hour a-hour) #f]
           [(< a-min b-min) #t]
           [(< b-min a-min) #f]
           [(< a-sec b-sec) #t]
           [(< b-sec a-sec) #f]
           [else (< a-milli b-milli)])]))

(define (string->time s)
  (if s
      (match (regexp-match #rx"^([0-9][0-9]):([0-9][0-9])\\.([0-9][0-9][0-9])$" s)
        [(list _ m s mi) (racetime 0 (string->number m) (string->number s) (string->number mi))]
        [#f (match (regexp-match #rx"^([0-9])+:([0-9][0-9]):([0-9][0-9])\\.([0-9][0-9][0-9])$" s)
              [(list _ h m s mi) (racetime (string->number h) (string->number m) (string->number s) (string->number mi))]
              [#f (error 'nooo "~a" s)])])
      #f))

(define (time->string t)
  (match t
    [#f "--"]
    [(racetime 0 min sec mil)
     (format "~a:~a.~a"
             (~a min #:width 2 #:align 'right #:pad-string "0")
             (~a sec #:width 2 #:align 'right #:pad-string "0")
             (~a mil #:width 3 #:align 'right #:pad-string "0"))]
    [(racetime hour min sec mil)
     (format "~a:~a:~a.~a"
             (~a hour #:width 2 #:align 'right #:pad-string "0")
             (~a min #:width 2 #:align 'right #:pad-string "0")
             (~a sec #:width 2 #:align 'right #:pad-string "0")
             (~a mil #:width 3 #:align 'right #:pad-string "0"))]))


(define (time->millis t)
  (match t
    [(racetime hour min sec mil)
     (+ mil (* 1000 (+ sec (* 60 (+ min (* 60 hour))))))]))

(define (millis->time n)
  (define s 1000)
  (define m (* s 60))
  (define h (* m 60))
  (racetime (quotient n h)
            (quotient (remainder n h) m)
            (quotient (remainder n m) s)
            (remainder n s)))

(define (time-diff a b)
  (match (list a b)
    [(list #f _) #f]
    [(list _ #f) #f]
    [_ (millis->time (- (time->millis a) (time->millis b)))]))

