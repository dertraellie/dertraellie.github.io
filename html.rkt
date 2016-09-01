#lang racket
(require "structs.rkt")
(provide event/results->table)

(define (event/results->table e/r)  
  (match e/r
    [(event/results (event e-cat e-date e-id)
                    stages)
     `(div
       ((class "result"))
       (h3 () ,(format "~a, ~a (~a)" (date->string e-date) e-cat e-id))
       (div ((class "scrollish"))
           (table ()
                  ,@(stages->row stages)
                  ,@(stages->result-rows stages))))]))

(define (stages->result-rows stages)
    (define p-timess (map (stages->p-times stages) (stream->list (in-range (length (stage-results (car stages)))))))
    
    (append*
      (for/list ([p-times (sort p-timess result-better)])
        (match p-times
          [(list p-name p-vehicle p-times ...)
           (define all-times (map cddr p-timess))
           
           (define times-res
             
             (for/list ([p-time p-times]
                        [s-index (in-naturals)])
               (define p-stagetime (car p-time))
               (define best-stagetime (car (sort (map (λ (l) (car (list-ref l s-index)))
                                                      all-times)
                                                 time<)))
               (define p-total (cadr p-time))
               (define best-total (car (sort (map (λ (l) (cadr (list-ref l s-index)))
                                                   all-times)
                                              time<)))
               (if (= s-index 0)
                   (tds p-name p-stagetime best-stagetime #t)
                   (append (tds p-name p-stagetime best-stagetime #f)
                           (tds p-name p-total best-total #f)))))
           
           `((tr ()
                (td ((class ,(~a "color-" p-name))) ,p-name)
                (td ((class ,(~a "color-" p-name))) ,(or p-vehicle ""))
                ,@(append* times-res)))]))))

(define (tds p-name t best double-col?)
  (define diff (time-diff t best))
  (define is-best? (and t (equal? t best)))
  (define class/col (append (if double-col? `((colspan "2")) '())
                            (if is-best? `((class ,(~a "color-" p-name))) '())))
  `((td ,class/col ,(time->string t))
    (td ,class/col ,(~a (if diff "+" "") (time->string diff)))))

(define (sorted-player-indexes s)
    (match s
      [(stage _
              _
              (list (race-result _ _ ts)
                    ...))
       (define idx/ts (for/list ([t ts] [i (in-naturals)]) (list i t)))
       (map car (sort idx/ts (λ (a b) (time< (cadr a) (cadr b)))))]))

(define (stages->row stages)
    (define playersn (length (stage-results (car stages))))
    `((tr ()
         (th ((rowspan "3")) "Player")
         (th ((rowspan "3")) "Vehicle")
         ,@(add-between (map (λ (s) `(th ((colspan "4")) ,(hash-ref (stage-info s) 'location))) stages)
                        `(th ((rowspan ,(~a (+ 3 playersn)))) "")))
      (tr ()
          ,@(map (λ (s) `(th ((colspan "4")) ,(stage-name s))) stages))
      (tr ()
          (th ((colspan "4")) "Stage")
          ,@(append* (map (λ (s)
                            `((th ((colspan "2")) "Stage") (th ((colspan "2")) "Total")))
                          (cdr stages))))))

(define ((stages->p-times stages) p-index)
    (match-define (race-result (player _ p-name _) p-vehicle _)
      (list-ref (stage-results (car stages)) p-index))
    
    (define (foo s)
      (define results (stage-results s))
      (race-result-time (list-ref results p-index)))

    (define totals (map foo stages))
    (define prevs (cons (racetime 0 0 0 0) (take totals (- (length totals) 1))))
    (list* p-name
           p-vehicle
           (map (λ (total prev)
                  (list (time-diff total prev) total))
                totals
                prevs)))
(define (result-better a b)
      (match* (a b)
        [((list _ _ as ...) (list _ _ bs ...))
         (match* ((memf (compose cadr identity) (reverse as)) (memf (compose cadr identity) (reverse bs)))
           [(#f #f) #f]
           [(#f _) #f]
           [(_ #f) #t]
           [(al bl) (if (not (= (length al) (length bl)))
                        (> (length al) (length bl))
                        (time< (cadar al) (cadar bl)))])]))
