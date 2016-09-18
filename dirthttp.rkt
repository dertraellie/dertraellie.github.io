#lang racket
(require "structs.rkt"
         "fromjs.rkt"
         "html.rkt"
         "http.rkt"
         (only-in json string->jsexpr)
         (only-in xml string->xexpr)
         (only-in xml xexpr->string))

(define (events)
  (my-get "https://www.dirtgame.com/uk/events"))

(define (find-events s)
  (define (from-option x)
    (match x
      [`(option ((value ,id)) ,name)
       `((,name ,id))]
      ["\r\n" '()]))
  
  (define (from-select x)
    (match x
      [`(select (,_ ,_ (name ,name))
               ,options
               ...)
       
       (define e-cat
         (match (regexp-match #rx"^(.*)_prevEvents$" name)
           [(list _ s) s]))
       
       (define (foo x)
         (match x
           [(list date-str e-id)
            (match (regexp-match #rx"^([0-9][0-9])/([0-9][0-9])/([0-9][0-9][0-9][0-9])$" date-str)
              [(list _ day month year)
               (event e-cat (date (string->number year) (string->number month) (string->number day)) e-id)]
              [#f (event e-cat #f e-id)])]))
         
       (map foo (append* (map from-option options)))]))
  
  (append* (map (compose from-select string->xexpr)
                (regexp-match* #rx"<select data-ng-model=\"eventId\".*?</select>" s))))
  
(define (player-from js p)
  (match p
    [(player _ name id)
     (define res
       (findf (λ (j) (same-player? p j))
              (hash-ref js 'Entries)))
     (when (not res)
       (define almost-res
         (findf (λ (j) (equal? (string-downcase name) (string-downcase (hash-ref j 'Name))))
                (hash-ref js 'Entries)))
       (when almost-res
         (error 'wrong-player "found ~a" almost-res)))
     res]))

(define (get-event/player e-id p stage-id)
  (match p
    [(player platform p-name p-id)
     (my-get (format "https://www.dirtgame.com/uk/changeplatform?platform=~a" platform))
     (player-from (string->jsexpr (my-get (format "https://www.dirtgame.com/uk/api/event?assists=any&eventId=~a&group=all&leaderboard=true&nameSearch=~a&number=10&page=1&stageId=~a&wheel=any"
                                                  e-id
                                                  p-name
                                                  stage-id)))
                  p)]))

(define (get-events)
  (find-events (events)))

(define (same-player? p j)
  (match p
    [(player _ name id)
     (and (equal? name (hash-ref j 'Name))
          (equal? id (hash-ref j 'PlayerId)))]))

(define players (list #;(player steam "Garoof" 63353)
                      (player steam "Guggesokk" 14380)
                      (player ps4 "Gnorrf" 547927)
                      (player ps4 "frodegill" 283125)
                      (player ps4 "Eitrir" 518640)))
  
(define (get-stage-info e stage results)
  (match e
    [(event _ _ e-id)
     (define j (string->jsexpr (my-get (format "https://www.dirtgame.com/uk/api/event?assists=any&eventId=~a&group=all&leaderboard=false&stageId=~a&wheel=any" e-id stage))))
     (jsexpr/results->stage j results)]))
     
(define (get-stuff players e)
  (match e
    [(event e-cat e-date e-id)
     (define event-info (string->jsexpr (my-get (format "https://www.dirtgame.com/uk/api/event?assists=any&eventId=~a&group=all&leaderboard=false&stageId=0&wheel=any" e-id))))
     (define stages (hash-ref event-info 'TotalStages))
     (define results
       (for/list ([stage (if (= stages 1)
                             '(0)
                             (in-range 1 (+ stages 1)))])
         (unless (= stages 1)
           (printf "~a " stage))
         (get-stage-info e
                         stage
                         (for/list ([p players])
                           (match p
                             [(player platform p-name p-id)
                              (jsexpr/player->race-result (get-event/player e-id p stage) p)])))))
     (unless (= stages 1)
       (printf "~n"))
     (event/results e results)]))

(define (get/save-events es)
  (define file/hash #f)
  
  (define (save)
    (match file/hash
      [#f
       (void)]
      [(list filename _ #f)
       (printf "not saving file ~a. (not changed)\n" filename)]
      [(list filename hash #t)
      (printf "saving file ~a.\n" (car file/hash))
      (call-with-atomic-output-file filename
                                    (λ (port file)
                                      (pretty-write hash port)))]))
  
  (define res
    (for/list ([e es])
      (match e
        [(event _ #f _)
         (printf "not storing ~a. (no date)\n" e)
         (get-stuff players e)]
        [(event _ (date y m _) _)
         (define filename (year/month->string y m))
         (set! file/hash
               (cond [(and file/hash (equal? (car file/hash) filename))
                      file/hash]
                     [else
                      (save)
                      (list filename
                            (cond [(file-exists? filename)
                                   (printf "reading file ~a.\n" filename)
                                   (call-with-input-file filename read)]
                                  [else
                                   (printf "starting new file ~a.\n" filename)
                                   #hash()])
                            #f)]))
         (cond [(hash-has-key? (cadr file/hash) e)
                (printf "skipping ~a. (already stored)\n" e)
                (hash-ref (cadr file/hash) e)]
               [(printf "storing ~a.\n" e)
                (define x (get-stuff players e))
                (set! file/hash (list (car file/hash)
                                      (hash-set (cadr file/hash) e x)
                                      #t))])])))
  (save)
  res)

(define (get/save-event e)
  (car (get/save-events (list e))))

(define (get-promotional)
  (match (regexp-match #rx"<div class=\"event promotional\".*? data-ng-event-id=\"(.*?)\".*?>" (my-get "https://www.dirtgame.com/uk/events"))
    [`(,_ ,x) (event "promotional" (current-date) x)]
    [_ #f]))

(define (save-promotional)
  (match (get-promotional)
    [#f #f]
    [e (save-events (list e)) e]))

(define (save-events es)
  (define (has-date? e)
    (match e
      [(event _ #f _) (printf "skipping ~a. (no date)\n" e)
                      #f]
      [_ #t]))
  (get/save-events (filter has-date? es))
  (void))

(define (->html xexprs)
  (string-append "<!DOCTYPE HTML>\n"
                 (xexpr->string `(html (head (meta ((charset "UTF-8"))
                                                   (link ((rel "stylesheet") (href "styles.css")))))
                                       (body ,@xexprs)))))

(define (write-month-html year month)
  (define file (year/month->string year month))
  (define es (hash-values (call-with-input-file file read)))
  (define html-file (format "~a.html" file))
  (call-with-output-file html-file #:exists 'truncate
    (λ (port)
      (display (->html (map event/results->table
                            (sort es (λ (a b) (event< (event/results-event b) (event/results-event a))))))
               port))))

(define (write-current-month-html-command)
  (match (current-date)
    [(date y m _) `(write-month-html ,y ,m)]))

(define (write-events-html file es)
  (call-with-output-file file #:exists 'truncate
    (λ (port)
      (display (->html (map event/results->table
                            (get/save-events es)))
               port))))

(define (get-current-events [daily #t] [weekly #f] [monthly #f])
  (filter (λ (e)
            (match e
              [(event "daily" #f _) daily]
              [(event "daily2" #f _) daily]
              [(event "weekly" #f _) weekly]
              [(event "weekly2" #f _) weekly]
              [(event "monthly" #f _) monthly]
              [_ #f]))
          (get-events)))

(define (write-current-events-html [daily #t] [weekly #f] [monthly #f])
  (write-events-html "current.html" (get-current-events daily weekly monthly)))

`(begin (save-events (get-events))
        ,(write-current-month-html-command)
        (write-current-events-html #t #f #f))
'(save-promotional)
