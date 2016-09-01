#lang racket
(require net/url
         net/url-string)

(provide my-get)

(define cookies (hash))

(define (my-get s [h (hash->headers cookies)])
  (define sp (open-output-string))
  (define p (get-impure-port (string->url s) h))
  (define heads (purify-port p))
  (update-cookies! heads)
  (copy-port p sp)
  (get-output-string sp))

(define (cookies-from s)
  (define cstrings
    (append* (for/list ([line (string-split s "\r\n")])
               (define prefix "Set-Cookie: ")
               (if (string-prefix? line prefix)
                   (list (substring line (string-length prefix)))
                   '()))))
  (for/list ([cstring cstrings])
    (map (λ (x) (string-split x "="))
         (string-split cstring "; "))))

(define (update-cookies! s)
 (for ([x (cookies-from s)])
   (set! cookies (hash-set cookies (car x) x))))

(define (hash->cookie h)
  (apply ~a
         "Cookie: "
         (add-between (map (λ (x) (~a (caar x) "=" (cadar x)))
                           (hash-values h))
                      "; ")))
(define (hash->headers h)
  (if (hash-empty? h)
      '()
      (list (hash->cookie h))))