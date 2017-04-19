#lang racket

(require xml
         xml/path
         net/url
         html-parsing
         html)
         
(provide get-url-return-news)

;Function to append strings in string list
(define (recurse-append lst)
  (if (null? lst)
      "\0"
      (string-append (car lst) (recurse-append (cdr lst)))))

;Function you want... just pass it a string
(define (get-url-return-news input)
  (begin

    ;establish url x expression from input string
    (define myurl (string->url input))  
    (define myport (get-pure-port myurl))
    (define myxexp (html->xexp myport))
          
    ;preprocess list of xexpressions
    (define prelist (se-path*/list '(p) myxexp))

    ; map into a list of strings
    (define postlist (map (lambda (n) (xexpr->string n) ) prelist))

    ;Filter to remove anything with a tag... as predicate only returns true if no tag
    (define penultlist (filter (lambda (n)
                                 (if (> (string-length n) 1)
                                     (not (equal? (string-ref n 0) #\<))
                                     #f))
                               postlist))

    ; map into a list of strings
    (define stringlist (map (lambda (n) (string-trim n) ) penultlist) )

    ;Finally build final string to return
    (define mynews (recurse-append stringlist ) )

    ;return the news
      mynews ) )