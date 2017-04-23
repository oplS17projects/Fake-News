#lang racket

(require xml
         xml/path
         net/url
         html-parsing
         html)
         
(provide get-url-return-news)

;There are two ways to print to the file, the first is
;to append all strings in the final stringlist
;and output it as one string
;this helps remove quoation marks but without newlines
;it's a giant glob
;Function to append strings in string list
(define (recurse-append lst)
  (if (null? lst)
      "\0"
      (string-append (car lst) (recurse-append (cdr lst)))))

;Walk through list method
;this method is better as it includes newlines
;however getting the strings into an object later without quoation
;makrs  (this object is mynews for the html website)
;this proves to be a challenge
;(define (list-walk-print listn)
 ; (if (null? listn)
  ;    " "
   ;   (begin
    ;    (fprintf out (car listn))
     ;   (newline out)
      ;  (list-walk-print (cdr listn)))))

;predicate abstraction
(define (length-mean-news? teststring)
  (> (string-length teststring) 114) )

(define (is-this-the-end? teststring)
  (< (string-length teststring) 50) )

;Filter to remove anything with a tag... as predicate only returns true if no tag
(define (filter-tags-brute-force alist)
  (filter (lambda (n)
            (if (> (string-length n) 1)
                (not (equal? (string-ref n 0) #\<))
                #f))
          alist))

;make sure we're only dealign with the news,
;get rid of anything before
(define (filter-pre-news-noise mylistofstrings)
  (if (length-mean-news? (car mylistofstrings))
      (now-parse-the-goods mylistofstrings)
      (filter-pre-news-noise (cdr mylistofstrings)) ) )

;helper function
(define (now-parse-the-goods listofstrings)
  (filter-tags-brute-force listofstrings) )

;get rid of anything after
(define (cut-end-noise alist)
  (if (is-this-the-end? (car alist))
      '()
      (cons (car alist) (cut-end-noise (cdr alist))) ) )

(define (remove-json1 alist)
  (map (lambda (n)
            (regexp-replace #rx"\u00AD" n "") )
          alist))

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

    ;remove pre news body noise
    (define almostlist (filter-pre-news-noise postlist) )

    ;remove after noise
    (define almostnews (cut-end-noise almostlist))

    ;remove json
    (define stringlist (remove-json1 almostnews))

    ; map into a list of strings
    ;(define stringlist (map (lambda (n) (string-trim n) ) penultlist) )

    ;Finally build final string to return
    (define mynews (recurse-append stringlist ) )

    ;return the news
      mynews ) )

(get-url-return-news "https://www.washingtonpost.com/world/national-security/trump-calls-turkeys-erdogan-to-congratulate-him-on-contested-referendum/2017/04/17/f997d306-2397-11e7-a1b3-faff0034e2de_story.html?hpid=hp_hp-top-table-main_usturkey-8pm%3Ahomepage%2Fstory&utm_term=.204a17b06086" )
