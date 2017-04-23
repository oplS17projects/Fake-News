The report should provide an overview of your project, and then narrate your own code contributions.

You must highlight how your code embodies the ideas of the course.
    
Your report should name and briefly describe any libraries or external technology used.
    
Focus on your own contributions to the project.
    
Please see Analysis for discussion of what to highlight.
    
See this example of what a report might look like.

You should highlight three to five code excerpts.
    
```racket
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
```
    
```racket
;Finally build final string to return
(define mynews (recurse-append stringlist ) )
    
(define (recurse-append lst)
    (if (null? lst)
        "\0"
        (string-append (car lst) (recurse-append (cdr lst)))))
```
        
    
    
```racket
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
```

```racket
;make sure we're only dealign with the news,
;get rid of anything before
(define (filter-pre-news-noise mylistofstrings)
  (if (length-mean-news? (car mylistofstrings))
      (now-parse-the-goods mylistofstrings)
      (filter-pre-news-noise (cdr mylistofstrings)) ) )

;helper function
(define (now-parse-the-goods listofstrings)
  (filter-tags-brute-force listofstrings) )
  ```

Make it clear who wrote the code that you are highlighting. Ideally it's your code. But if you are writing about your partner's code, say so.
