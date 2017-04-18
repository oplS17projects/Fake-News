#lang racket

(require xml
         xml/path
         net/url
         html-parsing
         html
         web-server/servlet
         web-server/servlet-env)

;(require "markov.rkt")

;Web scraper & Formatter

;Sample news
;http://www.npr.org/2017/04/06/522747322/first-listen-sexmob-cultural-capital
;https://www.washingtonpost.com/world/asia_pacific/pence-makes-surprise-stop-to-demilitarized-zone-during-korea-trip/2017/04/16/e1da822e-230e-11e7-a1b3-faff0034e2de_story.html?hpid=hp_hp-top-table-main_nkorea0418-425am%3Ahomepage%2Fstory&utm_term=.9c0ce8806f38
;https://www.washingtonpost.com/world/national-security/trump-calls-turkeys-erdogan-to-congratulate-him-on-contested-referendum/2017/04/17/f997d306-2397-11e7-a1b3-faff0034e2de_story.html?hpid=hp_hp-top-table-main_usturkey-8pm%3Ahomepage%2Fstory&utm_term=.204a17b06086
  
(define (mypage req)
  (response/xexpr
   `(html (head (title "DONNY T'S FAKE NEWS MACHINE")
                (link ((rel "stylesheet")
                       (href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")
                       (type "text/css")))
                (link ((rel "stylesheet")
                       (href "/webscrape.css")
                       (type "text/css")))
                (script ((src "https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js")))
                (script ((src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"))))
          (body
           (header 
                   (div ((class "container"))
                        (div ((class "row"))
                             (div ((class "col-lg-12"))
                                       (h1 ((class "sitename"))
                                           "UML PUBLIC RADIO")
                                       (h3 ((class "quote"))
                                              " Blurring the lines ") ) ) ) )
                     
           (hr ((class "line")))
           (div ((class "container"))
                (div ((class "row"))
                     (div ((class "col-md-6 col-centered"))
                          (div ((class "text-centered"))
                               (h3 "Real News"))
                          ,mynews )
                     (div ((class "col-md-6 col-centered"))
                          (div ((class "text-centered"))
                               (h3 "Fake News"))
                          ,mynews  )))   ))) )

(define root (current-directory))

(define (prompt-for-input string)
  (newline) (newline) (display string))

(define input-prompt "Enter REAL NEWS:  ")

(define out (open-output-file "foodforMarkov.txt" #:exists 'update))

;When UI becomes primary concern
;(prompt-for-input input-prompt)
;(define input (read-line))

;for Testing
(define input  "https://www.washingtonpost.com/world/national-security/trump-calls-turkeys-erdogan-to-congratulate-him-on-contested-referendum/2017/04/17/f997d306-2397-11e7-a1b3-faff0034e2de_story.html?hpid=hp_hp-top-table-main_usturkey-8pm%3Ahomepage%2Fstory&utm_term=.204a17b06086" )
(define myurl (string->url input))  
(define myport (get-pure-port myurl))
(define myxexp (html->xexp myport))

;preprocess list of xexpressions
(define prelist (se-path*/list '(p) myxexp) ) 

; map into a list of strings
(define postlist (map (lambda (n) (xexpr->string n) ) prelist) )

;Filter to remove anything with a tag... as predicate only returns true if no tag
(define penultlist (filter (lambda (n)
                             (if (> (string-length n) 1)
                                 (not (equal? (string-ref n 0) #\<))
                                 #f))
                           postlist))

; map into a list of strings
(define stringlist (map (lambda (n) (string-trim n) ) penultlist) )

(define (recurse-append lst)
  (if (null? lst)
      "\0"
      (string-append (car lst) (recurse-append (cdr lst)))))

(define mynews (recurse-append stringlist ) )

(write mynews out)
    
(close-output-port out)

;call markov model on foodforMarkov here
;(define z (MarkovModel "foodforMarkov.txt" 7) )

;put stuff on website
(serve/servlet mypage
                 #:extra-files-paths
                 (list
                  (build-path root "css")))
