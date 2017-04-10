#lang racket

(require xml
         xml/path
         net/url
         html-parsing
         html
         web-server/servlet
         web-server/servlet-env)

;Web scraper & Formatter

;Samples
;http://www.npr.org/2017/04/06/522747322/first-listen-sexmob-cultural-capital

(define realnews  "Real news will go here")

(define fakenews "Fake news will go here")

(define (mypage req)
  (response/xexpr
   `(html (head (title "DONNY T'S FAKE NEWS MACHINE")
          (body (div ((class "jumbotron text-center"))
                     (h1 "FAKENEWS"))
                (div ((class "container"))
                     (div ((class "row"))
                          (div ((class "col-xs-6 col-centered text-center"))
                               (h2 "Real News")
                               (p "Article before being run through markov Model"))
                          (div ((class "col-xs-6 col-centered text-center"))
                               (h2 "Fake News")
                               (p "Article after having been run through markov model")))))))))

(define root (current-directory))

(define (prompt-for-input string)
  (newline) (newline) (display string))

(define input-prompt "Enter REAL NEWS:  ")

(define (list-walk-print listn)
  (if (null? listn)
      " "
      (begin
        (write (car listn) out)
        (newline out)
        (list-walk-print (cdr listn)))))

(define out (open-output-file "foodforMarkov.txt"))

;driver
(define (driver-loop)
  (begin
    
    (prompt-for-input input-prompt)
    
    (define input (read-line))
    
    (define myurl (string->url input))
   
    (define myport (get-pure-port myurl))

    (define mylist (html->xexp myport))

    (list-walk-print
     (se-path*/list '(title) mylist) )

    (list-walk-print
     (se-path*/list '(p) mylist) )
    ;we need to filter stuff
    ;(remove* (lambda (lst) equal? lst) )

    (close-output-port out)

    ;call markov model on foodforMarkov here

    ;put stuff on website
    (serve/servlet mypage)))

(driver-loop)