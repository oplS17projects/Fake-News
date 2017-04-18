#lang racket
(require "MarkModel.rkt")

(define input-prompt "Enter a URL of a news artcle that you would like to base yours on:  ")
;When UI becomes primary concern
(printf  input-prompt)
(newline)
(define input-news (read-line))



(define order-prompt "Order that you want to make your model with. 7 to 12 is recommended: ")
(printf order-prompt)
(newline)
(define order (or (string->number (read-line))
                  (error "You need to enter a number.")))


(define length-article-prompt "How long do you want the artcle: ")
(printf length-article-prompt)
(newline)
(define length-article (or (string->number (read-line))
                           (error "You need to enter a number."))) ;; need to cast to an int

(define options
  "(define fake-news (MarkovModel file-name.txt 7))
7 = order of the model which is the length of kgrams

(fake-news 'obj)
return most of the imporant members in the object in the form of nested list
but you can acces them all indivauly as well with keywords

(fake-news 'kgram)
returns all the valid kgrams in the object

(fake-news 'freq-kgram)
returns the freq of the kgrams

(fake-news 'total)
the total number of each kgram

(fake-news 'alpha)
returns the all the char in the model

(fake-news 'total-char)
return the total number of each char

(fake-news 'alpha-freq)
the frequency of the char that follow a kgram

(fake-news 'order)
returns the order

(fake-news 'news)
returns raw string of the input string

((fake-news 'get-the-news) 250)
250 is the numbers of char you want to generate
this will generate the fake news

please enter one of the symbol that you want to pass in to the model

")


(define message-prompt "input -o for option or write fake-news to just gen fake news")
(printf message-prompt)
(newline)
(define message (read-line))

(define (users-op mess)
  (if (equal? mess "-o")
      (begin (display options)
             (display "Which option would you like: ")
             (newline)
             (set! message (read-line)))
      1))

(define (main)
  (users-op message))

(main)