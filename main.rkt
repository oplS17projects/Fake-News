#lang racket
(require "MarkModel.rkt"
         "get_url_return_news.rkt"
         "infowars.rkt") ;real deal
;(require "MarkModel.rkt") ; just for testing
;(provide mynews fakenews)


;; gets input from users
;; gets url
(define input-prompt "Enter a URL of a news artcle that you would like to base yours on:  ")
(printf  input-prompt)
(newline)
(define input-news (read-line))


;; gets order
(define order-prompt "What order do you want to make your model with. 7 to 12 is recommended: ")
(printf order-prompt)
(newline)
(define order (or (string->number (read-line))
                  (error "You need to enter a number.")))

;; get the length of the article
(define length-article-prompt "How long do you want the artcle. Must be greater then your order. ")
(printf length-article-prompt)
(newline)
(define length-article (or (string->number (read-line))
                           (error "You need to enter a number.")))

(define options-list '(obj kgram freq-kgram total alpha total-char alpha-freq get-the-news))

(define options
  "(define fake-news (MarkovModel file-name.txt 7))
7 = order of the model which is the length of kgrams

obj
-return most of the imporant members in the object in the form of nested list
-but you can acces them all indivauly as well with keywords

kgram
-returns all the valid kgrams in the object

freq-kgram
-returns the freq of the kgrams

total
the total number of each kgram

alpha
-returns the all the char in the model

total-char
-return the total number of each char

alpha-freq
-the frequency of the char that follow a kgram

get-the-news
-250 is the numbers of char you want to generate
-this will generate the fake news

please enter one of the symbol that you want to pass in to the model

")

;; lets you get different info from the object by writing -o checkout string above for
;; the details
(define message-prompt "Input -o for option or write get-the-news to just gen fake news")
(printf message-prompt)
(newline)
(define message (read-line))

;; checks user input for options and to see if it is valid
(define (users-op mess)
  (cond
    [(equal? "get-the-news" mess) (begin (set! message (string->symbol mess)) message)]
    [(equal? mess "-o")
     (begin (display options)
            (display "Which option would you like: ")
            (newline)
            (set! message (string->symbol (read-line))) message)]
    [else (error "NOT valid input!!!")]))
;; checks that the message passed in is valid one from a list of valid messages
(define (mess-check mess)
  (member mess options-list))

(define fake-news 1) ;; where we are going to store are fake news

(define (main)
  ;; sets up user input to a valid state for the markov model
  (begin (users-op message)
         (or (mess-check message) (error "Bad Message!"))
         ;;sets fake-news to the markovmodel
         (set! fake-news (MarkovModel (get-url-return-news input-news) order))
         ;; sets fake-news to the fake news i use fake-news again b/c we dont need the o
         ;; object after we generate the fake news
         (set! fake-news ((fake-news message) length-article))
         ;; now we write the output to a file
         (define out (open-output-file "Fake-New.txt" #:exists 'replace))
         (write fake-news out)
         (close-output-port out)))

;; FAKER NEWS
;; main gets called
(main)
;; fuction that call the website
(infowars)
