#lang racket
(require rackunit "MarkModel.rkt")
#|
this is test to show that the markov model works
it take the file test3 which was a news article i got
from npr and copied and pasted into that file.
|#


(define mm  (MarkovModel (string-normalize-spaces (string-trim (file->string "test3.txt"))) 12))
(define test-news ((mm 'get-the-news) 1000))

(define out (open-output-file "outputTest3.txt" #:exists 'replace))
(write test-news out)
(close-output-port out)