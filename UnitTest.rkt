#lang racket
(require rackunit "MarkModel.rkt")



(test-begin
 "Test string order 1"
 (let ([mm (MarkovModel (string-normalize-spaces (string-trim (file->string "test.txt"))) 1)])

   (check = (mm 'order) 1)
   (check-not-equal? (mm 'order) 5)

   (check-equal? (mm 'text) "gagggagaggcgagaaa")
   (check-not-equal? (mm 'text) "oploploploploplopl")

   (check = (first (mm 'frq-kgram)) 7) ; 'a'
   (check = (second (mm 'frq-kgram)) 1) ; 'c'
   (check = (third (mm 'frq-kgram)) 9) ; 'g'

   (let ([kgram-a (first(mm 'alpha-freq))])
     (check =  (first kgram-a) 2) ; "aa"
     (check =  (second kgram-a) 0) ; "ac"
     (check =  (third kgram-a) 5) ; "ag"
     )

   (let ([kgram-c (second (mm 'alpha-freq))])
     (check =  (first kgram-c) 0) ; "ca"
     (check =  (second kgram-c) 0) ; "cc"
     (check =  (third kgram-c) 1) ; "cg"
     )

   (let ([kgram-g (third (mm 'alpha-freq))])
     (check =  (first kgram-g) 5) ; "ga"
     (check =  (second kgram-g) 1) ; "gc"
     (check =  (third kgram-g) 3) ; "gg"
     )
   ))


(test-begin
 "Test string with order 2"
 (let ([mm (MarkovModel (string-normalize-spaces (string-trim (file->string "test.txt"))) 2)])

   (check = (mm 'order) 2)
   (check-equal? (mm 'text) "gagggagaggcgagaaa")

   (check = (first (mm 'frq-kgram)) 2) ; 'aa'
   (check = (second (mm 'frq-kgram)) 5) ; "ag"
   (check = (third (mm 'frq-kgram)) 1) ; 'cg'
   (check = (fourth (mm 'frq-kgram)) 5) ; 'ga'
   (check = (fifth (mm 'frq-kgram)) 1) ; "gc"
   (check = (sixth (mm 'frq-kgram)) 3) ; "gg"

   (let ([kgram-aa (first(mm 'alpha-freq))])
     (check =  (first kgram-aa) 1) ; "aaa"
     (check =  (second kgram-aa) 0) ; "aac"
     (check =  (third kgram-aa) 1) ; "aab"
     )

   (let ([kgram-ag (second (mm 'alpha-freq))])
     (check =  (first kgram-ag) 3) ; "aga"
     (check =  (second kgram-ag) 0) ; "agc"
     (check =  (third kgram-ag) 2) ; "agg"
     )

   (let ([kgram-cg (third (mm 'alpha-freq))])
     (check =  (first kgram-cg) 1) ; "cga"
     (check =  (second kgram-cg) 0) ; "cgc"
     (check =  (third kgram-cg) 0) ; "cgg"
     )

   (let ([kgram-ga (fourth (mm 'alpha-freq))])
     (check =  (first kgram-ga) 1) ; "cga"
     (check =  (second kgram-ga) 0) ; "cgc"
     (check =  (third kgram-ga) 4) ; "cgg"
     )

   (let ([kgram-gc (fifth (mm 'alpha-freq))])
     (check =  (first kgram-gc) 0) ; "gca"
     (check =  (second kgram-gc) 0) ; "gcc"
     (check =  (third kgram-gc) 1) ; "gcg"
     )

   (let ([kgram-gg (sixth (mm 'alpha-freq))])
     (check =  (first kgram-gg) 1) ; "gga"
     (check =  (second kgram-gg) 1) ; "ggc"
     (check =  (third kgram-gg) 1) ; "ggg"
     )
   ))
