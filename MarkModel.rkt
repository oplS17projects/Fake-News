#lang racket
(require rackunit racket/random)

(define (MarkovModel text order)
  ;;helper functions
  (define (get-kgrams str order)
    (let ([count 0]) 
      (if (< (string-length str) order)
          '()
          (cons (substring str count order)
                (get-kgrams (substring str (add1 count)) order)))))

  (define (filter-kgram lst)
    (if (empty? lst)
        '()
        (cons (count (λ (x) (equal? (car lst) x)) lst)
              (filter-kgram (filter (λ (x) (not (string=? (car lst) x))) (cdr lst))))))

  (define (bucket order lst)
    (define (bucketf num items)
      (if (empty? items) (list (list num))
          (let ((head (car items)))
            (if (string=? (substring num 0 order) (substring (car head) 0 order))
                (cons (cons num head) (cdr items))
                (cons (list num) items)))))
    (foldr bucketf '() lst))

  ;(char=? (last (string->list (car lst))) (car alpha)) 1 0
  (define (char-count kgram alpha)
    (if (empty? alpha)
        '()
        (cons (count (λ (x) (char=? (last x) (car alpha))) (map string->list kgram)) 
              (char-count kgram (cdr alpha)))))

  (define (accumulate-n op init seqs)
    (if (null? (car seqs))
        '()
        (cons (foldr op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

  (define (char-prob lst)
    (let ([temp (foldr + 0 lst)])
      (map (λ (n) (/ n temp)) lst)))

  ; prob index
  (define (prob-alpha-help alpha-num char)
    (if (= 0 alpha-num)
        '()
        (cons char (prob-alpha-help (sub1 alpha-num) char))))

  (define (prob-helper alpha-lst alpha-frq)
    (if (and(empty? alpha-lst)(empty? alpha-frq))
        '()
        (append (prob-alpha-help (car alpha-frq) (car alpha-lst))
                (prob-helper (cdr alpha-lst) (cdr alpha-frq)))))


(define (rando your-kgram the-lst-of-kgrams prob-helper)
  (define (rando-helper your-kgram prob-helper the-lst-of-kgrams)
    (if (equal? your-kgram (car the-lst-of-kgrams))
        (car prob-helper)
        (rando-helper your-kgram (cdr prob-helper )(cdr the-lst-of-kgrams))))
  (if (member your-kgram the-lst-of-kgrams)
      (random-ref (rando-helper your-kgram prob-helper the-lst-of-kgrams))
      ( error "sorry cant find your kgram." your-kgram)))
; (list-ref prob-helper (remainder (random 0 4294967087) (length prob-helper))) old code holding onto just in case



  (define (gen fake-news new-kgram kgrams prob-helper length-of-news)
    (if (< (sub1 length-of-news) (string-length fake-news))
        fake-news
        (begin
          (let* ([new-char (make-string 1 (rando new-kgram kgrams prob-helper))]
                 [new-news (string-append (substring new-kgram 1)  new-char)])
            ;;(display (string-append  fake-news "+" new-char " ")) ;;  uncomment for testing
            (gen (string-append fake-news  new-char)
                 new-news
                 kgrams
                 prob-helper
                 length-of-news)))))


(define (get-rid-doubles lst)
  (if (and (= 2 (length lst)) (check-duplicates lst))
      (remove-duplicates lst)
      lst))

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; start of the object
  (if (not (file-exists? text))
      (error "File does not exist sorry!")
      (let* ([news (string-normalize-spaces (string-trim (file->string text)))]
             [news-help (string-append news (substring news 0 (- order 1)))]; this will give use the circluar buffer affect
             [news-ext (string-append news (substring news 0 order))]
             [raw-kgram (sort (get-kgrams news-help order) string<?)]
             [alpha (sort(string->list news)char<?)]
             [alpha-abs (remove-duplicates alpha)]
             [kgram-offset (bucket order (sort (get-kgrams news-ext (+ order 1)) string<?))]
             [alpha-freq (map (λ (n) (char-count n alpha-abs)) kgram-offset)]
             [total-char (accumulate-n + 0 alpha-freq)]
             [prob-helper (map get-rid-doubles (map (λ (x) (prob-helper alpha-abs x)) alpha-freq))]
             [MarkovModel (list 'Mm
                                (list 'kgrams (remove-duplicates raw-kgram))
                                (list 'frq-kgram (filter-kgram raw-kgram))
                                (list 'total (list (foldr + 0 (filter-kgram raw-kgram))))
                                (list 'alpha alpha-abs)
                                (list 'total-char total-char)
                                (list 'alpha-freq alpha-freq)
                                (list 'alpha-prob (map (λ (n) (char-prob n)) alpha-freq)))]
             )
        (λ (message)
          (cond
            [(eq? 'obj message) MarkovModel]
            [(eq? 'kgram message)      (second (second MarkovModel))]
            [(eq? 'frq-kgram message)  (second (third MarkovModel))]
            [(eq? 'total message)      (second (fourth MarkovModel))]
            [(eq? 'alpha message)      (second (fifth MarkovModel))]
            [(eq? 'total-char message) (second (sixth MarkovModel))]
            [(eq? 'alpha-freq message) (second (seventh MarkovModel))]
            [(eq? 'alpha-prob message) (second (eighth MarkovModel))]
            [(eq? 'order message) order]
            [(eq? 'text message) news]
            [(eq? 'prob-helper message) prob-helper] ;for testing
            [(eq? 'get-the-news message)
             (λ (len)(let ([start-kgram (random-ref (second (second MarkovModel)))])
               (gen start-kgram start-kgram (second (second MarkovModel)) prob-helper len)))]
            [else 'badMessage])))
      ))

;(gen fake-news new-kgram kgrams prob-helper length-of-news)




(define a (MarkovModel "test.txt" 2))
(define b (MarkovModel "test2.txt" 2))
(define (test num)
  (let ([t (random-ref (a 'kgram))])
    (if (= 0 num)
        'test-done
        (begin (display (string-append ": " (b 'get-the-news)))(newline) (test (sub1 num))))))

(test 0)

(test-begin
 "Test string order 1"
 (let ([mm (MarkovModel "test.txt" 1)])
   
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
 (let ([mm (MarkovModel "test.txt" 2)])
   
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
