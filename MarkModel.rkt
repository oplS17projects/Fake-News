#lang racket
(require rackunit)

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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; start of the object
  (if (not (file-exists? text))
      (error "File does not exist sorry!")
      (let* ([news (string-trim (file->string text))]
             [news-help (string-append news (substring news 0 (- order 1)))]; this will give use the circluar buffer affect
             [news-ext (string-append news (substring news 0 order))]
             [raw-kgram (sort (get-kgrams news-help order) string<?)]
             [alpha (sort(string->list news)char<?)]
             [kgram-offset (bucket order (sort (get-kgrams news-ext (+ order 1)) string<?))]
             [alpha-freq (map (λ (n) (char-count n (remove-duplicates alpha))) kgram-offset)]
             [total-char (accumulate-n + 0 alpha-freq)]
             [MarkovModel (list 'Mm
                                (list 'kgrams (remove-duplicates raw-kgram))
                                (list 'frq-kgram (filter-kgram raw-kgram))
                                (list 'total (list (foldr + 0 (filter-kgram raw-kgram))))
                                (list 'alpha (remove-duplicates alpha))
                                (list 'total-char total-char)
                                (list 'alpha-freq alpha-freq)
                                (list 'alpha-prob (map (λ (n) (char-prob n)) alpha-freq)))])
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
            [else 'badMessage])))
        
      ))

(test-begin
 "Test string"
 (let ([mm (MarkovModel "test.txt" 1)])
   
   (check = (mm 'order) 1)
   (check-not-equal? (mm 'order) 5)
   
   (check-equal? (mm 'text) "gagggagaggcgagaaa")
   (check-not-equal? (mm 'text) "oploploploploplopl")
   
   (check = (first (mm 'frq-kgram)) 7) ; 'a'
   (check = (second (mm 'frq-kgram)) 1) ; 'c'
   (check = (third (mm 'frq-kgram)) 9) ; 'g'
   
   (let ([kgram-a (first(mm 'alpha-freq))])
     (check =  (first kgram-a) 2)
     (check =  (second kgram-a) 0)
     (check =  (third kgram-a) 5)
     )

   (let ([kgram-c (second (mm 'alpha-freq))])
     (check =  (first kgram-c) 0)
     (check =  (second kgram-c) 0)
     (check =  (third kgram-c) 1)
     )

   (let ([kgram-g (third (mm 'alpha-freq))])
     (check =  (first kgram-g) 5)
     (check =  (second kgram-g) 1)
     (check =  (third kgram-g) 3)
     )
   )) 
