#lang racket
(require racket/random)
(provide (all-defined-out)) ;; lets other files to access MarkovModel

(define (MarkovModel text order)
  ;;helper functions

  ;; breaks up input into a list of kgram of size order
  (define (get-kgrams str order)
    (let ([count 0])
      (if (< (string-length str) order)
          '()
          (cons (substring str count order)
                (get-kgrams (substring str (add1 count)) order)))))

  ;; gets rid of any mult copies of kgram so there is only one of each
  (define (filter-kgram lst)
    (if (empty? lst)
        '()
        (cons (count (λ (x) (equal? (car lst) x)) lst)
              (filter-kgram (filter (λ (x) (not (string=? (car lst) x))) (cdr lst))))))

  ;; use bucket to help with offset of kgrams to count the number of letters that
  ;; come after the kgram
  (define (bucket order lst)
    (define (bucketf num items)
      (if (empty? items) (list (list num))
          (let ((head (car items)))
            (if (string=? (substring num 0 order) (substring (car head) 0 order))
                (cons (cons num head) (cdr items))
                (cons (list num) items)))))
    (foldr bucketf '() lst))

  ;; counts the number of times a char follows a kgram
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
  
  ;; reduce the char prob list
  (define (char-prob lst)
    (let ([temp (foldr + 0 lst)])
      (map (λ (n) (/ n temp)) lst)))

  ;; creates prob index 
  (define (prob-alpha-help alpha-num char)
    (if (= 0 alpha-num)
        '()
        (cons char (prob-alpha-help (sub1 alpha-num) char))))

  ;; as it move though on list it changes another
  ;; this is used to make the prob list lists
  (define (prob-helper alpha-lst alpha-frq)
    (if (and(empty? alpha-lst)(empty? alpha-frq))
        '()
        (append (prob-alpha-help (car alpha-frq) (car alpha-lst))
                (prob-helper (cdr alpha-lst) (cdr alpha-frq)))))

  ;; this function will take a kgram list of kgrams and the prob-helper and rerturn the
  ;; a random valid char
(define (rando your-kgram the-lst-of-kgrams prob-helper)
  (define (rando-helper your-kgram prob-helper the-lst-of-kgrams)
    (if (equal? your-kgram (car the-lst-of-kgrams))
        (car prob-helper)
        (rando-helper your-kgram (cdr prob-helper )(cdr the-lst-of-kgrams))))
  (if (member your-kgram the-lst-of-kgrams)
      (random-ref (rando-helper your-kgram prob-helper the-lst-of-kgrams))
      ( error "sorry cant find your kgram." your-kgram)))

  ;; this function does the work of generating the text
  ;; it does this by taking a kgram then finding a random char
  ;; to follow it. then makes a subtring of length of the order
  ;; and then gets another valid random char and appends it
  ;; it does this over and over again untill length-of-news reaches 0
  (define (gen fake-news new-kgram kgrams prob-helper length-of-news)
    (if (< (sub1 length-of-news) (string-length fake-news))
        fake-news
        (begin
          (let* ([new-char (make-string 1 (rando new-kgram kgrams prob-helper))]
                 [new-news (string-append (substring new-kgram 1)  new-char)])
            ;; (display (string-append  fake-news "+" new-char " ")) ;;  uncomment for testing
            (gen (string-append fake-news  new-char)
                 new-news
                 kgrams
                 prob-helper
                 length-of-news)))))

;; had a bug where char list had doubles in it so i used this to get rid of them
(define (get-rid-doubles lst)
  (if (and (= 2 (length lst)) (check-duplicates lst))
      (remove-duplicates lst)
      lst))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; start of the object
  ;; I make a bunch of local varibles to make it easier to see the struct of the tagged list
  (let* ([news (string-normalize-spaces (string-trim  text))]
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
    ;; this is where the message passing takes place
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
             (λ (len)(let ([start-kgram (substring news 0 order)])
               (gen start-kgram start-kgram (second (second MarkovModel)) prob-helper len)))]
            [else 'badMessage])))
      )

#| how to run this
(define fake-news (MarkovModel "file-name.txt" 7))
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
|# 