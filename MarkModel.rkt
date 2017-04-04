#lang racket

(define (MarkovModel text order)
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
             )
        (list 'Mm
              (list 'kgrams (remove-duplicates raw-kgram))
              (list 'frq-kgram (filter-kgram raw-kgram))
              (list 'total (list (foldr + 0 (filter-kgram raw-kgram))))
              (list 'alpha (remove-duplicates alpha))
              (list 'total-char total-char)
              (list 'alpha-freq alpha-freq)
              (list 'alpha-prob (map (λ (n) (char-prob n)) alpha-freq)))
        )))

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

(define (accumulate-tree tree term combiner null-value)
  (cond [(empty? tree) null-value]
        [(not (pair? tree)) (term tree)]
        [else (combiner (accumulate-tree  (car tree) term combiner null-value)
                        (accumulate-tree  (cdr tree) term combiner null-value))]))

(define (char-prob lst)
  (let ([temp (foldr + 0 lst)])
    (map (λ (n) (/ n temp)) lst)))
