#lang racket
(require rackunit)
;; make a cons cell one with the string and other is the k
;; takes a file path and the k order of your model
(define (markModel file k)
  (if (file-exists? file)
      (cons (string-trim (file->string file)) k)
      (error "file does not exits sorry")))

;; selectors
(define (order mM)
  (cdr mM))

(define (kgram mM)
  (car mM))

;; apply regexp then length
(define count-substring 
  (compose length regexp-match*))

(define (freqOfStr mM str)
  (if (< (order mM) (string-length (kgram mM)))
      (count-substring str (kgram mM))
      (error "order is bigger then the length of the input so MarkModel can't be done")))

;;(define (freqOfChar mM c)
;;  (if (< (order mM) (length (kgram mM)))
;;      (foldr (λ (x lst) (+ (if (eq? x c) 1 0) lst)) 0 (string->list (kgram mM)))
;;      (error "order is bigger then the length of the input so MarkModel can't be done")))

(define (freqOfChar mM str c)
  (let ([x (string-append str c)])
       (if (< (order mM) (string-length (kgram mM)))
           (count-substring x (kgram mM))
           (error "order is bigger then the length of the input so MarkModel can't be done"))))

(define (gen kStr lenKStr) 1)
  
(define test  (markModel "test.txt" 3))

;;(define in (open-input-file "test.txt"))
;;(define out (open-output-file "output.txt"))

(test-begin
 "Test string is a modified string from princeton"
 (let ([mm (markModel "test.txt" 1)])
   (check = (order mm) 1)
   (check-not-equal? (order mm) 5)
   (check-equal? (kgram mm) "gagggagaggcgagaaa")
   (check-not-equal? (kgram mm) "jakejakejakejake")
   (check = (freqOfStr mm "j") 0)
   (check = (freqOfStr mm "g") 9)
   (check = (freqOfStr mm "a") 7)
   (check = (freqOfStr mm "c") 1)
   (check =  (freqOfChar mm "g" "a") 5)
   (check =  (freqOfChar mm "g" "c") 1)
   (check =  (freqOfChar mm "c" "g") 1)
   ;;(check =  (freqOfChar mm "g" "g") 3);;2 returning should be three
   ;;(check =  (freqOfChar mm "a" "a") 2);;1 retrning should be 2
   ))