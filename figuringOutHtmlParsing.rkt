#lang racket

(require html)
;http://docs.racket-lang.org/html/
(require html-parsing)
;https://docs.racket-lang.org/html-parsing/#%28part._.Interface%29
(require net/url)
;https://docs.racket-lang.org/net/url.html

 
; Some of the symbols in html and xml conflict with
; each other and with racket/base language, so we prefix
; to avoid namespace conflict.
(require (prefix-in h: html)
         (prefix-in x: xml))

;these are the ports created from urls' of some news outlets
(define nprurl (string->url "http://www.npr.org/"))
(define nprport (get-pure-port nprurl))

(define huffposturl (string->url "http://www.huffingtonpost.com/"))
(define huffpostport (get-pure-port huffposturl))

(define cnnurl (string->url "http://www.cnn.com/"))
(define cnnport (get-pure-port cnnurl))

(define usatodayurl (string->url "http://www.usatoday.com/"))
(define usatodayport (get-pure-port usatodayurl))

(define washposturl (string->url "https://www.washingtonpost.com/"))
(define washpostport (get-pure-port washposturl))

;here we're creating html object instances of them
(define npr-html (read-html nprport))
(define huffpost-html (read-html huffpostport))
(define cnn-html (read-html cnnport))
(define usatoday-html (read-html usatodayport))
(define washpost-html (read-html washpostport))
  
; extract-pcdata: html-content/c -> (listof string)
; Pulls out the pcdata strings from some-content.
(define (extract-pcdata some-content)
  (cond [(x:pcdata? some-content)
         (list (x:pcdata-string some-content))]
        [(x:entity? some-content)
         (list)]
        [else
         (extract-pcdata-from-element some-content)]))
 
; extract-pcdata-from-element: html-element -> (listof string)
; Pulls out the pcdata strings from an-html-element.
(define (extract-pcdata-from-element an-html-element)
  (match an-html-element
    [(struct h:html-full (attributes content))
     (apply append (map extract-pcdata content))]
 
    [(struct h:html-element (attributes))
     '()]))
 

;list of news outlets
(define realnewslist (list
                      npr-html
                      huffpost-html
                      cnn-html
                      usatoday-html
                      washpost-html))

;iterator to iterate through our list of news outlets
(define (myiter newslist)
  (if (null? newslist)
      "done"
      (begin
        (printf "~s\n" (extract-pcdata (car newslist)))
        (myiter (cdr newslist)))))


;So All we're doing here is calling the iterator to iterate through our list
;and print out the source of each website. 
(myiter realnewslist)

