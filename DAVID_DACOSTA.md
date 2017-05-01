# Web parsing & servlet hosting for Fake News Generator

## David DaCosta
### April 22, 2017

# Overview
- ### [Web Parsing](https://github.com/oplS17projects/Fake-News/blob/master/get_url_return_news.rkt)

We decided to generate fake news by taking input as a news article that we got from the internet. The first step in the process was to parse a web page. A key part and one of the hardest parts of the parsing was making sure that the news article was a string with no weird html junk on it. Then once we got a clean string we give the news to the Markov model which takes the news as a string and an order which is the length of the kgrams. The kgrams are substrings of the article which have the same length as the order. You can see a list of the kgrams in the model by passing the object the symbol 'kgram. When the model is created, it breaks the article into kgrams and alphabet. And using those two things it generates text. It generates text by taking kgram and randomly choosing a letter to follow it based on the probity that the character would follow that kgram. (See image bellow to see it create the model piece by piece). And as you can see I used a tagged list when i made the object to make it easier to get to data that was need for other parts.

**Authorship note:** All of the code described here was written by myself.
# Libraries Used
The code uses these libraries:

```
(require xml
         xml/path
         net/url
         html-parsing
         html)
         
(require web-server/servlet
         web-server/servlet-env)
```

* The ```net/url``` library provides the ability to make REST-style https queries to the Google Drive API.

# Key Code Excerpts

Here is a discussion of the most essential procedures, including a description of how they embody ideas from 
UMass Lowell's COMP.3010 Organization of Programming languages course.

Five examples are shown and they are individually numbered. 

## 1. Map & Filter
```racket
;establish url x expression from input string
(define myurl (string->url input))  
(define myport (get-pure-port myurl))
(define myxexp (html->xexp myport))
          
;preprocess list of xexpressions
(define prelist (se-path*/list '(p) myxexp))

; map into a list of strings
(define postlist (map (lambda (n) (xexpr->string n) ) prelist))

;remove pre news body noise
(define almostlist (filter-pre-news-noise postlist) )
```
## 2. Recursion, list traversal
```racket
;Finally build final string to return
(define mynews (recurse-append stringlist ) )
    
(define (recurse-append lst)
    (if (null? lst)
        "\0"
        (string-append (car lst) (recurse-append (cdr lst)))))
```
        
## 3. Predicates
```racket
(define (length-mean-news? teststring)
(> (string-length teststring) 114) )

(define (is-this-the-end? teststring)
  (< (string-length teststring) 50) )
  ```
  
## 4. Procedural Abstraction / Filter / Lambda
```racket

;Filter to remove anything with a tag... as predicate only returns true if no tag
(define (filter-tags-brute-force alist)
  (filter (lambda (n)
            (if (> (string-length n) 1)
                (not (equal? (string-ref n 0) #\<))
                #f))
          alist))
```

## 5. Helper Functions / Procedural Abstraction
```racket
;make sure we're only dealign with the news,
;get rid of anything before
(define (filter-pre-news-noise mylistofstrings)
  (if (length-mean-news? (car mylistofstrings))
      (now-parse-the-goods mylistofstrings)
      (filter-pre-news-noise (cdr mylistofstrings)) ) )

;helper function
(define (now-parse-the-goods listofstrings)
  (filter-tags-brute-force listofstrings) )
  ```
  
```racket
(define (mypage req)
(response/xexpr
`(html (head (title "UML INFOWARS II : Faker News!")
                (link ((rel "stylesheet")
                       (href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")
                       (type "text/css")))
                (link ((rel "stylesheet")
                       (href "/webscrape.css")
                       (type "text/css")))
                (script ((src "https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js")))
                (script ((src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"))))
          (body
           (header 
                   (div ((class "container"))
                        (div ((class "row"))
                             (div ((class "col-lg-12"))
                                       (h1 ((class "sitename"))
                                           "UML INFO WARS")
                                       (h3 ((class "quote"))
                                              " Democracy is but Mob Rule ") ) ) ) )
                     
           (hr ((class "line")))
           (div ((class "container"))
                (div ((class "row"))
                     (div ((class "col-md-6 col-centered"))
                          (div ((class "text-centered"))
                               (h3 "Real News"))
                          ,mynews )
                     (div ((class "col-md-6 col-centered"))
                          (div ((class "text-centered"))
                               (h3 "Fake News"))
                          ,fakenews  )))   ))) )
```
  
