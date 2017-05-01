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
         html
         web-server/servlet
         web-server/servlet-env)
```

* The ```net/url``` library provides the ability to make REST-style https queries to the Google Drive API.
* The ```web-server/servlet``` library allows us .
* The ```html-parsing``` library allows us to create a parse trees of HTML source elements.

# Key Code Excerpts

Here is a discussion of the most essential procedures, including a description of how they embody ideas from 
UMass Lowell's COMP.3010 Organization of Programming languages course.

Five examples are shown and they are individually numbered. 

## 1. Map & Filter
So when we give the program a URL, we then create a list of all html paragraph tags. Then we have end up with a list of html objects, so we use the map function to turn these html objects into strings. Now, we can do asci manipulation. So, some of those paragraph strings will now have text in them, others have child html elements or links perhaps & such. Since we only want text that is actually news, we use filters to remove any lines that have the opening "<" asci charcter, as that signifies non text. Filters, maps, & list/string operations are a core proponent of functional programming. 
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
After all of the strings in our list of strings are what we want, we define this list walk function that appends all of them into one final string object for the Markov Model; as it accepts strings. 
```racket
;Finally build final string to return
(define mynews (recurse-append stringlist ) )
    
(define (recurse-append lst)
    (if (null? lst)
        "\0"
        (string-append (car lst) (recurse-append (cdr lst)))))
```
        
## 3. Predicates
There is a lot of auxilary noise on news websites that are in the form of text. This can be advertisements, comment sections or other articles and their thumbnails. These predicates were created to determine at what point in the source the scraper determines is news or isn't. How did we go about this? Well, paragraphs of text tend to be larger in terms of asci length. Therefore, we can determine that we don't want any text before the first significantly sized body of text. That's what length-means-news accounts for. As for the end of most news, we search for anything less then 50. This will usually be </body> or something of that nature. Predicates provide some abstraction as well as readability, I think, for the overall body of code, which I believe follows a consistent nomenclature. 
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
  
