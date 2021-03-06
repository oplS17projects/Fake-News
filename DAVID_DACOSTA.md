# Web parsing & servlet hosting for Fake News Generator

## David DaCosta
### April 22, 2017

# Overview
- ### [Web Parsing](https://github.com/oplS17projects/Fake-News/blob/master/get_url_return_news.rkt)

Our project recieves news articles (or any website or text file theoretically) from the user & outputs onto a servlet the original body of text alongside a fake news generated by the Markov Model object. My job was to take care of the parsing. It was pretty easy to incorporate the courses elements as a majority of my job was what I saw as asci manipulation, which boils down to string stuff. Functional is phenomenal for this. Maps, filters, anonymous functions, predicates; all super usefull. Anyway, I pull paragraph tags from a url and store it into a list such that I have a list of strings that are all the paragraph tags in any given article. Then I run a series of procedures on this data structure, each time creating a new list of strings, each more refined, until it's perfect and just news. When we get to that point my job is done and I give Jake the string so his object can mangle it into balogna & I can host it on the servlet. The results are usually funny. 

So where-as Jake's code is very algorithmically elegant & impressive in that regard, I was more focused on getting these libraries that I had elected to work with to do what I needed them to do. It was rather frustrating having to pickup and learn a lot of these new tools as opposed to using just the skills we learned in the course. Ultimately, I did get to put a lot of my skills to good use as well as learned some cool web parsing tricks, so overall I'm happy with how this assignment turned out.  Honestly, where-as initially during the course functional seemed strange, now it seems powerful by comparison. Jake's also a peach to work with, so there's that.

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
* The ```web-server/servlet``` library allows us to host a small local site to compare the fake and real news.
* The ```html-parsing    html``` libraries allows us to create a parse trees of HTML source elements.
* The ```xml/path    xml``` libraries allows us to use the integral se-path*/list function to get all of the paragraph tags from the parse tree we generate. 

# Key Code Excerpts

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

This function uses a map with an anonymous function and regular expressions to replace all JSON \u00AD instances in text that will sometimes show up. 
```racket
(define (remove-json1 alist)
  (map (lambda (n)
         (regexp-replace #rx"\u00AD" n "") )
       alist))
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

An earlier version of the program actually needed to output to a file for the Markov Model object, as opposed to it accepting a single string. This other list walk satisfied the property of having to out put the list of strings we end up with after parsing into a file. 
```racket
;this proves to be a challenge
;(define (list-walk-print listn)
; (if (null? listn)
;    " "
;   (begin
;    (fprintf out (car listn))
;   (newline out)
;  (list-walk-print (cdr listn)))))
```

Here is another example of recursion & list traversal used to return a list of all the text before the first string less then 50 characters, using the predicates we define to identify when a string is less then 50 characters in length. This is because most paragraph news text is significantly greater then this in length so we can use that property to properly gather news from websites. 
```racket
;get rid of anything after
(define (cut-end-noise alist)
  (if (is-this-the-end? (car alist))
      '()
      (cons (car alist) (cut-end-noise (cdr alist))) ) )
```
        
## 3. Predicates
There is a lot of auxilary noise on news websites that are in the form of text. This can be advertisements, comment sections or other articles and their thumbnails. These predicates were created to determine at what point in the source the scraper determines is news or isn't. How did we go about this? Well, paragraphs of text tend to be larger in terms of asci length. Therefore, we can determine that we don't want any text before the first significantly sized body of text. That's what length-means-news accounts for. As for the end of most news, we search for anything less then 50. This will usually be </body> or something of that nature. Predicates provide some abstraction as well as readability, I think, for the overall body of code, which I believe follows a consistent nomenclature. 
```racket
;make sure we're only dealing with the news,
(define (length-mean-news? teststring)
(> (string-length teststring) 114) )

(define (is-this-the-end? teststring)
  (< (string-length teststring) 50) )
  ```
  
## 4. Procedural Abstraction / Filter / Lambda
Another filter, this time defined with an anonymous function so that we can work on the elements of the list argument we pass it. This is the filter mentioned earlier that removes all strings that start with the asci "<" character. This is just one example of the abstraction used to aid modularity & readability in the code. 
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
Frequently in functional programming, when writing a recursive function, we need helper functions to process end goal behavior. This function pre-news-noise that utilizes the predicates we describe above, has it's own helper function that calls the filter-tags-brute-force function also described above. What filter-pre-news-noise does is it checks for the first string that satisfies length-mean-news? and then calls filter pre news noise to return everything after that.
```racket
;get rid of anything before
(define (filter-pre-news-noise mylistofstrings)
  (if (length-mean-news? (car mylistofstrings))
      (now-parse-the-goods mylistofstrings)
      (filter-pre-news-noise (cdr mylistofstrings)) ) )

;helper function
(define (now-parse-the-goods listofstrings)
  (filter-tags-brute-force listofstrings) )
```

# Early implementation
- ### [web-scraper.rkt](https://github.com/oplS17projects/Fake-News/blob/master/web-scraper.rkt)
You'll notice there's a file in the repo web-scraper.rkt. Well that's where I spent a lot of the early figuring out days getting all of my half set up. The file runs pretty much on it's on. You give the user a url input in an input field based on the Meta Circular Evaluator assignment setup, then it does some of the primitive filters I had before and outputs the strings after all of the dirty work is done onto a servlet. It just didn't have the Markov Model aspect implemented as Jake was working on it at the time. The idea at the time was to also output the strings into a file for the markov model so it did that behavior as well.

```racket
(define (prompt-for-input string)
  (newline) (newline) (display string))

(define input-prompt "Enter REAL NEWS:  ")

(define out (open-output-file "foodforMarkov.txt" #:exists 'replace))
```


# Auxiliary tech

This is the main servlet body that generates the website. The css for it was written in Bootstrap because I'm a big fan of Bootstrap. It's based on the freelancer template. So, I really like how Functional Programmings indentation convention suits web code very well. Seems like their ideal for each other honestly in that regard. 
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

#### Code that I wrote:
- [get_url_return_news.rkt](https://github.com/oplS17projects/Fake-News/blob/master/get_url_return_news.rkt)
- [web-scraper.rkt](https://github.com/oplS17projects/Fake-News/blob/master/web-scraper.rkt) 
- [webscrape.css](https://github.com/oplS17projects/Fake-News/blob/master/css/webscrape.css) 
