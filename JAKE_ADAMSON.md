# Markov Model, Unit Testing, User Input for Fake-News Project
By Jake Adamson
- All code in this write up was written by me other than the code in the Dave's Code section.


## Overview
- ### [Markov Model](https://github.com/oplS17projects/Fake-News/blob/master/MarkModel.rkt)
We decided to generate fake news by taking input as a news article that we got from the internet. The first step in the process was to parse a web page. A key part and one of the hardest parts of the parsing was making sure that the news article was a string with no weird html junk on it. Then once we got a clean string we give the news to the Markov model which takes the news as a string and an order which is the length of the kgrams. The kgrams are substrings of the article which have the same length as the order. You can see a list of the kgrams in the model by passing the object the symbol 'kgram. When the model is created, it breaks the article into kgrams and alphabet. And using those two things it generates text. It generates text by taking kgram and randomly choosing a letter to follow it based on the probity that the character would follow that kgram. (See image bellow to see it create the model piece by piece). And as you can see I used a tagged list when i made the object to make it easier to get to data that was need for other parts.

```racket
> (define t (MarkovModel "opppplllolpopl" 2))
;; Shows the internals of the object
> (t 'obj)
'(Mm
  (kgrams ("ll" "lo" "lp" "ol" "op" "pl" "po" "pp"))
  (frq-kgram (2 2 1 1 2 2 1 3))
  (total (14))
  (alpha (#\l #\o #\p))
  (total-char (5 3 6))
  (alpha-freq ((1 1 0) (1 0 1) (0 1 0) (0 0 1) (1 0 1) (1 1 0) (0 0 1) (1 0 2)))
  (alpha-prob ((1/2 1/2 0) (1/2 0 1/2) (0 1 0) (0 0 1) (1/2 0 1/2) (1/2 1/2 0) (0 0 1) (1/3 0 2/3))))
  ;;  Show step by step how the final output was made
> ((t 'get-the-news) 8)
op+p opp+l oppl+o opplo+l opplol+p opplolp+o "opplolpo"
;; you get this output only if you uncomment the noted line in the gen 
;; procedure 
```

- ### [Unit Testing](https://github.com/oplS17projects/Fake-News/blob/master/UnitTest.rkt) 
I used unit testing to validate object members to make sure that they were what I expected. Here is an example of one of my tests.
```racket
(test-begin
 ;; attempt to make some of the test easier to understand
 (define a first)
 (define c second)
 (define o third)
 (define t fourth)
 "Test3 string order 1"
 (let ([mm (MarkovModel "tacooococct" 1)])
   
;; checks the order to make sure it stored as it should be
   (check = (mm 'order) 1)
   (check-not-equal? (mm 'order) 55)
   
;; checks that the text has not changed from when it was inputted
   (check-equal? (mm 'text) "tacooococct")
   (check-not-equal? (mm 'text) "oploploploploplopl")
   
;; checks the kgrams and since the order is 1 they are only one char long
   (check = (first (mm 'frq-kgram))  1) ; 'a'
   (check = (second (mm 'frq-kgram)) 4) ; 'c'
   (check = (third (mm 'frq-kgram))  4) ; 'o'
   (check = (fourth (mm 'frq-kgram)) 2) ; 't'
;; checks how many times a kgram is followed by a specific char
   ;; in this case it is 'a' + some char
   ;; in this case 'a' is only ever followed by a 'c'
   ;; once so (c kgram-a) return 1
   (let ([kgram-a (first(mm 'alpha-freq))])
     (check =  (a kgram-a)  0) ; "aa"
     (check =  (c kgram-a)  1) ; "ac"
     (check =  (o kgram-a)  0) ; "ao"
     (check =  (t kgram-a)  0) ; "at"
     )
;; in this case it is 'c' + some char
   (let ([kgram-c (second (mm 'alpha-freq))])
     (check =  (a kgram-c)  0) ; "ca"
     (check =  (c kgram-c)  1) ; "cc"
     (check =  (o kgram-c)  2) ; "co"
     (check =  (t kgram-c)  1) ; "ct"
     )
;; in this case it is 'o' + some char
   (let ([kgram-o (third (mm 'alpha-freq))])
     (check =  (a kgram-o)  0) ; "oa"
     (check =  (c kgram-o)  2) ; "oc"
     (check =  (o kgram-o)  2) ; "og"
     (check =  (t kgram-o)  0) ; "ot"
     )
;; in this case it is 't' + some char
   (let ([kgram-t (fourth (mm 'alpha-freq))])
     (check =  (a kgram-t)  1) ; "oa"
     (check =  (c kgram-t)  0) ; "oc"
     (check =  (o kgram-t)  0) ; "og"
     (check =  (t kgram-t)  1) ; "ot"
     )
   ))

```
- ### [User Input](https://github.com/oplS17projects/Fake-News/blob/master/main.rkt)
I just ask the user for the information we need to create fake new like the url of the new story they would like to base there news off of. Also ask them for thing like the order with suggestrion and explination of what we were ask for. Also ask for things like length of the article. One thing special you could do was when prompted you could input -o for more information on the object you where creating.
```racket
(define (main)
  ;; sets up user input to a valid state for the markov model
  (begin (users-op message)
         (or (mess-check message) (error "Bad Message!"))
         ;;sets fake-news to the markovmodel
         (set! fake-news (MarkovModel (get-url-return-news input-news) order))
         ;; now we write the output to a file
         (define out (open-output-file "Fake-New.txt" #:exists 'replace))
         ;; ((fake-news message) returns a string and then is write out to
         ;; the file called Fake-New.txt
         (write ((fake-news message) length-article) out)
         (close-output-port out)))
```

## Code Samples
### 1. Message Passing and Lambda
I used message passing because I thought it would be the easiest way to a data members. Which need to use lambda functions. This allowed me to pass a message in to the object. (you can see some examples of that above) Also I used lambda when getting the length of the article you want to generate.
```racket
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
```
### 2. Tail Recursion and let*
In this code example, I used tail recursion to grow the output of the Markov model. Also, used lets to try and make the code a little easier to read. Since new-news uses new-char it need to be let`*` to make it possible to find the next char need to gen the fake news.
```racket
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
```
### 3. Higher Order Functions (Map, Filter, Fold ... Bucket)
I used higher order functions all over the place but in this example, I used them to cut down on code bloat. Example of map is in alpha-freq where I used it to count then number of time a character follows a kgram. I even used a map that return a list to be mapped again in prob-helper. Other higher order function I used filter in the list 'frq-kgram which is a modified filter call but it just filters out duplicates in the kgrams. And I used foldr to count the total number of kgrams. Also, I used bucket which is not really a higher order function it more of a fold. I used it to temporally make a list of kgrams with the add characters to figure out how many times the kgram appeared with that character at the end of it.
```racket
(let* ([news (string-normalize-spaces (string-trim  text))]
             [news-help (string-append news (substring news 0 (- order 1)))]
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
```
### 4. Closure
I used closure in the Markov model for the helper function because they do not need to exist outside that function. If you want to see this check out [MarkModel.rkt](https://github.com/oplS17projects/Fake-News/blob/master/MarkModel.rkt) file. I tried to paste an example here but the whole thing would have taken up too much room since it is my entire Markov Model procedure. But the file is linked at the end. 
```racket
define (MarkovModel text order)
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
  
  ;; ... rest of the model
  )
```

### 5. Selectors using Procedural Abstraction
I made some local Selectors for the unit test to hopefully make them easier to understand. By giving them the name of the letter that the test was testing for.
```racket
(test-begin
 ;; attempt to make some of the test easier to understand
 (define a first)
 (define c second)
 (define o third)
 (define t fourth)
 "Test3 string order 1"
 (let ([mm (MarkovModel "tacooococct" 1)])
   ;; ... some tests
   ;; these procedures are testing for the 
   ;; following letter is what the selector is
 (let ([kgram-a (first(mm 'alpha-freq))])
     (check =  (a kgram-a)  0) ; "aa"
     (check =  (c kgram-a)  1) ; "ac"
     (check =  (o kgram-a)  0) ; "ao"
     (check =  (t kgram-a)  0) ; "at"
     )
```

## Other Thoughts
Originally my idea was to make the Markov model using a cons cell that held only the original text and the order. Which I thought would be the functional way of doing thing but when I started to implement the model this way. I quickly found out that I need some parts of the object more than once which would mean that I would have to make them repeatedly. So, I switched to a tagged list where I made a bunch of local variables using let star. Which let me use a tagged list which I think works well. It also has the benefit that I can see the whole model at ones which help with troubleshooting. (Bellow is the general idea from the read me the whole version has long since been delete)
```racket
(define (markModel file k)
 (if (file-exists? file)
    (cons (string-trim (file->string file)) k)
    (error "file does not exits sorry")))
      
(define (order mM)
(cdr mM))

(define (kgram mM)
(car mM))
; these are the other member function that only have 
; rough drafts right now but mostly the return the number
; of time a str or char are in the model
; final gen generates teh probly of all the combinations
(freqOfStr mM str)
(freqOfChar mM c)
(gen kStr lenKStr)
```


## Dave's Code
Some cool code that Dave wrote. The list in list action here is pretty crazy. Not only is this racket code cool it makes a website that looks really cool. I bet this lib uses some kind of evaluator to transform these lists in to html code since the list look like they are tagged with html keywords like body, header, and dev.
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
- [MarkModel.rkt](https://github.com/oplS17projects/Fake-News/blob/master/MarkModel.rkt)
- [UnitTest.rkt](https://github.com/oplS17projects/Fake-News/blob/master/UnitTest.rkt) 
- [main.rkt](https://github.com/oplS17projects/Fake-News/blob/master/main.rkt) (Didn't write the website stuff.)
