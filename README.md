# Fake-News :newspaper: :radio: :tv: :computer: 
Dave &amp; Jake's Fake News Generator 

## Statement

   We have implemented a Fake News Generator. Given the current political climate of hysteria regarding "Fake News", we figured it would be funny to create an application which dynamically creates believable Fake News using information from data collected by web scraping real news websites. Neither of us have done web scraping before and find it interesting.
Also, if you are interested in seeing some examples click link bellow.
- [Examples of fake news](https://github.com/oplS17projects/Fake-News/blob/master/Examples-of-fake-news.txt)

## Analysis

   For Web Parsing, we essentially created functions that pull specific parts from HTML bodies, as well as in defining the types of objects pulled. We'll probably utilize some filters to parse HTML tags & JSON. We're going to recurse through our data structures to find data as well as attempt to create closures and objects to encapsulate different "Fake News" pages.
   
   For the Markov Model, originally we thought we would just store the input string and the order in a cons cell. But then we realized that there were parts of the Markov Model that need other members to be calculated first to get their own value. But we porotyped the cons cell version as you can see below. But we later came to the realization that we could use a tagged list so we started over and the whole thing was rewritten which gave us the results that we were looking for.
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
   Here is where the tagged list is created.
   ```racket
   [MarkovModel (list 'Mm
                                (list 'kgrams (remove-duplicates raw-kgram))
                                (list 'frq-kgram (filter-kgram raw-kgram))
                                (list 'total (list (foldr + 0 (filter-kgram raw-kgram))))
                                (list 'alpha alpha-abs)
                                (list 'total-char total-char)
                                (list 'alpha-freq alpha-freq)
                                (list 'alpha-prob (map (λ (n) (char-prob n)) alpha-freq)))]
   ```
   This allowed us to use message passing in order to get at different parts of the object. This also made testing a lot easier because we could see all the objects members at once. This was really only helpful with small inputs with small order values. 
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
  ```
Here is what url pulling looks like:
```racket
(define (get-url-return-news input)
  (begin

    ;Establish url x-expression from input string
    (define myurl (string->url input))  
    (define myport (get-pure-port myurl))
    (define myxexp (html->xexp myport))
          
    ;Creates a list of html paragraph tagged x-expressions
    (define prelist (se-path*/list '(p) myxexp))

    ;Map into a list of strings
    (define postlist (map (lambda (n) (xexpr->string n) ) prelist))
```
When exploring different options for getting information from websites we played different libraries and options. We went with xml and html-parsing as it creates a parse tree with branches easily accesible through se-path*/lists as can be seen in the code:
```racket
;Creates a list of html paragraph tagged x-expressions
(define prelist (se-path*/list '(p) myxexp))
```
   Upon looking at some sample source for websites, we decided that most of the news content on news websites is in the paragraph tags. Therefore, we concentrated solely on pulling the paragraph tags from the html-parsing parse trees. We like to think that the results end up at least semi coherent yet humorous results, like when quotes are repeated and such.
   
...and this is what hosting with Boostrap grids looks like
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
                          ,fake-news  )))   ))) )
```

## External Technologies

### Unit Testing
   One of the external libs we are using is unit testing. We are using unit tests to prove that the behavior of our program is reliable and testable, which is a hallmark of functional programing. Below is an example of one of our unit tests.
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
   (check = (a (mm 'frq-kgram))  1) ; 'a'
   (check = (c (mm 'frq-kgram)) 4) ; 'c'
   (check = (o (mm 'frq-kgram))  4) ; 'o'
   (check = (t (mm 'frq-kgram)) 2) ; 't'
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
   ))
   ```

## Deliverable

   What this project is a functional fake news generator that will generate fake news by scraping a real news article off of the internet. Then that real news is feed into a Markov Model as a string which will then generate a fake news article. 
   Additional functionality that we added was making a user interface where the user can input the url, order, and other information need to generate an article. Once that article is generated it get sent to the racket file that will make website that will display the real news next to are fake news so you can compare the results.
   In are demo you are able to give are program the url of the article that you want base you fake news off of. Also you will have the option to look at any part of the object that you want to see. Then it will show up on the website for you to check out.
   
## Demonstration
   To run the Fake News application, you have a few options. 
   To use a URL, simply launch the executable and the command line or DrRacket interface will prompt you for some input. First, you'll need to enter a news URL. Then you'll be asked to enter the markov Model Input.
   You may also elect to examine these text files we've supplied with examples of generated news: [Examples of fake news](https://github.com/oplS17projects/Fake-News/blob/master/Examples-of-fake-news.txt)


## Architecture Diagram
![realnewsorfake](/Block2.PNG?raw=true "FAKE NEWS")

1. The program asks the user for input which is the url where the news that you want to base you fake news off of is. The order of the kgram which is the size of the strings that the article will be broken up into (there is a recommend size to help people decide). Then they are ask for how long they would like their news to be. Next they will be asked if they would like to just generate the fake news or see a part of the Markov model they are interested in.

2. Then the url that was given to use by the user is passed off to the web parser which will return a string.

3. Now that string will be passed to the Markov model which will initialize all its members in its tagged list.
  
   + The Markov model is also united tested.

4. Based on the option that the user inputted the model will either generate fake news or return a member of it tagged list.

5. The output of the model will be a string.
   
   + Which will be captured by an output file.
   
   + It is also passed to are website creation racket file which will generate a website with the original news next to the fake news.

## Schedule

### First Milestone (Sun Apr 9) 
- [x] By the first milestone, the plan is to have all of the Markov Model object & web scraping done and have both pass an acceptable amount of unit tests.

### Second Milestone (Sun Apr 16)
- [x] We hope to have the Markov Model able to generate at least semi comprehensible fake news from a text file generated by the web parsing program.

### Public Presentation (Fri Apr 28)
- [ ]
For the public presentation, we will have a working fake news generator with some already generated news for people to look at. Our Block diagram and unit tests will demonstrate how our program works. Also if a user what to find some news on the web they can make some fake news with it.

## Stretch Goals

- [x] Web hosting for static website
- [x] User interface

## Group Responsibilities

David DaCosta (@anti-dave)

- :calling: Web-scraping/Parsing Web Data
      - I've created an application (get-url-return-news) that pulls and filters paragraph xml using functional techniques
- :calling: Web Hosting
      - I've created a small servlet that hosts the information pulled in comparison with information created using html & Bootstrap

Jacob Adamson (@jake-the-human)

- :cd: Markov Model Object
      - Which means I will take the input and transform it in to the data needed for the object.
- :cd: Unit Tests
      - Validating the Markov Model Object.
- :cd: User Interface
      - Getting input from the user to generating an output and sending that output to the appropriate places.
      
<!-- Links -->
[schedule]: https://github.com/oplS17projects/FP-Schedule
[markdown]: https://help.github.com/articles/markdown-basics/
