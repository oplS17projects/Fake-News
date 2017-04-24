# Fake-News :newspaper: :radio: :tv: :computer: 
Dave &amp; Jake's Fake News Generator 
## Statement

   We have implemented a Fake News Generator. Given the current political climate of hysteria regarding "Fake News", we figured it would be funny to create an application which dynamically creates believable Fake News using information from data collected by web scraping real news websites. Neither of us have done web scraping before and find it interesting.

## Analysis

   We essentially used data abstraction in creating the functions that pull specific parts from HTML bodies, as well as in defining the types of objects pulled. We'll probably utilize some filters to parse HTML tags & JSON. We'll be creating an expression evaluator to evaluate html elements into their respective object containers. We're going to recurse through our data structures to find data as well as attempt to create closures and objects to encapsulate different "Fake News" pages.
   Originally I thought I would just store the input string and the order in a cons cell. But then I realized that there were parts of the Markov Model that need other members to be calculated first to get their own value. But I porotype the cons cell version as you can see below. But I later came to the realization that I could use a tagged list so I started over a rewrote the whole thing which gave me the results that I was looking for.
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
   Here is where I create the tagged list.
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
   This allowed me to use message passing in order to get at different parts of the object. this also made testing a lot easier because I could see all the objects members at once. This was really only helpful with small inputs with small order values. 
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
   ))
   ```
   
We are going to be webscraping data from typical news outlets such as:
"http://www.npr.org/"
"http://www.huffingtonpost.com/"
"http://www.cnn.com/"
"http://www.usatoday.com/"
"https://www.washingtonpost.com/"

Here is what url pulling looks like4\:
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

Upon looking at some sample source for websites, we decided that most of the news content on news websites is in the paragraph tags. Therefore, we concentrated solely on pulling the paragraph tags from the html-parsing parse trees. At current understanding, after we pull these sections, we will be putting the ones we want into a text file. We are going to isolate the bodies of articles as we are not concerned with titles or authors. After the data is collected we will run the text file through our markov model program to generate a "false" implementation based on the probabilities of words in succession. We're shooting for at least semi coherent yet humorous results.

## Deliverable and Demonstration

What this project is a functional fake news generator that will generate fake news by scraping a real news article off of the internet. Then that real news is feed into a Markov Model as a string which will then generate a fake news article. 
Additional functionality that we added was making a user interface where the user can input the url, order, and other information need to generate an article. Once that article is generated it get sent to the racket file that will make website that will display the real news next to are fake news so you can compare the results.
In are demo you are able to give are program the url of the article that you want base you fake news off of. Also you will have the option to look at any part of the object that you want to see. Then it will show up on the website for you to check out.


## Architecture Diagram
![realnewsorfake](/Block2.PNG?raw=true "FAKE NEWS")

1. The program reads a text file. The file should be filled with multiple news stories or with stories that you want spliced into the new generated file. It can be user created. 
2. You will alternatively be able to give the program a website url which should link to real news stories.

   2a. The program will then generate a text file to be read by the Markov Model.

3. Next we make a markov model object which is a cons cell. You can see this in more detail in the sections above. A general idea is that it takes the file, extracts strings out of it and stores in in the car of the cons cell and you also give the constructor a integer which will be the order which is the size of the k-gram.
4. Then we take the Markov model object and generate an output.
  
   4a. There will be special cases for unit testing to confirm that the program is working properly.
   
   4b. We will also output the result of that we generated into a text file.


## Schedule

### First Milestone (Sun Apr 9) 
- [x] By the first milestone, the plan is to have all of the Markov Model object & web scraping done and have both pass an acceptable amount of unit tests.

### Second Milestone (Sun Apr 16)
- [x] We hope to have the Markov Model able to generate at least semi comprehensible fake news from a text file generated by the web parsing program.

### Public Presentation (Fri Apr 28)
- [ ]
For the public presentation, we will have a working fake news generator with some already generated news for people to look at.
Our Block diagram and unit tests will demonstrate how our program works.

## Stretch Goals

- [x] Web hosting for static website
- [x] User interface

## Group Responsibilities

David DaCosta (@anti-dave)

- :calling: Web-scraping/Parsing Web Data
- :calling: Unit testing any web-scraping functions

Jacob Adamson (@jake-the-human)

- :cd: Markov Model Object
      - Which means I will take the input and transform it in to the data need for the object.
- :cd: Unit Tests
      - Validating both the Markov Model Object and any parsing function.
<!-- Links -->
[schedule]: https://github.com/oplS17projects/FP-Schedule
[markdown]: https://help.github.com/articles/markdown-basics/
