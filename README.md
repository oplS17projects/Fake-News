# Fake-News :newspaper: :radio: :tv: :computer: 
Dave &amp; Jake's Fake News Generator 
## Statement

   We plan on implementing a Fake News Generator. Given the current political climate of hysteria regarding "Fake News", we figured it would be funny to create an application which dynamically creates believable Fake News using information from data collected by web scraping real news websites. Neither of us have done web scraping before and find it interesting. We both love a good laugh at the expense of others.

## Analysis

   At current understanding of the project we imagine that we're going to be using data abstraction in creating the functions that will pull specific parts from HTML bodies, as well as in defining the types of objects pulled. We'll probably utilize some filters to parse HTML tags & JSON. We'll be creating an expression evaluator to evaluate html elements into their respective object containers. We're going to recurse through our data structures to find data as well as attempt to create closures and objects to encapsulate different "Fake News" pages.
   The Markov Model will be made up of a constructor and other member functions that will be outlined here. Which take a file and order. There is some procedural abstraction to get at parts of the object since it is stored in a cons cell we abstract it using car and cdr.
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
   
    
## External Technologies

### Unit Testing
One of the external libs we are using is unit testing. We are using unit tests to prove that the behavior of our program is reliable and testable, which is a hallmark of functional programing. Below is an example of one of our unit tests.
```racket
(test-begin
 "Test string is gagggagaggcgagaaa"
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
   (check =  (freqOfChar mm "g" "g") 3)
   (check =  (freqOfChar mm "a" "a") 2)
   ))
   ```
   
We are going to be webscraping data from typical news outlets such as:
"http://www.npr.org/"
"http://www.huffingtonpost.com/"
"http://www.cnn.com/"
"http://www.usatoday.com/"
"https://www.washingtonpost.com/"

We are going to be looking for title elements, heading tags, footers, headers, & paragraph tags. At current understanding we will be storing these into their respective objects. By objects what we mean is that we'll most likely define lists of strings, where the strings will be of these categories, & then the list will be defined with it's respective and appropriate name. After the data is collected we will run the categorized bodies through our markov model program to generate a "false" implementation based on the probabilities of words in succession. We're shooting for at least semi coherent yet humorous results.

## Deliverable and Demonstration

When the project is done we should have a fully functional fake news generator that will generate fake news by scraping real news off of real news websites and creating false versions of the parts of a standard html article. All appropriate modules will be unit tested to prove functionality. During our live demo we will be showing some of the data that we collect and then we will run the generator and show a few fake articles.

## Architecture Diagram
![realnewsorfake](/realnewsorfake.png?raw=true "FAKE NEWS")

1. You can give are program a text file. Which should be filled with multiple news stories or with stories that you want spliced into the new generated file.
2. Or you will be able to give the program a website which should like to real news stories.

   2a. The those stories will go though text file generator to get the data ready for the program to turn it into a Markov Model

3. Next we make a markov model object which is a cons cell you can see this in more detail in the sections above. But a general idea is that it takes the file a extracts the string out of it and stores in in the car of the cons cell and you also give the constructor a integer which will be the order which is the size of the k-gram.
4. Then we take the Markov model object and generate an output.
  
   4a. There will be special cases for unit testing. To confirm that the program is working properly.
   
   4b. We also will output the result of that we generated into a text file.

Create several paragraphs of narrative to explain the pieces and how they interoperate.


## Schedule

### First Milestone (Sun Apr 9) 
- [ ]
By the first milestone, the plan is to have all the Markov Model object done and have it pass all the unit tests.

### Second Milestone (Sun Apr 16)
- [ ]
By the second milestone, we hope to have the html parsing done and unit tested. Also we hope to have the Markov Model being able to generate fake news from a text file.

### Public Presentation (Mon Apr 24, Wed Apr 26, or Fri Apr 28 [your date to be determined later])
- [ ]
For the public presentation, we will have a working fake news generator with some already generated news for people to look at.
Block diagram and unit tests will demonstrate how are program works.

## Group Responsibilities

Here each group member gets a section where they, as an individual, detail what they are responsible for in this project. Each group member writes their own Responsibility section. 
Include milestones and final deliverable.


David DaCosta (@anti-dave)

- [ ] Web-scraping/Parsing Web Data
- [ ] Unit Tests -Help Jake if he needs help with Unit testing any web-scraping functions
- [ ] Evaluator

I will work on web-scraping & efficiently as well as neatly organizing data that is scraped. Dave will be leading evaluator construction. 

Jacob Adamson (@jake-the-human)

- [ ] Markov Model Object
      - Which mean I will take the input and transform it in to the data need for the object
- [ ] Unit Tests
      - Validating both the Markov Model Object and any parsing function we write
- [ ] Evaluator
      - Help Dave if help is need
<!-- Links -->
[schedule]: https://github.com/oplS17projects/FP-Schedule
[markdown]: https://help.github.com/articles/markdown-basics/
