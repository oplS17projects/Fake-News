# Fake-News
Dave &amp; Jake's Fake News Generator
Statement

   We plan on implementing a Fake News Generator. Given the current political climate of hysteria regarding "Fake News", we figured it would be funny to create an application which dynamically creates believable Fake News using information from web scraping real news websites. Neither of us have done web scraping before and find it interesting. We both love a good laugh at the expense of others. 

## Analysis

   At current understanding of the project we imagine that we're going to be using data abstraction in creating the functions that will pull specific parts from HTML bodies, as well as in defining the types of objects pulled. We'll probably utilize some filters to parse HTML tags & JSON. We'll be creating an expression evaluator to evaluate html elements into they're respective object containers. We're going to recurse through our data structures to find data as well as attempt to create closures and objects to encapsulate different "Fake News" pages. 
   The Markov Model will be made up of a Constructor and other member function that will be outlined here. Which take a file and order. There is some procedural abstraction to get at parts of the object since it is stored in a cons cell we abstract it using car and cdr.
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
One of the external libs we are using is unit test. We are using them to prove that the behavor of are program is relaible and test able which is a halmark of functional programing.

You are encouraged to develop a project that connects to external systems. For example, this includes systems that:

    retrieve information or publish data to the web
    generate or process sound
    control robots or other physical systems
    interact with databases

If your project will do anything in this category (not only the things listed above!), include this section and discuss.
Data Sets or other Source Materials

If you will be working with existing data, where will you get those data from? (Dowload from a website? Access in a database? Create in a simulation you will build? ...)

How will you convert your data into a form usable for your project?

If you are pulling data from somewhere, actually go download it and look at it before writing the proposal. Explain in some detail what your plan is for accomplishing the necessary processing.

If you are using some other starting materials, explain what they are. Basically: anything you plan to use that isn't code.





## Deliverable and Demonstration

Explain exactly what you'll have at the end. What will it be able to do at the live demo?

What exactly will you produce at the end of the project? A piece of software, yes, but what will it do? Here are some questions to think about (and answer depending on your application).

Will it run on some data, like batch mode? Will you present some analytical results of the processing? How can it be re-run on different source data?

Will it be interactive? Can you show it working? This project involves a live demo, so interactivity is good.
Evaluation of Results

How will you know if you are successful? If you include some kind of quantitative analysis, that would be good.
Architecture Diagram

Upload the architecture diagram you made for your slide presentation to your repository, and include it in-line here.
## Architecture Diagram
![realnewsorfake](/realnewsorfake.png?raw=true "FAKE NEWS")

Create several paragraphs of narrative to explain the pieces and how they interoperate.


Schedule
Explain how you will go from proposal to finished product.

There are three deliverable milestones to explicitly define, below.

The nature of deliverables depend on your project, but may include things like processed data ready for import, core algorithms implemented, interface design prototyped, etc.

You will be expected to turn in code, documentation, and data (as appropriate) at each of these stages.

Write concrete steps for your schedule to move from concept to working system.
First Milestone (Sun Apr 9)

Which portion of the work will be completed (and committed to Github) by this day?
Second Milestone (Sun Apr 16)

Which portion of the work will be completed (and committed to Github) by this day?
Public Presentation (Mon Apr 24, Wed Apr 26, or Fri Apr 28 [your date to be determined later])

What additionally will be completed before the public presentation?

## Group Responsibilities

Here each group member gets a section where they, as an individual, detail what they are responsible for in this project. Each group member writes their own Responsibility section. 
Include milestones and final deliverable.

In the headings below, replace the silly names and GitHub handles with your actual ones.
David DaCosta @anti-dave
will work on webscraping & efficiently as well as neatly organizing data that is scraped. Dave will be leading evaluator construction. 

Jacob Adamson @jake-the-human
will work on one the Markov Model Object and Unit Tests and I will be working on the evaluator as well.
