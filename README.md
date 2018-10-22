This bibliography attempts to cover all of the literature on _gradual typing_, defined as safe interoperability between typed and untyped portions of a single program.  It begins with the original work on gradual typing, which was independently presented by four sets of authors in between September 2006 and January 2007, and then covers the extensive subsequent literature, both theoretical and practical.

The latter sections cover related work.  First, early work on safe interoperation, such as the work on contracts.  Second, work on type systems (broadly defined) designed to work with existing untyped languages, including recent work done in the context of gradual typing.  Work on type system design that also considers issues of interoperability with untyped programs appears in the main section of the bibliography.

---

For papers written in [Scribble](http://docs.racket-lang.org/scribble/), this
bibliography can be installed as a package and referenced directly from the
paper. Use the following command to install:

`raco pkg install git://github.com/samth/gradual-typing-bib`

Sample use:

````
#lang scribble/base

@(require gradual-typing-bib
          scriblib/autobib)

@(define-cite ~cite citet generate-bibliography)

Original gradual typing papers
@~cite[st-sfp-2006 thf-dls-2006 mf-toplas-2007 gktff-sfp-2006].

@generate-bibliography[]
````

# Gradual Typing

## The original papers

These four papers all independently introduce the fundamental idea of gradual typing: dynamic enforcement of types using contracts.

##### Gradual typing for functional languages.
Jeremy G. Siek and Walid Taha.  
In Seventh Workshop on Scheme and Functional Programming, University of Chicago Technical Report TR-2006-06, pages 81–92, September 2006.  
http://ecee.colorado.edu/~siek/pubs/pubs/2006/siek06_gradual.pdf

##### Interlanguage migration: from scripts to programs.
Sam Tobin-Hochstadt and Matthias Felleisen.  
In OOPSLA ’06: Companion to the 21st annual ACM SIGPLAN Conference on Object Oriented Programming, Systems, Languages, and Applications, pages 964–974. ACM Press, 2006.  
http://www.ccs.neu.edu/racket/pubs/dls06-tf.pdf

##### Operational semantics for multi-language programs
Jacob Matthews and Robert Bruce Findler  
POPL 2007  
See subsequent version in TOPLAS 2009.  
http://www.eecs.northwestern.edu/~robby/pubs/papers/toplas09-mf.pdf

##### Sage: Hybrid Checking for Flexible Specifications
Jessica Gronski, Kenneth Knowles, Aaron Tomb, Stephen N. Freund, and Cormac Flanagan
In Seventh Workshop on Scheme and Functional Programming, University of Chicago Technical Report TR-2006-06, pages 93-104, September 2006.  
http://users.soe.ucsc.edu/~atomb/gronski06sage.pdf


## Subsequent work

Every paper below here cites at least one of the 4 original papers.

##### Sage: Unified Hybrid Checking for First-Class Types, General Refinement Types, and Dynamic. (extended report)
Kenneth Knowles, Aaron Tomb, Jessica Gronski, Stephen N. Freund, Cormac Flanagan.
Technical report, May 2007  
http://sage.soe.ucsc.edu/sage-tr.pdf

##### Space-efficient gradual typing. 
David Herman, Aaron Tomb, and Cormac Flanagan.
In Proceedings of the Eighth Symposium on Trends in Functional Programming, TFP 2007, pages 1–18, 2008.
(See subsequent journal version.)

##### Gradual typing for objects.
Jeremy G. Siek and Walid Taha.  
In ECOOP 2007, volume 4609 of
LCNS, pages 2–27. Springer Verlag, August 2007.  
http://ecee.colorado.edu/~siek/gradual-obj.pdf

##### Advanced Macrology and the Implementation of Typed Scheme.
Ryan Culpepper, Sam Tobin-Hochstadt, and Matthew Flatt.  
In Proceedings of the 2007 Workshop on Scheme and Functional Programming, Universit́e Laval Technical Report DIUL-RT-0701, pages 1–13, 2007  
http://www.ccs.neu.edu/racket/pubs/scheme2007-ctf.pdf

##### Well-typed programs can’t be blamed
Philip Wadler and Robert Bruce Findler  
In Proceedings of the 2007 Workshop on Scheme and Functional Programming, Universit́e Laval Technical Report DIUL-RT-0701, pages 1–13, 2007  
http://www.eecs.northwestern.edu/~robby/pubs/papers/scheme2007-wf.pdf

##### Evolutionary Programming and Gradual Typing in ECMAScript 41
Lars T Hansen
Adobe Systems Technical Report, November 2007  
http://www.ecmascript.org/es4/spec/evolutionary-programming-tutorial.pdf

##### Status report: specifying JavaScript with ML
David Herman, Cormac Flanagan
Proceedings of the 2007 workshop on Workshop on ML, Pages 47-52, 2007.  
http://users.soe.ucsc.edu/~cormac/papers/ml07.pdf

##### The design and implementation of Typed Scheme.
Sam Tobin-Hochstadt and Matthias Felleisen.  
In POPL ’08: Proceedings of the 35th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages, pages 395–406. ACM Press, 2008  
http://www.ccs.neu.edu/racket/pubs/popl08-thf.pdf

##### Safe Cross-Language Inheritance
Kathryn E. Gray  
In proceedings of ECOOP 2008  
http://www.cl.cam.ac.uk/~keg29/inheritance-model/inheritance.pdf

##### Gradual typing with unification-based inference
Jeremy G. Siek, Manish Vachharajani  
DLS '08 Proceedings of the 2008 symposium on Dynamic languages, 2008  
http://ecee.colorado.edu/~siek/dls08igtlc.pdf

##### The Design and Implementation of Typed Scheme
Sam Tobin-Hochstadt and Matthias Felleisen.  
To appear in Higher-Order and Symbolic Computation.

##### Well-typed programs can’t be blamed
Philip Wadler and Robert Bruce Findler  
In ESOP ’09: Proceedings of the Eighteenth European Symposium on Programming, volume 5502 of Lecture Notes in Computer Science, pages 1–16. Springer-Verlag, 2009.

##### Exploring the Design Space of Higher-Order Casts
Jeremy Siek, Ronald Garcia, Walid Taha
In ESOP ’09: Proceedings of the Eighteenth European Symposium on Programming, volume 5502 of Lecture Notes in Computer Science, pages 17-31. Springer-Verlag, 2009.  
http://ecee.colorado.edu/~siek/siek09esop.pdf

##### A Model of Java/Scheme Interoperability
Kathryn E. Gray
Chapter of _Semantics Engineering with PLT Redex_
Edited by Robert Bruce Findler, Matthew Flatt, and Matthias Felleisen. MIT Press 2009

##### Operational semantics for multi-language programs
Jacob Matthews and Robert Bruce Findler  
ACM Transactions on Programming Languages and Systems, 31(3):1–44, 2009.  
http://www.eecs.northwestern.edu/~robby/pubs/papers/toplas09-mf.pdf

#####  Gradual typing for Featherweight Java.
Lintaro Ina and Atsushi Igarashi.
Computer Software, 26(2):18–40, 2009.

In Japanese:
Featherweight Javaのための漸進的型付け
https://www.jstage.jst.go.jp/article/jssst/26/2/26_2_2_18/_pdf

##### Static type inference for Ruby
Michael Furr, Jong-hoon (David) An, Jeffrey S. Foster, and Michael Hicks  
In SAC ’09: Proceedings of the 2009 ACM Symposium on Applied Computing, pages 1859–1866. ACM Press, 2009

##### STOP ’09: Proceedings for the 1st workshop on Script to Program Evolution
Tobias Wrigstad, Nate Nystrom, and Jan Vitek, editors.  
ACM Press 2009

###### From Soft Scheme to Typed Scheme: Experiences from 20 Years of Script Evolution, and Some Ideas on What Works (Talk)
Matthias Felleisen
Invited talk at STOP 2009
http://www.ccs.neu.edu/home/matthias/Presentations/stop.html

##### Towards gradual typing for generics
Lintaro Ina, Atsushi Igarashi  
STOP '09 Proceedings for the 1st workshop on Script to Program Evolution, 2009  
See subsequent version in OOPSLA 2011

##### A theory of typed coercions and its applications
Nikhil Swamy, Michael Hicks, Gavin M. Bierman
Proceedings of the 14th ACM SIGPLAN international conference on Functional programming, Pages 329-340, 2009

##### Threesomes, with and without blame
Jeremy G. Siek, Philip Wadler  
STOP '09 Proceedings for the 1st workshop on Script to Program Evolution, 2009  
See subsequent version in POPL 2010.
http://homepages.inf.ed.ac.uk/wadler/papers/threesomes-stop/threesomes-stop.pdf

##### Software Hardening: A Research Agenda
Wrigstad, Eugster, Field, Nystrom, Vitek  
STOP '09 Proceedings for the 1st workshop on Script to Program Evolution, 2009  

##### Cycles without pollution: a gradual typing poem.
Sam Tobin-Hochstadt and Robert Bruce Findler.  
STOP '09 Proceedings for the 1st workshop on Script to Program Evolution, 2009,   pages 47–57

##### Tests to the left of me, types to the right: how not to get stuck in the middle of a Ruby execution.  
Michael Furr, Jong-hoon (David) An, Jeffrey S. Foster, and Michael Hicks.  
STOP '09 Proceedings for the 1st workshop on Script to Program Evolution, 2009, pages 14–16.

##### Thorn: Robust, Concurrent, Extensible Scripting on the JVM.
Bard Bloom, John Field, Nathaniel Nystrom, Johan Ostlund, Gregor Richards, Rok Strnisa, Jan Vitek, and Tobias Wrigstad.  
In OOPSLA, 2009.

###### Of scripts and programs: tall tales, urban legends, and future prospects
Jan Vitek  
Invited talk at DLS 2009
http://dl.acm.org/citation.cfm?id=1640134.1640136

##### Combining Static and Dynamic Typing in Ruby
Michael Furr
Ph.D. Thesis, University of Maryland, 2009

##### Extending Dylan’s Type System for Better Type Inference and Error Detection
Hannes Mehnert
Diploma Thesis, Technische Universit ̈at Berlin, October 1, 2009

##### Typed Scheme: From Scripts to Programs.
Sam Tobin-Hochstadt  
PhD Dissertation, Northeastern University, January 2010. 

##### Integrating typed and untyped code in a scripting language.
Tobias Wrigstad, Francesco Zappa Nardelli, Sylvain Lebresne, Johan Ostlund, and Jan Vitek.  
In Symposium on Principles of Programming Languages, 2010.

##### Threesomes, with and without blame.
Jeremy G. Siek and Philip Wadler.  
In Symposium on Principles of Programming Languages, 2010.

##### Adding types to untyped languages
Matthias Felleisen
Invited talk at TLDI '10 Proceedings of the 5th ACM SIGPLAN workshop on Types in language design and implementation  
http://dl.acm.org/citation.cfm?id=1708016.1708017

##### Space-efficient gradual typing
David Herman, Aaron Tomb, and Cormac Flanagan
Higher-Order and Symbolic Computation, Vol. 23 No. 2, pages 167-189, 2010

#####  Adding dynamic types to C#.
Gavin Bierman, Erik Meijer, and Mads Torgersen. 
In European Conference on Object-Oriented Programming, ECOOP’10. Springer-Verlag, 2010.

##### Extending Dylan's type system for better type inference and error detection
Hannes Mehnert
Proceedings of the 2010 international conference on Lisp, Pages 1-10, 2010.
https://doi.org/10.1145/1869643.1869645

##### Interoperability in a Scripted World: Putting Inheritance and Prototypes Together
Kathryn E. Gray  
FOOL 2010

##### Blame for All.
Amal Ahmed, Robert Bruce Findler, Jeremy G. Siek, and Philip Wadler.  
In Symposium on Principles of Programming Languages, January 2011.
http://homepages.inf.ed.ac.uk/wadler/papers/blame-for-all/blame-for-all.pdf

##### Proceedings of the Second Workshop on Script to Program Evolution. 
Edited by Robert Bruce Findler
January 2011

##### Languages as Libraries
Sam Tobin-Hochstadt, Vincent St-Amour, Ryan Culpepper, Matthew Flatt, and Matthias Felleisen
PLDI 2011

##### Always-available static and dynamic feedback.
M. Bayne, R. Cook, and M.D. Ernst.  
In International Conference on Software Engineering, 2011.

##### Gradual typestate.
Roger Wolff, Ronald Garcia, Éric Tanter, and Jonathan Aldrich.  
In European Conference on Object-Oriented Programming, ECOOP’11. Springer-Verlag, 2011.
http://pleiad.dcc.uchile.cl/papers/2011/wolffAl-ecoop2011.pdf

##### Gradual typing for generics.
Lintaro Ina and Atsushi Igarashi.  
In Proceedings of the 2011 ACM International conference on Object oriented programming systems languages and applications, OOPSLA ’11, 2011.

##### The impact of optional type information on jit compilation of dynamically typed languages
Mason Chang, Bernd Mathiske, Edwin Smith, Avik Chaudhuri, Andreas Gal, Michael Bebenita, Christian Wimmer, Michael Franz
Proceedings of the 7th symposium on Dynamic languages, Pages 13-24, 2011

##### Gradual Information Flow Typing
Tim Disney and Cormac Flanagan
International Workshop on Scripts to Programs, 2011

##### Language with a Pluggable Type System and Optional Runtime Monitoring of Type Errors
Jukka Lehtosalo and David J. Greaves
International Workshop on Scripts to Programs, 2011

##### Application optimization when using gradual typing
Esteban Allende, Johan Fabry
Proceedings of the 6th Workshop on Implementation, Compilation, Optimization of Object-Oriented Languages, Programs and Systems, Article No. 3, 2011

##### Foundations for Scripting Languages (Report from Dagstuhl Seminar 12011)
Edited by Robert Hirschfeld, Shriram Krishnamurthi, and Jan Vitek
http://www.cs.purdue.edu/homes/jv/pubs/dagstuhl12.pdf

##### The ins and outs of gradual type inference.
Aseem Rastogi, Avik Chaudhuri, and Basil Hosmer.  
In Proceedings of the 39th annual ACM SIGPLAN-SIGACT symposium on Principles of programming languages, POPL ’12, 2012.

##### Complete Monitors for Behavioral Contracts. 
Christos Dimoulas, Sam Tobin-Hochstadt, and Matthias Felleisen.  
European Symposium on Programming (ESOP), March 2012. 

##### Gradual Ownership Types
I. Sergey, D. Clarke
In ESOP 2012, LNCS, vol. 7211, pp. 579–599, 2012.

##### JavaScript as an Embedded DSL
Grzegorz Kossakowski, Nada Amin, Tiark Rompf, and Martin Odersky
In ESOP 2012, LNCS, vol. 7211, pp. 409–434, 2012.

##### Practical Permissions for Race-Free Parallelism
Edwin Westbrook, Jisheng Zhao, Zoran Budimli ́c, and Vivek Sarkar
In ESOP 2012, LNCS, vol. 7211, pp. 614–639, 2012.

##### Modelyze: a Gradually Typed Host Language for Embedding Equation-Based Modeling Languages
David Broman, Jeremy G. Siek
EECS University of California at Berkeley Technical Report No. UCB/EECS-2012-173

##### Type systems directed programming language evolution: overview and research trends
Jaime Niño  
ACM-SE '12 Proceedings of the 50th Annual Southeast Regional Conference

A survey/overview paper

##### Proceedings of the Third Workshop on Script to Program Evolution. 
Edited by Sam Tobin-Hochstadt  
NU CCIS Technical Report NU-CCIS-12-02, June 2012. 

##### Towards Gradual Typing in Jython
Michael M. Vitousek, Shashank Bharadwaj, Jeremy G. Siek  
STOP 2012

##### Interpretations of the gradually-typed lambda calculus.
Jeremy G. Siek and Ronald Garcia.  
In Scheme and Functional Programming Workshop, 2012.

##### Method Lookup Simulation with a Gradual Typing Language 
Esteban Allende, Johan Fabry
In 2012 31st International Conference of the Chilean Computer Science Society.
https://doi.org/10.1109/SCCC.2012.7

##### Gradual Typing for First-class Classes. 
Asumu Takikawa, T. Stephen Strickland, Christos Dimoulas, Sam Tobin-Hochstadt and Matthias Felleisen.  
Object Oriented Programming, Systems, Languages and Applications (OOPSLA), October 2012. 

##### Chaperones and Impersonators: Runtime support for reasonable interposition
T. Stephen Strickland, Sam Tobin-Hochstadt, Robert Bruce Findler and Matthew Flatt.  
Object Oriented Programming, Systems, Languages and Applications (OOPSLA), October 2012. 

##### Optimizing Jython using invokedynamic and Gradual Typing
Shashank Bharadwaj.
Master's Thesis, Department of Electrical, Energy and Computer Engineering, University of Colorado 2012.
https://scholar.colorado.edu/cgi/viewcontent.cgi?article=1056&context=ecen_gradetds

##### Gradual Typing for Mutable Objects
Jeremy G. Siek, Michael M. Vitousek, and Shashank Bharadwaj.  
Unpublished manuscript, 2012.  
http://ecee.colorado.edu/~siek/gtmo.pdf

##### Constraining Delimited Control with Contracts
Asumu Takikawa, T. Stephen Strickland, Sam Tobin-Hochstadt.  
ESOP 2013. 

##### The Blame Theorem for a Linear Lambda Calculus with Type Dynamic
Luminous Fennell,Peter Thiemann
Trends in Functional Programming, LNCS Volume 7829, 2013, pp 37-52 

##### The Ruby Type Checker 
Brianna M. Ren, John Toman, T. Stephen Strickland, Jeffrey S. Foster  
OOPS 2013 

##### A Gradual Polymorphic Type System with Subtyping for Prolog
Spyros Hadjichristodoulou and David Warren
Workshop on Logic-based Programming Environments (WLPE 2012)

##### Gradual typing for Smalltalk
Esteban Allende, Oscar Callaú, Johan Fabry, Éric Tanter, and Marcus Denker.  
Science of Computer Programming (2013).
http://pleiad.dcc.uchile.cl/papers/2013/allendeAl-scp2013.pdf

##### Cast Insertion Strategies for Gradually-Typed Objects
Esteban Allende, Johan Fabry, Éric Tanter
Proceedings of the 9th ACM Dynamic Languages Symposium (DLS 2013), Indianapolis, IN, USA, October 2013, ACM Press
http://pleiad.dcc.uchile.cl/papers/2013/allendeAl-dls2013.pdf

##### Gradual Security Typing with References 
Luminous Fennell, Peter Thiemann.
Computer Security Foundations Symposium (CSF) 2013, IEEE, 224-239.
https://doi.org/10.1109/CSF.2013.22

##### Calculating threesomes, with blame
Ronald Garcia.
ICFP 2013, ACM, New York, NY, USA, 417-428.
https://doi.org/10.1145/2500365.2500603

##### Foundations of Typestate-Oriented Programming. 
Ronald Garcia, Éric Tanter, Roger Wolff, Jonathan Aldrich:
ACM TOPLAS 36(4): 12:1-12:44 (2014)
http://pleiad.dcc.uchile.cl/papers/2014/garciaAl-toplas2014.pdf

##### Deriving interpretations of the gradually-typed lambda calculus
Álvaro García-Pérez, Pablo Nogueira, Ilya Sergey.
PEPM 2014, ACM, 157-168.
https://doi.org/10.1145/2543728.2543742

##### The Problem of Structural Type Tests in a Gradual-Typed Language
John Tang Boyland.
FOOL 2014.
http://www.cs.uwm.edu/faculty/boyland/papers/type-test.pdf

##### Gradual Typing for Annotated Type Systems
Peter Thiemann, Luminous Fennell
ESOP 2014, LNCS vol. 8410, Springer.
https://doi.org/10.1007/978-3-642-54833-8_4

##### A Theory of Gradual Effect Systems
Felipe Bañados Schwerter, Ronald Garcia, Éric Tanter.
International Conference on Functional Programming 2014.
http://pleiad.dcc.uchile.cl/papers/2014/banadosAl-icfp2014.pdf

##### Design and Evaluation of Gradual Typing for Python
Michael M. Vitousek, Andrew Kent, Jeremy G. Siek, Jim Baker
Proceedings of the 10th ACM Dynamic Languages Symposium (DLS 2014), Portland, OR, USA, October 2014, ACM Press

##### Understanding TypeScript
Gavin Bierman, Martín Abadi, Mads Torgersen.
ECOOP 2014, LNCS vol 8586, Springer.
https://doi.org/10.1007/978-3-662-44202-9_11

##### Gradual typing embedded securely in JavaScript
Nikhil Swamy, Cédric Fournet, Aseem Rastogi, Karthikeyan Bhargavan, Juan Chen, Pierre-Yves Strub, Gavin Bierman
POPL 2014, ACM, New York, NY, USA 425-437.
https://doi.org/10.1145/2535838.2535889

##### JavaScript with Blame
Jakub Zalewski
Master's Thesis, School of Informatics, University of Edinburgh.
http://project-archive.inf.ed.ac.uk/msc/20141715/msc_proj.pdf

##### Confined Gradual Typing
Esteban Allende, Johan Fabry, Ronald Garcia, Éric Tanter.
OOPSLA 2014
http://pleiad.dcc.uchile.cl/papers/2014/allendeAl-oopsla2014.pdf

##### Gradual evolution
Neil Savage
Communications of the ACM, vol. 57 issue 10, 2014.
https://doi.org/10.1145/2659764

##### Safe & Efficient Gradual Typing for TypeScript
Aseem Rastogi, Nikhil Swamy, Cédric Fournet, Gavin Bierman, Panagiotis Vekris.
In Proceedings of the 42nd Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL '15). ACM, New York, NY, USA, 167-180.
https://doi.org/10.1145/2775051.2676971

##### Principal Type Schemes for Gradual Programs
Ronald Garcia, Matteo Cimini.
POPL 2015, ACM, New York, NY, USA, 303-315.
https://doi.org/10.1145/2775051.2676992

##### Monotonic References for Efficient Gradual Typing
Jeremy Siek, Michael M. Vitousek, Matteo Cimmini, Sam Tobin-Hochstadt, Ronald Garcia
ESOP 2015, LNCS Volume 9032, pp 432-456

##### EMPIRICALLY-DRIVEN DESIGN AND IMPLEMENTATION OF GRADUALTALK
OSCAR EDWIN ALVAREZ CALLAU ́
Ph.D. Thesis, University of Chile, 2015.
http://repositorio.uchile.cl/bitstream/handle/2250/132889/Empirically-driven-design-and-implementation-of-Gradualtalk.pdf?sequence=1

##### Towards Practical Gradual Typing
Asumu Takikawa, Daniel Feltey, Earl Dean, Robert Bruce Findler, Matthew Flatt, Sam Tobin-Hochstadt, Matthias Felleisen
ECOOP 2015, LIPIcs Volume 37, pp 4-27

##### Concrete Types for TypeScript
Gregor Richards, Francesco Zappa Nardelli, Jan Vitek
ECOOP 2015, LIPIcs Volume 37, pp 76-100

##### Refined Criteria for Gradual Typing
Jeremy G. Siek, Michael M. Vitousek, Matteo Cimini, John Tang Boyland
SNAPL 2015, LIPIcs Volume 32, pp 274-293
http://drops.dagstuhl.de/opus/volltexte/2015/5031/

##### Customizable Gradual Polymorphic Effects for Scala
Matías Toro, Éric Tanter
In Proceedings of the 2015 ACM SIGPLAN International Conference on Object-Oriented Programming, Systems, Languages, and Applications (pp. 935-953)
http://pleiad.dcc.uchile.cl/papers/2015/toroTanter-oopsla2015.pdf

##### Gradual Certified Programming in Coq
Éric Tanter, Nicolas Tabareau
Proceedings of the 11th ACM Dynamic Languages Symposium (DLS 2015), pp.26-40, Pittsburgh, PA, USA, October 2015, ACM Press
http://pleiad.dcc.uchile.cl/papers/2015/tanterTabareau-dls2015.pdf

##### A Complement to Blame
Philip Wadler
SNAPL 2015, LIPIcs vol. 32, 309-320.
http://drops.dagstuhl.de/opus/volltexte/2015/5033/

##### A formalization of typed lua
André Murbach Maidl, Fabio Mascarenhas, Roberto Ierusalimschy
DLS 2015, ACM, Pages 13-25.
https://doi.org/10.1145/2816707.2816709
          
##### Shifting the Blame
T. Sekiyama, S. Ueda, A. Igarashi.
APLAS 2015. LNCS, vol 9458. Springer.
https://doi.org/10.1007/978-3-319-26529-2_11

##### LJGS: Gradual Security Types for Object-Oriented Languages
Luminous Fennell and Peter Thiemann.
ECOOP 2016, Leibniz International Proceedings in Informatics (LIPIcs), Pages 9:1--9:26.
http://drops.dagstuhl.de/opus/volltexte/2016/6103/

##### Gradual type-and-effect systems
Felipe Bañados Schwerter, Ronald Garcia, Éric Tanter.
Journal of Functional Programming, vol. 26, 2016.
https://doi.org/10.1017/S0956796816000162

##### Abstracting Gradual Typing
Ronald Garcia, Alison M. Clark, Éric Tanter
POPL 2016, ACM, New York, NY, USA, 429-442.
http://dl.acm.org/citation.cfm?id=2837670
http://pleiad.dcc.uchile.cl/papers/2016/garciaAl-popl2016.pdf

##### The Gradualizer: a methodology and algorithm for generating gradual type systems
Matteo Cimini, Jeremy Siek
POPL 2016, ACM, New York, NY, USA, 443-455.
http://cimini.info/publications/Gradualizer_Draft.pdf

##### Is Sound Gradual Typing Dead?
Asumu Takikawa, Daniel Feltey, Ben Greenman, Max New, Jan Vitek, Matthias Felleisen
POPL 2016, ACM, New York, NY, USA, 456-468.
http://www.ccs.neu.edu/racket/pubs/popl16-tfgnvf.pdf

##### Blame Tracking at Higher Fidelity
Jakub Zalewski
Master of Science by Research, School of Informatics, University of Edinburgh.
https://project-archive.inf.ed.ac.uk/msc/20162082/msc_proj.pdf

##### Gradual Pluggable Typing in Java
Daniel Brotherston.
Master's Thesis, University of Waterloo.
https://uwspace.uwaterloo.ca/handle/10012/10409

##### Type Soundness in the Dart Programming Language
Fabio Strocco.
Ph.D. Thesis, Aarhus University, 2016.
https://pure.au.dk/portal/files/108950717/Thesis_Fabio_Strocco.pdf

##### Side effects take the blame
Felipe Bañados Schwerter
SLE 2016, ACM, Pages 195-206.
https://doi.org/10.1145/2997364.2997381

##### The recursive union of some gradual types
Jeremy G. Siek, Sam Tobin-Hochstadt.
A List of Successes That Can Change the World (Wadler Fest), LNCS volume 9600, Springer, 2016, 388-410.
https://doi.org/10.1007/978-3-319-30936-1_21

##### Blame and coercion: Together again for the first time
Jeremy G. Siek, Peter Thiemann, Philip Wadler.
PLDI 2016, ACM, New York, NY, USA, 425-435.
https://doi.org/10.1145/2737924.2737968

##### ALGT A Framework to Describe and Gradualize Programming Languages
Pieter Vander Vennet.
Master's Dissertation, Ghent University.
http://lib.ugent.be/fulltxt/RUG01/002/376/271/RUG01-002376271_2017_0001_AC.pdf

##### Theorems for free for free: Parametricity, with and without types
Amal Ahmed, Dustin Jamner, Jeremy G. Siek, Philip Wadler
ICFP 2017, ACM, New York, NY, USA
https://doi.org/10.1145/3110283

##### Gradual session types
Atsushi Igarashi, Peter Thiemann, Vasco T. Vasconcelos, Philip Wadler
ICFP 2017, ACM, New York, NY, USA
https://doi.org/10.1145/3110282

##### On polymorphic gradual typing
Yuu Igarashi, Taro Sekiyama, Atsushi Igarashi
ICFP 2017, ACM, New York, NY, USA
https://doi.org/10.1145/3110284

##### Gradual typing with union and intersection types
Giuseppe Castagna, Victor Lanvin
ICFP 2017, ACM, New York, NY, USA
https://doi.org/10.1145/3110285

##### Sound gradual typing: Only mostly dead
Spenser A. Bauman, Sam Tobin-Hochstadt, Jeremy G. Siek, Carl Friedrich Bolz-Tereick
OOPSLA 2017, ACM, New York, NY, USA
https://doi.org/10.1145/3133878

##### The VM already knew that: leveraging compile-time knowledge to optimize gradual typing
Gregor Richards, Ellen Arteca, Alexi Turcotte
OOPSLA 2017, ACM, New York, NY, USA
https://doi.org/10.1145/3133879

##### Sound gradual typing is nominally alive and well
Fabian Muehlboeck, Ross Tate
OOPSLA 2017, ACM, New York, NY, USA
https://doi.org/10.1145/3133880

#####  A Gradual Interpretation of Union Types
M. Toro, É Tanter.
SAS 2017, LNCS vol. 10422, Springer.
https://doi.org/10.1007/978-3-319-66706-5_19

##### Mixed Messages: Measuring Conformance and Non-Interference in TypeScript
Jack Williams, J. Garrett Morris, Philip Wadler, Jakub Zalewski
ECOOP 2017, Leibniz International Proceedings in Informatics (LIPIcs), pages 28:1--28:29.
http://drops.dagstuhl.de/opus/volltexte/2017/7264/

##### Migratory Typing: Ten Years Later
Sam Tobin-Hochstadt, Matthias Felleisen, Robert Findler, Matthew Flatt, Ben Greenman, Andrew M. Kent, Vincent St-Amour, T. Stephen Strickland, Asumu Takikawa
SNAPL 2017, Leibniz International Proceedings in Informatics (LIPIcs), pages 17:1--17:17.
http://drops.dagstuhl.de/opus/volltexte/2017/7120/

##### Big types in little runtime
Michael M. Vitousek, Cameron Swords, and Jeremy G. Siek.
POPL 2017, ACM, New York, NY, USA, 762-774.
https://doi.org/10.1145/3093333.3009849

##### Gradual refinement types
Nico Lehmann, Éric Tanter.
POPL 2017, ACM, New York, NY, USA, 775-788.
https://doi.org/10.1145/3093333.3009856

##### Automatically generating the dynamic semantics of gradually typed languages
Matteo Cimini, Jeremy G. Siek.
POPL 2017, ACM, New York, NY, USA, 789-803.
https://doi.org/10.1145/3093333.3009863

##### Sums of uncertainty: refinements go gradual
Khurram A. Jafery, Joshua Dunfield.
POPL 2017, ACM, New York, NY, USA, 804-817.
https://doi.org/10.1145/3093333.3009865

##### Hazelnut: a bidirectionally typed structure editor calculus
Cyrus Omar, Ian Voysey, Michael Hilton, Jonathan Aldrich, Matthew A. Hammer.
POPL 2017, ACM, New York, NY, USA, 86-99.
https://doi.org/10.1145/3009837.3009900

##### Gradually typed symbolic expressions
David Broman, Jeremy G. Siek
PEPM 2018, ACM, New York, NY, USA, 15–29.
https://doi.org/10.1145/3162068
https://people.kth.se/~dbro/papers/broman-siek-2018-gradually-typed-symbols.pdf

##### Consistent Subtyping for All
Ningning Xie, Xuan Bi, Bruno C. d. S. Oliveira.
ESOP 2018, LNCS vol 10801, Springer.
https://doi.org/10.1007/978-3-319-89884-1_1

##### On the Cost of Type-Tag Soundness
Ben Greenman, Zeina Migeed.
PEPM 2018, ACM, New York, NY, USA.
https://doi.org/10.1145/3162066

##### Graduality from embedding-projection pairs
Max S. New, Amal Ahmed.
ICFP 2018, ACM, New York, NY, USA.
https://doi.org/10.1145/3236768

##### A spectrum of type soundness and performance
Ben Greenman, Matthias Felleisen.
ICFP 2018, ACM, New York, NY, USA.
https://doi.org/10.1145/3236766

##### Casts and costs: harmonizing safety and performance in gradual typing
John Peter Campora, Sheng Chen, Eric Walkingshaw.
ICFP 2018, ACM, New York, NY, USA.
https://doi.org/10.1145/3236793

##### Migrating gradual types
John Peter Campora, Sheng Chen, Martin Erwig, Eric Walkingshaw.
POPL 2018, ACM, New York, NY, USA.
https://doi.org/10.1145/3158103

##### Efficient Gradual Typing
Andre Kuhlenschmidt and Deyaaeldeen Almahallawi and Jeremy G. Siek.
arXiv:1802.06375, February 2018.
https://arxiv.org/abs/1802.06375

##### KafKa: Gradual Typing for Objects
Benjamin Chung, Paley Li, Francesco Zappa Nardelli, Jan Vitek.
ECOOP 2018, Leibniz International Proceedings in Informatics (LIPIcs), Pages 12:1--12:24.
http://drops.dagstuhl.de/opus/volltexte/2018/9217/

##### Shallow Types for Insightful Programs: Grace is Optional, Performance is Not
Richard Roberts, Stefan Marr, Michael Homer, James Noble.
arXiv:1807.00661
https://arxiv.org/abs/1807.00661

##### Gradual Liquid Type Inference
Niki Vazou, Éric Tanter, David Van Horn.
OOPSLA 2018, ACM.
https://arxiv.org/abs/1807.02132

##### Type-Driven Gradual Security with References: Complete Definitions and Proofs
Matías Toro, Ronald Garcia, Éric Tanter.
Technical Report TR/DCC-2018-4, Computer Science Department, University of Chile.
https://www.dcc.uchile.cl/TR/2018/TR_DCC-20180801-004.pdf

##### Collapsible Contracts: Fixing a Pathology of Gradual Typing
Daniel Feltey, Ben Greenman, Christophe Scholliers, Robert Bruce Findler, Vincent St-Amour.
OOPSLA 2018, ACM.
https://www.eecs.northwestern.edu/~robby/pubs/papers/oopsla2018-fgsfs.pdf
https://doi.org/10.1145/3276503

##### Gradual Parametricity, Revisited
Matías Toro, Elizabeth Labrada, Éric Tanter.
arXiv:1807.04596
https://arxiv.org/abs/1807.04596

##### The Behavior of Gradual Types: A User Study
Preston Tunnell Wilson, Justin Pombrio, Ben Greenman, Shriram Krishnamurthi.
DLS 2018, ACM.
http://www.ccis.northeastern.edu/~types/publications/apples-to-apples/tgpk-dls-2018.pdf

# Early Work on Interoperation

These papers all consider the challenges of interoperability between typed and untyped languages, which is at the core of the gradual typing project.  Papers about type checking untyped languages that do not consider interoperation appear in the next section.

##### Quasi-static Typing
Satish Thatte  
In Proceedings of the 17th ACM SIGPLAN-SIGACT symposium on Principles of programming languages (POPL '90). ACM, New York, NY, USA, 367-381. 

##### Pluggable type systems.
Gilad Bracha.  
In OOPSLA Workshop on the Revival of Dynamic Languages, 2004.

##### Contracts for higher-order functions
Robert Bruce Findler and Matthias Felleisen  
In ICFP ’02: Proceedings of the Seventh ACM SIGPLAN International Conference on Functional Programming, pages 48–59. ACM Press, 2002

##### BabyJ: From Object Based to Class Based Programming via Types
Christopher Anderson, Sophia Drossopoulou  
Proceedings of Workshop on Object Oriented Developments 2003, Electronic Notes in Theoretical Computer Science, Volume 82, Issue 8, October 2003, Pages 53–81

##### Compiling Java to PLT Scheme
Kathryn E. Gray and Matthew Flatt.  
In Scheme Workshop 2004

##### Fine-grained interoperability through mirrors and contracts.
Kathryn E. Gray, Robert Bruce Findler, and Matthew Flatt.  
In OOPSLA ’05: Proceedings of the 20th annual ACM SIGPLAN Conference on Object Oriented Programming, Systems, Languages, and Applications, pages 231–245. ACM Press, 2005

##### Hybrid type checking. 
Cormac Flanagan.  
In Conference Record of POPL ’06: The 33th ACM SIGPLAN-SIGACT Symposium on Principles of Programming
Languages, pages 245–256. ACM Press, 2006.

##### Towards Customizable Pedagogic Programming Languages
Kathryn E. Gray  
PhD Dissertation, University of Utah, School of Computing, August 2006.

##### Introducing safe unknown types in Java-like languages
Giovanni Lagorio and Elena Zucca
ACM Symp. on Applied Computing (SAC 2006), Special Track on Object-Oriented Programming Languages and Systems, 2006

##### Just: safe unknown types in Java-like languages
Giovanni Lagorio and Elena Zucca
Journal of Object Technology, Vol. 6, No. 2, Special Issue OOPS Track at SAC 2006, February 2007

# Related

## Contracts

##### Unifying Hybrid Types and Contracts.
Jessica Gronski, Cormac Flanagan.
Trends in Functional Programming, 2007.

##### Polymorphic Contracts
João Filipe Belo, Michael Greenberg, Atsushi Igarashi, Benjamin C. Pierce 
Programming Languages and Systems, LNCS Volume 6602, 2011, pp 18-37
http://www.seas.upenn.edu/~mgree/papers/esop2011sub_fh.pdf

##### Manifest Contracts
Michael Greenberg.
PhD Dissertation, University of Pennsylvania, January 2013.
https://repository.upenn.edu/cgi/viewcontent.cgi?article=1625&context=edissertations

##### Sound and Complete Models of Contracts
Matthias Blume and David Mcallester.
Journal of Functional Programming, vol 16, 2006.
https://doi.org/10.1017/S0956796806005971

##### On Contract Satisfaction in a Higher-order World
Christos Dimoulas and Matthias Felleisen.
ACM Transactions on Programming Languages and Systems (TOPLAS), 33(5):1–29, 2011.
https://doi.org/10.1145/2039346.2039348

## Typing Untyped Languages

These papers address the challenges of type checking existing untyped programs and programming languages, but do not address the core gradual type checking concern of interoperability.

##### User-defined data types as an aid to verifying LISP programs.
Robert Cartwright.  
In International Conference on Automata, Languages and Programming, pages 228–256, 1976.

##### Inferring types in Smalltalk
Norihisa Suzuki  
POPL '81 Proceedings of the 8th ACM SIGPLAN-SIGACT symposium on Principles of programming languages 

##### Creating efficient systems for object-oriented languages
Norihisa Suzuki and Minoru Terada  
POPL '84 Proceedings of the 11th ACM SIGACT-SIGPLAN symposium on Principles of programming languages 

##### TS: An Optimizing Compiler for Smalltalk
Ralph E. Johnson and Justin O. Graver and Lawrence W. Zurawski
OOPSLA '88: Proceedings of the 3rd annual ACM SIGPLAN Conference on Object Oriented Programming, Systems, Languages, and Applications, pages 18-26.
http://dl.acm.org/citation.cfm?id=62086

#####  Static type inference in a dynamically typed language
Alexander Aiken and Brian R. Murphy  
In POPL ’91: Proceedings of the 18th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages, pages 279–290. ACM Press, 1991.


##### Soft typing. 
Robert Cartwright and Mike Fagan.  
In PLDI ’91: Proceedings of the ACM SIGPLAN 1991 Conference on Programming Language Design and Implementation, pages 278–292. ACM Press, 1991.

##### Strongtalk: typechecking Smalltalk in a production environment.
Gilad Bracha and David Griswold.  
In OOPSLA ’93: Proceedings of the 8th annual ACM SIGPLAN Conference on Object Oriented Programming, Systems, Languages, and Applications, pages 215–230. ACM Press, 1993.

##### Strong Static Type Checking for Functional Common Lisp
Robert Akers  
University of Texas Dissertation, also Computational Logic Inc Technical Report 96  
ftp://ftp.cs.utexas.edu/pub/boyer/diss/akers.pdf

##### Soft typing with conditional types. 
Alexander Aiken, Edward L. Wimmers, and T. K. Lakshman   
In POPL ’94: Proceedings of the 21st ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages, pages 163–173. ACM Press, 1994

##### Dynamic typing: Syntax and proof theory.
Fritz Henglein.  
Science of Computer Programming, 22(3):197–230, 1994.

##### Safe polymorphic type inference for a dynamically typed language: translating Scheme to ML. 
Fritz Henglein and Jakob Rehof.    
In FPCA ’95: Proceedings of the Seventh International Conference on Functional Programming Languages and Computer Architecture, pages 192–203. ACM Press, 1995.

##### Infer: A statically-typed dialect of Scheme.
Christopher T. Haynes  
Technical Report 367, Indiana University, 1995.

##### Catching bugs in the web of program invariants
Cormac Flanagan, Matthew Flatt, Shriram Krishnamurthi, Stephanie Weirich, and Matthias Felleisen  
In Proceedings of the ACM SIGPLAN 1996 conference on Programming language design and implementation (PLDI '96)  
http://www.ccs.neu.edu/racket/pubs/#pldi96-ffkwf


##### The Strongtalk Type System for Smalltalk.
Gilad Bracha  
OOPSLA96 Workshop on Extending the Smalltalk Language, 1996  
http://www.bracha.org/nwst.html

##### Program Verification through Soft Typing 
Robert Cartwright and Matthias Felleisen  
ACM Computing Surveys 28, 2 (June 1996)  
http://www.ccs.neu.edu/racket/pubs/#cs96-cf

##### Componential set-based analysis
Cormac Flanagan and Matthias Felleisen  
In Proceedings of the ACM SIGPLAN 1997 conference on Programming language design and implementation (PLDI '97)  
http://www.ccs.neu.edu/racket/pubs/#pldi97-ff  


##### A practical subtyping system for Erlang. 
Simon Marlow and Philip Wadler.  
In ICFP ’97: Proceedings of the Second ACM SIGPLAN International Conference
on Functional Programming, pages 136–149. ACM Press, 1997.
http://homepages.inf.ed.ac.uk/wadler/topics/erlang.html

##### A practical soft type system for Scheme.
Andrew K. Wright and Robert Cartwright. 
ACM Transactions on Programming Languages and Systems, 19(1):87–152, 1997

##### A New Way of Debugging LISP Programs 
Cormac Flanagan and Matthias Felleisen  
In Lisp in the Mainstream: The 40th Annniversary Conference of Lisp Users.  
Berkeley, November 1998.  
http://www.ccs.neu.edu/racket/pubs/#lugm98-ff

##### Componential set-based analysis
Cormac Flanagan and Matthias Felleisen  
ACM Trans. Program. Lang. Syst. 21, 2 (March 1999), 370-416  
http://www.ccs.neu.edu/racket/pubs/#toplas99-ff  

##### Revised NISP manual. 
Drew McDermott.  
Technical Report YALE/DCS/RR-642, Yale University, Department of Computer Science, 2004

##### Starkiller: A static type inferencer and compiler for Python.
Michael Salib.  
Master’s thesis, Massachusetts Institute of Technology, Cambridge, Massachusetts, 2004.

#####  A Type Notation for Scheme.
Gary T. Leavens, Curtis Clifton, and Brian Dorn.  
Technical Report 05-18a, Iowa State University, 2005.

#####  Practical Type Inference Based on Success Typings
Tobias Lindahl and Konstantinos Sagonas
International Symposium on Principles and Practice of Declarative Programming (PPDP), 2006.
http://www.it.uu.se/research/group/hipe/papers/succ_types.pdf


## Type Systems for Gradual-typing

This work presents type systems for untyped languages, influenced by gradual typing work on interoperation, but doesn't explicitly deal with typed/untyped interoperation.

##### Practical Variable-Arity Polymorphism
T. Stephen Strickland, Sam Tobin-Hochstadt, and Matthias Felleisen.  
European Symposium on Programming (ESOP), March 2009. 

##### Static Type Inference for Ruby
Michael Furr, Jong-hoon (David) An, Jeffrey S. Foster, and Michael Hicks  
In Object-Oriented Program Languages and Systems (OOPS) Track at ACM Symposium on Applied Computing (SAC), pages 1859–1866, Honolulu, Hawaii, March 2009.

##### Tests to the Left of Me, Types to the Right: How Not to Get Stuck in the Middle of a Ruby Execution (A Demo of Diamondback Ruby)
Michael Furr, Jong-hoon (David) An, Jeffrey S. Foster, and Michael Hicks  
In 1st International Workshop on Script to Program Evolution (STOP), Genova, Italy, July 2009.

##### Profile-Guided Static Typing for Dynamic Scripting Languages
Michael Furr, Jong-hoon (David) An, and Jeffrey S. Foster  
In ACM SIGPLAN International Conference on Object-Oriented Programming, Systems, Languages and Applications (OOPSLA), pages 283–300, Orlando, Floria, October 2009. 

##### Work In Progress: an Empirical Study of Static Typing in Ruby
Mark T. Daly, Vibha Sazawal, and Jeffrey S. Foster  
In Workshop on Evaluation and Usability of Programming Languages and Tools (PLATEAU), Orlando, Florida, October 2009.

##### Static Typing for Ruby on Rails
Jong-hoon (David) An, Avik Chaudhuri, and Jeffrey S. Foster  
In IEEE/ACM International Conference on Automated Software Engineering (ASE), pages 590–594, Auckland, New Zealand, November 2009. Short paper.

##### Logical Types for Untyped Languages
Sam Tobin-Hochstadt and Matthias Felleisen  
International Conference on Functional Programming (ICFP), September 2010. 

##### Dynamic Inference of Static Types for Ruby
Jong-hoon (David) An, Avik Chaudhuri, Jeffrey S. Foster, and Michael Hicks  
In ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL), pages 459–472, Austin, TX, USA, January 2011.

##### Position Paper: Dynamically Inferred Types for Dynamic Languages
Jong-hoon (David) An, Avik Chaudhuri, Jeffrey S. Foster, and Michael Hicks  
In 2nd International Workshop on Script to Program Evolution (STOP), Austin, TX, USA, January 2011.

##### Typing the Numeric Tower 
Vincent St-Amour, Sam Tobin-Hochstadt, Matthew Flatt and Matthias Felleisen  
Symposium on Practical Aspects of Declarative Languages (PADL), January 2012. 

##### Nested Refinements: A Logic for Duck Typing
Ravi Chugh, Patrick M. Rondon, and Ranjit Jhala  
In Proceedings of the ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL), pages 231-244, Philadelphia, PA, January 2012. 

##### Status Report: Dependent Types for JavaScript
Ravi Chugh, David Herman, Ranjit Jhala
STOP 2012
See subsequent OOPSLA paper.

##### Minigrace: A progress report
Michael Homer and James Noble  
STOP 2012

#####  Dependent Types for JavaScript
Ravi Chugh, David Herman, and Ranjit Jhala  
In Proceedings of the ACM SIGPLAN Conference on Object-Oriented Programming, Systems, Languages, and Applications (OOPSLA), pages 587-606, Tucson, AZ, October 2012.

##### Progressive Types
Joe Gibbs Politz, Hannah Quay-de la Vallee, Shriram Krishnamurthi  
In Proceedings of the ACM international symposium on New ideas, new paradigms, and reflections on programming and software (Onward! '12)  
http://cs.brown.edu/~sk/Publications/Papers/Published/pqk-progressive-types/

##### A Practical Optional Type System for Clojure
Ambrose Bonnaire-Sergeant  
Honours thesis, University of Western Australia, 2012
http://cloud.github.com/downloads/frenchy64/papers/ambrose-honours.pdf

##### Typed Lua: An Optional Type System for Lua
André Murbach Maidl, Fabio Mascarenhas, Roberto Ierusalimschy.
Dyla 2014.
http://www.lifl.fr/dyla14/papers/dyla14-4-typed-lua-an-optional-type-system.pdf

##### Practical Optional Types for Clojure
Ambrose Bonnaire-Sergeant, Rowan Davies, and Sam Tobin-Hochstadt.  
In ESOP 2016: Proceedings of the 25th European Symposium on Programming Languages and Systems, volume 9632 of Lecture Notes in Computer Science, pages 68-94. Springer-Verlag, 2016.  
http://frenchy64.github.io/papers/esop16-short.pdf

##### Occurrence Typing Modulo Theories
Andrew M. Kent, David Kempe, and Sam Tobin-Hochstadt.
In Proceedings of the ACM SIGPLAN 2016 conference on Programming Language Design and Implementation (PLDI '16)  
