#lang at-exp racket

;; This module defines a gradual typing bibliography in
;; autobib format, suitable for use in papers written in Scribble

;; FIXME: this doesn't have all the papers from the README yet

(require racket/format
         scriblib/autobib)

(provide (all-defined-out))

;; ----------------------------------------

;; In a submodule so that it doesn't get exported automatically by
;; the outer module
(module util racket/base
  (require racket/format)
  (provide (all-defined-out))

  (define short? #f)
  (define-syntax define/short
    (syntax-rules ()
      [(_ i e e*) (define i (if short? e e*))]
      [(_ i e) (define i e)]))

  (define IEEE "IEEE ")
  (define ACM "ACM ")
  (define International "Intl. ")
  (define Conference "Conf. ")
  (define Workshop "Wksp. ")
  (define Journal "J. ")
  (define Symposium "Sym. ")
  (define Transactions "Trans. ")

  (define/short aplas "APLAS" (string-append "Asian " Symposium "Programming Languages and Systems"))
  (define/short fpca "FPCA" (string-append ACM International Conference "Functional Programming Languages and Computer Architecture"))
  (define/short icfp "ICFP" (string-append ACM International Conference "Functional Programming"))
  (define/short pldi "PLDI" (string-append ACM Conference "Programming Language Design and Implementation"))
  (define/short popl "POPL" (string-append ACM Symposium "Principles of Programming Languages"))
  (define/short lncs "LNCS" "Lecture Notes in Computer Science")
  (define/short sigplan-notices "SIGPLAN Notices" (string-append ACM "SIGPLAN Notices"))
  (define/short scheme-workshop "SFP" (string-append Workshop "Scheme and Functional Programming"))
  (define/short ml-workshop "ML" (string-append Workshop "on ML"))
  (define/short jfp "JFP" (string-append Journal "Functional Programming"))
  (define/short hosc "HOSC" "Higher-Order and Symbolic Programming")
  (define/short lfp "LFP" "LISP and Functional Programming")
  (define/short lsc "LSC" "LISP and Symbolic Computation")
  (define/short ifl "IFL" (string-append International Symposium "Functional and Logic Programming"))
  (define/short tfp "TFP" (string-append Symposium "Trends in Functional Programming"))
  (define/short ecoop "ECOOP" (string-append "European " Conference "Object-Oriented Programming"))
  (define/short oopsla "OOPSLA" (string-append ACM Conference "Object-Oriented Programming, Systems, Languages and Applications"))
  (define/short ieee-software (string-append IEEE "Software"))
  (define/short toplas "TOPLAS" (string-append Transactions "Programming Languages and Systems"))
  (define/short dls "DLS" "Dynamic Languages Symposium")
  (define/short flops "FLOPS" (string-append Symposium "Functional and Logic Programming"))
  (define/short esop "ESOP" (string-append "European " Symposium "on Programming"))
  (define/short iclp "ICLP" (string-append  International Conference "on Logic Programming"))
  (define/short fse "FSE" (string-append International Symposium "on the Foundations of Software Engineering"))
  (define/short aosd "AOSD" (string-append International Conference "on Aspect-Oriented Software Development"))
  (define/short foal "FOAL" "Foundations of Aspect-Oriented Languages")
  (define/short tlca "TLCA" (string-append International Conference "Typed Lambda Calculi and Applications"))
  (define/short i&c "Info. & Comp." "Information and Computation")
  (define/short padl "PADL" (string-append Symposium "on Practical Aspects of Declarative Languages"))
  (define/short ppdp "PPDP" (string-append International Symposium "on Principles and Practice of Declarative Programming"))
  (define/short fool "FOOL" (~a International Workshop "on Foundations of Object-Oriented Languages"))
  (define/short icse "ICSE" (~a International Conference "on Software Engineering"))
  (define/short icalp "ICALP" (string-append International "Colloquium on Automata, Languages, and Programming"))
  (define/short sac "SAC" (string-append Symposium "on Applied Computing"))
  (define/short dyla "DYLA" (string-append Workshop "on Dynamic Languages and Applications")))

(require 'util)

;; ----------------------------------------
;; The original papers

(define st-sfp-2006
  (make-bib
   #:title "Gradual Typing for Functional Languages"
   #:author (authors "Jeremy G. Siek" "Walid Taha")
   #:location (proceedings-location scheme-workshop)
   #:date 2006))

(define thf-dls-2006
  (make-bib
   #:title "Interlanguage Migration: from Scripts to Programs"
   #:author (authors "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location dls #:pages '(964 974))
   #:date 2006))

(define mf-toplas-2007
  (make-bib
   #:title "Operational Semantics for Multi-Language Programs"
   #:author (authors "Jacob Matthews" "Robert Bruce Findler")
   #:date 2009
   #:location (journal-location toplas
                                #:volume 31
                                #:number 3
                                #:pages '("12:1" "12:44"))))

(define gktff-sfp-2006
  (make-bib
   #:title "Sage: Hybrid Checking for Flexible Specifications"
   #:author (authors "Jessica Gronski" "Kenneth Knowles" "Aaron Tomb"
                     "Stephen N. Freund" "Cormac Flanagan")
   #:date 2006
   #:location (proceedings-location scheme-workshop
                                    #:pages '(93 104))))

;; ----------------------------------------
;; Subsequent work

(define ktgff-tech-2007
  (make-bib
   #:title @~a{Sage: Unified Hybrid Checking for First-Class Types,
               General Refinement Types, and Dynamic (Extended Report)}
   #:author (authors "Kenneth Knowles" "Aaron Tomb" "Jessica Gronski"
                     "Stephen N. Freund" "Cormac Flanagan")
   #:date 2007))

(define htf-tfp-2007
  (make-bib
   #:title "Space Efficient Gradual Typing"
   #:author (authors "David Herman" "Aaron Tomb" "Cormac Flanagan")
   #:location (proceedings-location tfp)
   #:date "2007"))

(define st-ecoop-2007
  (make-bib
   #:title "Gradual Typing for Objects"
   #:author (authors "Jeremy G. Siek" "Walid Taha")
   #:location (proceedings-location ecoop #:pages '(2 27))
   #:date 2007))

(define cthf-sfp-2007
  (make-bib
   #:title "Advanced Macrology and the Implementation of Typed Scheme"
   #:author (authors "Ryan Culpepper" "Sam Tobin-Hochstadt" "Matthew Flatt")
   #:location (proceedings-location scheme-workshop #:pages '(1 13))
   #:date 2007))

(define wf-sfp-2007
  (make-bib
   #:title "Well-typed Programs Can't be Blamed"
   #:author (authors "Philip Wadler" "Robert Bruce Findler")
   #:location (proceedings-location scheme-workshop)
   #:date 2007))

(define hansen-tech-2007
  (make-bib
   #:title "Evolutionary Programming and Gradual Typing in ECMAScript 4"
   #:author "Lars T. Hansen"
   #:date 2007))

(define hf-ml-2007
  (make-bib
   #:title "Status report: specifying JavaScript with ML"
   #:author (authors "David Herman" "Cormac Flanagan")
   #:location (proceedings-location ml-workshop)
   #:date 2007))

(define thf-popl-2008
  (make-bib
   #:title "The Design and Implementation of Typed Scheme"
   #:author (authors "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location popl
                                    #:pages '(395 406))
   #:date 2008))

(define gray-ecoop-2008
  (make-bib
   #:title "Safe Cross-Language Inheritance"
   #:author "Kathryn E. Gray"
   #:location (proceedings-location ecoop #:pages '(52 75))
   #:date 2008))

(define sv-dls-2008
  (make-bib
   #:title "Gradual Typing with Unification-based Inference"
   #:author (authors "Jeremy G. Siek" "Manish Vachharajani")
   #:location (proceedings-location dls)
   #:date 2008))

(define wf-esop-2009
  (make-bib
   #:title "Well-typed Programs Can't be Blamed"
   #:author (authors "Philip Wadler" "Robert Bruce Findler")
   #:location (proceedings-location esop #:pages '(1 15))
   #:date 2009))

(define sgt-esop-2009
  (make-bib
   #:title "Exploring the Design Space of Higher-Order Casts"
   #:author (authors "Jeremy G. Siek" "Ronald Garcia" "Walid Taha")
   #:location (proceedings-location esop #:pages '(17 31))
   #:date 2009))

(define gray-chapter-2009
  (make-bib
   #:title "A Model of Java/Scheme Interoperability"
   #:author "Kathryn E. Gray"
   #:date 2009))

(define mf-toplas-2009
  (make-bib
   #:title "Operational Semantics for Multi-language Programs"
   #:author (authors "Jacob Matthews" "Robert Bruce Findler")
   #:location (journal-location toplas
                                #:volume 31
                                #:number 3
                                #:pages '(1 44))
   #:date 2009))

(define ii-cs-2009
  (make-bib
   #:title "Gradual Typing for Featherweight Java"
   #:author (authors "Lintaro Ina" "Atsushi Igarashi")
   #:location (journal-location "Computer Software"
                                #:volume 26
                                #:number 2
                                #:pages '(18 40))
   #:date 2009))

(define shb-icfp-2009
  (make-bib
   #:title "A Theory of Typed Coercions and its Applications"
   #:author (authors "Nikhil Swamy" "Michael Hicks" "Gavin M. Bierman")
   #:location (proceedings-location icfp #:pages '(329 340))
   #:date 2009))

(define bfnorsvw-oopsla-2009
  (make-bib
   #:title "Thorn: Robust, Concurrent, Extensible Scripting on the JVM"
   #:author (authors "Bard Bloom" "John Field" "Nathaniel Nystrom"
                     "Johan Östlund" "Gregor Richards" "Rok Strniša"
                     "Jan Vitek" "Tobias Wrigstad")
   #:location (proceedings-location oopsla #:pages '(117 136))
   #:date 2009))

(define furr-dissertation-2009
  (make-bib
   #:title "Combining Static and Dynamic Typing in Ruby"
   #:author "Michael Furr"
   #:location (dissertation-location #:institution "University of Maryland"
                                     #:degree "Ph.D.")
   #:date 2009))

(define tobin-hochstadt-dissertation-2010
  (make-bib
   #:title "Typed Scheme: From Scripts to Programs"
   #:author "Sam Tobin-Hochstadt"
   #:location (dissertation-location #:institution "Northeastern University"
                                     #:degree "Ph.D.")
   #:date 2010))

(define wnlov-popl-2010
  (make-bib
   #:title "Integrating Typed and Untyped Code in a Scripting Language."
   #:author (authors "Tobias Wrigstad" "Francesco Zappa Nardelli"
                     "Sylvain Lebresne" "Johan Östlund" "Jan Vitek")
   #:location (proceedings-location popl #:pages '(377 388))
   #:date 2010))

(define sw-popl-2010
  (make-bib
   #:title "Threesomes, with and without blame"
   #:author (authors "Jeremy G. Siek" "Philip Wadler")
   #:location (proceedings-location popl #:pages '(365 376))
   #:date 2010))

(define htf-hosc-2010
  (make-bib
   #:title "Space-efficient Gradual Typing"
   #:author (authors "David Herman" "Aaron Tomb" "Cormac Flanagan")
   #:location (journal-location hosc
                                #:volume 23
                                #:number 2
                                #:pages '(167 189))
   #:date 2010))

(define bmt-ecoop-2010
  (make-bib
   #:title "Adding Dynamic Types to C#"
   #:author (authors "Gavin Bierman" "Erik Meijer" "Mads Torgersen")
   #:location (proceedings-location ecoop #:pages '(76 100))
   #:date 2010))

(define gray-fool-2010
  (make-bib
   #:title "Interoperability in a Scripted World: Putting Inheritance and Prototypes Together"
   #:author "Kathryn E. Gray"
   #:location (proceedings-location fool)
   #:date 2010))

(define afsw-popl-2011
  (make-bib
   #:author (authors "Amal Ahmed" "Robert Bruce Findler"
                     "Jeremy G. Siek" "Philip Wadler")
   #:title "Blame for All"
   #:location (proceedings-location popl #:pages '(201 214))
   #:date 2011))

(define thscff-pldi-2011
  (make-bib
   #:title "Languages as Libraries"
   #:author (authors "Sam Tobin-Hochstadt" "Vincent St-Amour"
                     "Ryan Culpepper" "Matthew Flatt" "Matthias Felleisen")
   #:location (proceedings-location pldi #:pages '(132 141))
   #:date 2011))

(define bce-icse-2011
  (make-bib
   #:title "Always-available Static and Dynamic Feedback"
   #:author (authors "Michael Bayne" "Richard Cook" "Michael D. Ernst")
   #:location (proceedings-location icse #:pages '(521 530))
   #:date 2011))

(define wgta-ecoop-2011
  (make-bib
   #:title "Gradual Typestate"
   #:author (authors "Roger Wolff" "Ronald Garcia"
                     "Éric Tanter" "Jonathan Aldrich")
   #:location (proceedings-location ecoop #:pages '(459 483))
   #:date 2011))

(define ii-oopsla-2011
  (make-bib
   #:title "Gradual Typing for Generics"
   #:author (authors "Lintaro Ina" "Atsushi Igarashi")
   #:location (proceedings-location oopsla #:pages '(609 624))
   #:date 2011))

(define cmscgbwf-dls-2011
  (make-bib
   #:title "The Impact of Optional Type Information on JIT Compilation of Dynamically Typed Languages"
   #:author (authors "Mason Chang" "Bernd Mathiske"
                     "Edwin Smith" "Avik Chaudhuri"
                     "Andreas Gal" "Michael Bebenita"
                     "Christian Wimmer" "Michael Franz")
   #:location (proceedings-location dls #:pages '(13 24))
   #:date 2011))

(define rch-popl-2012
  (make-bib
   #:title "The Ins and Outs of Gradual Type Inference"
   #:author (authors "Aseem Rastogi" "Avik Chaudhuri" "Basil Hosmer")
   #:location (proceedings-location popl #:pages '(481 494))
   #:date 2012))

(define dthf-esop-2012
  (make-bib
   #:title "Complete Monitors for Behavioral Contracts"
   #:author (authors "Christos Dimoulas" "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location esop #:pages '(214 233))
   #:date 2012))

(define sthff-oopsla-2012
  (make-bib
   #:author (authors "T. Stephen Strickland" "Sam Tobin-Hochstadt" "Robert Bruce Findler" "Matthew Flatt")
   #:title "Chaperones and Impersonators: Run-time Support for Reasonable Interposition"
   #:location (proceedings-location oopsla #:pages '(943 962))
   #:date 2012))

(define tsdthf-oopsla-2012
  (make-bib
   #:author (authors "Asumu Takikawa" "T. Stephen Strickland"
                     "Christos Dimoulas" "Sam Tobin-Hochstadt"
                     "Matthias Felleisen")
   #:title "Gradual Typing for First-Class Classes"
   #:location (proceedings-location oopsla #:pages '(793 810))
   #:date 2012))

(define tsth-esop-2013
  (make-bib
   #:author (authors "Asumu Takikawa" "T. Stephen Strickland" "Sam Tobin-Hochstadt")
   #:title "Constraining Delimited Control with Contracts"
   #:location (proceedings-location esop #:pages '(229 248))
   #:date "2013"))

(define acftd-scp-2013
  (make-bib
   #:author (authors "Esteban Allende" "Oscar Callaú" "Johan Fabry" "Éric Tanter" "Marcus Denker")
   #:title "Gradual typing for Smalltalk"
   #:location (journal-location "Science of Computer Programming")
   #:date 2013))

(define aft-dls-2013
  (make-bib
   #:author (authors "Esteban Allende" "Johan Fabry" "Éric Tanter")
   #:title "Cast Insertion Strategies for Gradually-Typed Objects"
   #:location (proceedings-location dls #:pages '(27 36))
   #:date 2013))

(define vksb-dls-2014
  (make-bib
   #:author (authors "Michael M. Vitousek" "Andrew Kent" "Jeremy G. Siek" "Jim Baker")
   #:title "Design and Evaluation of Gradual Typing for Python"
   #:location (proceedings-location dls #:pages '(45 56))
   #:date 2014))

(define rsfbv-popl-2015
  (make-bib
   #:author (authors "Aseem Rastogi" "Nikhil Swamy" "Cédric Fournet"
                     "Gavin Bierman" "Panagiotis Vekris")
   #:title "Safe & Efficient Gradual Typing for TypeScript"
   #:location (proceedings-location popl #:pages '(167 180))
   #:date 2015))

(define gc-popl-2015
  (make-bib
   #:author (authors "Ronald Garcia" "Matteo Cimini")
   #:title "Principal Type Schemes for Gradual Programs"
   #:location (proceedings-location popl #:pages '(303 315))
   #:date 2015))

(define vcgts-esop-2015
  (make-bib
   #:title "Monotonic References for Efficient Gradual Typing"
   #:location (proceedings-location esop #:pages '(432 456))
   #:date 2015
   #:author (authors "Jeremy Siek"
                     "Michael M. Vitousek"
                     "Matteo Cimmini"
                     "Sam Tobin-Hochstadt"
                     "Ronald Garcia")))

(define tfdffthf-ecoop-2015
  (make-bib
   #:author (authors "Asumu Takikawa" "Daniel Feltey"
                     "Earl Dean" "Robert Bruce Findler"
                     "Matthew Flatt" "Sam Tobin-Hochstadt"
                     "Matthias Felleisen")
   #:title "Towards Practical Gradual Typing"
   #:location (proceedings-location ecoop)
   #:date 2015))

(define rnv-ecoop-2015
  (make-bib
   #:author (authors "Gregor Richards" "Francesco Zappa Nardelli" "Jan Vitek")
   #:title "Concrete Types for TypeScript"
   #:location (proceedings-location ecoop)
   #:date 2015))

(define tt-oopsla-2015
  (make-bib
   #:title "Customizable Gradual Polymorphic Effects for Scala"
   #:author (authors "Matías Toro" "Éric Tanter")
   #:location (proceedings-location popl #:pages '(935 953))
   #:date 2015))

(define gct-popl-2016
  (make-bib
   #:title "Abstracting Gradual Typing"
   #:author (authors "Ronald Garcia" "Alison M. Clark" "Éric Tanter")
   #:location (proceedings-location popl #:pages '(429 442))
   #:date 2016))

(define cs-popl-2016
  (make-bib
   #:title "The Gradualizer: A methodology and algorithm for generating gradual type systems"
   #:author (authors "Matteo Cimini" "Jeremy Siek")
   #:location (proceedings-location popl #:pages '(443 455))
   #:date 2016))

(define tfgnvf-popl-2016
  (make-bib
   #:title "Is Sound Gradual Typing Dead?"
   #:author (authors "Asumu Takikawa" "Daniel Feltey" "Ben Greenman" "Max New" "Jan Vitek" "Matthias Felleisen")
   #:location (proceedings-location popl #:pages '(456 468))
   #:date 2016))

;; ----------------------------------------
; Early Work on Interoperation

(define ff-icfp-2002
  (make-bib
   #:title "Contracts for Higher-Order Functions"
   #:author (authors "Robert Bruce Findler" "Matthias Felleisen")
   #:location (proceedings-location icfp #:pages '(48 59))
   #:date "2002"))

(define gff-oopsla-2005
  (make-bib
   #:title "Fine-Grained Interoperability through Mirrors and Contracts"
   #:author (authors "Kathryn E. Gray" "Robert Bruce Findler" "Matthew Flatt")
   #:location (proceedings-location oopsla #:pages '(231 245))
   #:date 2015))

;; ----------------------------------------
; Related

;; ----------------------------------------
; Contracts

(define gf-tfp-2007
  (make-bib
   #:title "Unifying Hybrid Types and Contracts"
   #:author (authors "Jessica Gronski" "Cormac Flanagan")
   #:location (proceedings-location tfp)
   #:date 2007))

(define bgip-esop-2011
  (make-bib
   #:title "Polymorphic Contracts"
   #:author (authors "João Filipe Belo" "Michael Greenberg"
                     "Atsushi Igarashi" "Benjamin C. Pierce")
   #:location (proceedings-location esop #:pages '(18 37))
   #:date 2011))

;; ----------------------------------------
; Typing Untyped Languages

(define cartwright-icalp-1976
  (make-bib
   #:title "User-defined Data Types as an Aid to Verifying LISP Programs"
   #:author "Robert Cartwright"
   #:location (proceedings-location icalp #:pages '(228 256))
   #:date 1976))

(define suzuki-popl-1981
  (make-bib
   #:title "Inferring Types in Smalltalk"
   #:author "Norihisa Suzuki"
   #:location (proceedings-location popl #:pages '(187 199))
   #:date 1981))

(define st-popl-1984
  (make-bib
   #:title "Creating Efficient Systems for Object-Oriented Languages"
   #:author (authors "Norihisa Suzuki" "Minoru Terada")
   #:location (proceedings-location popl #:pages '(290 296))
   #:date 1984))

(define am-popl-1991
  (make-bib
   #:title "Static Type Inference in a Dynamically Typed Language"
   #:author (authors "Alexander Aiken" "Brian R. Murphy")
   #:location (proceedings-location popl #:pages '(279 290))
   #:date 1991))

(define cf-pldi-1991
  (make-bib
   #:title "Soft Typing"
   #:author (authors "Robert Cartwright" "Mike Fagan")
   #:location (proceedings-location pldi #:pages '(278 292))
   #:date 1991))

(define bg-oopsla-1993
  (make-bib
   #:title "Strongtalk: Typechecking Smalltalk in a Production Environment"
   #:author (authors "Gilad Bracha" "David Griswold")
   #:location (proceedings-location oopsla #:pages '(215 230))
   #:date 1993))

(define awl-popl-1994
  (make-bib
   #:title "Soft Typing with Conditional Types"
   #:author (authors "Alexander Aiken" "Edward L. Wimmers" "T.K. Lakshman")
   #:location (proceedings-location popl #:pages '(163 173))
   #:date 1994))

(define henglein-scp-1994
  (make-bib
   #:author "Fritz Henglein"
   #:title "Dynamic Typing: Syntax and Proof Theory"
   #:location (journal-location "Science of Computer Programming"
                                #:volume 22
                                #:number 3
                                #:pages '(197 230))
   #:date 1994))

(define hr-fpca-1995
  (make-bib
   #:author (authors "Fritz Henglein" "Jakob Rehof")
   #:title "Safe Polymorphic Type Inference for a Dynamically Typed Language: Translating Scheme to ML"
   #:location (proceedings-location fpca #:pages '(192 203))
   #:date 1995))

(define haynes-tech-1995
  (make-bib
   #:author "Christopher T. Haynes"
   #:title "Infer: a Statically-typed Dialect of Scheme"
   #:location (techrpt-location #:institution "Indiana University"
                                #:number "367")
   #:date 1995))

(define akers-dissertation-1996
  (make-bib
   #:title "Strong Static Type Checking for Functional Common Lisp"
   #:author "Robert Akers"
   #:location (dissertation-location #:institution "University of Texas")
   #:date 1996))

(define ffkwf-pldi-1996
  (make-bib
   #:title "Catching bugs in the web of program invariants"
   #:author (authors "Cormac Flanagan" "Matthew Flatt"
                     "Shriram Krishnamurthi" "Stephanie Weirich"
                     "Matthias Felleisen")
   #:location (proceedings-location pldi #:pages '(23 32))
   #:date 1996))

(define mw-icfp-1997
  (make-bib
   #:title "A Practical Subtyping System for Erlang"
   #:author (authors "Simon Marlow" "Philip Wadler")
   #:location (proceedings-location icfp #:pages '(136 149))
   #:date 1997))

(define wc-toplas-1997
  (make-bib
   #:title "A Practical Soft Type System for Scheme"
   #:author (authors "Andrew K. Wright" "Robert Cartwright")
   #:location (journal-location toplas
                                #:volume 19
                                #:number 1
                                #:pages '(87 152))
   #:date 1997))

(define ls-ppdp-2006
  (make-bib
   #:title "Practical Type Inference Based on Success Typings"
   #:author (authors "Tobias Lindahl" "Konstantinos Sagonas")
   #:location (proceedings-location ppdp #:pages '(167 178))
   #:date 2006))

;; ----------------------------------------
; Type Systems for Gradual Typing

(define thf-icfp-2010
  (make-bib
   #:title "Logical Types for Untyped Languages"
   #:author (authors "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location icfp #:pages '(117 128))
   #:date 2010))

(define sthff-padl-2012
  (make-bib
   #:title "Typing the Numeric Tower"
   #:author (authors "Vincent St-Amour" "Sam Tobin-Hochstadt"
                     "Matthew Flatt" "Matthias Felleisen")
   #:location (proceedings-location padl #:pages '(289 303))
   #:date 2012))

(define fafh-sac-2009
  (make-bib
   #:title "Static Type Inference for Ruby"
   #:author (authors "Michael Furr" "Jong-hoon (David) An"
                     "Jeffrey S. Foster" "Michael Hicks")
   #:location (proceedings-location sac #:pages '(1859 1866))
   #:date 2009))

(define acfh-popl-2011
  (make-bib
   #:title "Dynamic Inference of Static Types for Ruby"
   #:author (authors "Jong-hoon (David) An" "Avik Chaudhuri"
                     "Jeffrey S. Foster" "Michael Hicks")
   #:location (proceedings-location popl #:pages '(459 472))
   #:date 2011))

(define chj-oopsla-2012
  (make-bib
   #:title "Dependent Types for JavaScript"
   #:author (authors "Ravi Chugh" "David Herman"
                     "Ranjit Jhala")
   #:location (proceedings-location oopsla #:pages '(587 606))
   #:date 2012))

(define bonnaire-sergeant-thesis-2012
  (make-bib
   #:title "A Practical Optional Type System for Clojure"
   #:location (dissertation-location
               #:institution "University of Western Australia"
               #:degree "Honour's")
   #:date 2012))

(define mmi-dyla-2014
  (make-bib
   #:title "Typed Lua: An Optional Type System for Lua"
   #:author (authors "André Murbach Maidl" "Fabio Mascarenhas" "Roberto Ierusalimschy")
   #:location (proceedings-location dyla #:pages '(1 10))
   #:date 2014))
