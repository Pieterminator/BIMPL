## Grammatical Framework application grammar for translating between predicate logic and language

LoLa is an extension of Ranta's (2011) Grammatical Framework application:
_Translating between language and logic: what is easy and what is difficult._
A Ranta - International Conference on Automated Deduction, 2011, LNCS/LNAI. See http://www.cse.chalmers.se/~aarne/articles/cade2011.pdf.

The original version of Ranta's code is taken from [cade-2011](https://github.com/GrammaticalFramework/gf-contrib/tree/master/cade-2011). The file `Changes.md` lists all bug fixes and changes to the original code. The file `Experiment2.md` reports the steps taken for setting up experiment 2 (comparative translation quality assessment) and analyzing the results.

### Compile with stack

To build

    make pgf
    stack build

To test

    stack run trans "for all numbers x , x is even or x is odd"

For translating all sentences in a source language from an input text file to a target language with AST simplification, writing them to an output text file:

    stack run trans <source-language> <input-file> <target-language> <output-file>
    

### Source files

```
  -- reports
  Changes.md                -- report of bug fixes and changes in code
  Experiment2.md            -- report of all steps taken in experiment 2, see also the folder `exp2'
  
  -- language-neutral
  Prop.gf                   -- abstract syntax
  Trans.hs                  -- top loop
  TransProp.hs              -- conversions
  TransPropFunctions.hs     -- additional functions used in TransProp
  TransLogicLaws.hs         -- AST functions based on logical equivalence laws of Partee et al. (1990) Mathematical Methods in Linguistics
  Makefile

  -- concrete syntax
  PropI.gf                  -- concrete syntax, functor with RGL
  PropEng.gf                -- concrete syntax, English with RGL functor
  PropGGC.gf                -- concrete syntax, grammar for Grade Grinder Corpus notation

```

### Generated files

```
  Prop.hs        -- abstract syntax in Haskell
  Prop.pgf       -- abstract syntax in Portable Grammar Format (PGF)

```
