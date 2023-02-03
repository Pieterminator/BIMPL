## Grammatical Framework application grammar for translating between predicate logic and language

LoLa is an extension of [Ranta's (2011)](http://www.cse.chalmers.se/~aarne/articles/cade2011.pdf) GF application.
The original version of Ranta's code is taken from [cade-2011](https://github.com/GrammaticalFramework/gf-contrib/tree/master/cade-2011). The file `Changes.md` lists all bug fixes and changes to the original code.


### Prerequisites

Install GF with HASKELL BINDING. Follow https://inariksit.github.io/gf/2019/12/12/embedding-grammars.html and http://www.grammaticalframework.org/download/index-3.11.html


### Compile with stack

To build

    make pgf
    stack build

To test

    stack run trans "for all numbers x , x is even or x is odd"

For translating all sentences in a source language (e.g., `PropGGC`) from an input text file to a target language (e.g., `PropEng`), writing them to an output text file:

    stack run trans <mode> <source-language> <input-file> <target-language> <output-file>
    
where `<mode>` can be `MNone` to get Baseline-like translations (i.e., (quasi-)literal without any optimization), `MOptimize` to get the original Ranta version (i.e., with some AST optimizations), or `MSimplify` to get LoLa's output (as described in our paper).
    
You can find the list of the predicates supported at the moment in `PropGGC` or in `tqa/legend.jpg`.

### Source files

```
  -- reports
  Changes.md                -- report of bug fixes and changes in code
  
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
