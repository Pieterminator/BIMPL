## LoLa

LoLa is a [Grammatical Framework](http://www.grammaticalframework.org/) (GF) application grammar for translating between first-order predicate logic (FOL) and natural language.  
This is the code accompanying the [GEM @ EMNLP 2022](https://gem-benchmark.com/workshop) paper \[ADD PAPER\]

To run the app, follow the instructions in the `README.md` in the `src` folder.

In `tqa`, you will find the code used for the translation quality assessment.  
In `evaluation` there are 2 subfolders: `automatic`, where you can find the code used for the automatic comparative evaluation of the 3 systems (with the correlations and IAA calculations), and `human`, where you will find the material and scripts used for the comparative human evaluation (NLI and FR tasks).  
In `utils`, you can find the Random Generator, and the support script to preprocess the Grade Grinder Corpus (GGC).

NOTE:
- Unfortunately, we cannot share the GGC. To obtain it, you should contact the authors of the original [paper](https://www.semanticscholar.org/paper/Student-Translations-of-Natural-Language-into-The-Barker-Plummer-Cox/28e805aae41255b8515173669ea19faa61e7cb87). Consequently, we cannot upload the bare results involving GGC formulae, i.e., any for the automatic evaluation, and we had to remove the portion of the GGC involved in the human evaluation and in the TQA. Nonetheless, we share all the randomly generated formulae used in the analyses and all the numerical results.
- Some paths will need to be adjusted based on how you organize the files.
