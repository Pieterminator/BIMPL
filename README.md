## LoLa

LoLa is a [Grammatical Framework](http://www.grammaticalframework.org/) (GF) application grammar for translating between first-order predicate logic (FOL) and natural language.  
This is the code accompanying the [GEM @ EMNLP 2022](https://gem-benchmark.com/workshop) paper [Enhancing and Evaluating the Grammatical Framework Approach to Logic-to-Text Generation](https://aclanthology.org/2022.gem-1.13/).

To run the app, follow the instructions in the `README.md` in the `src` folder.

To run all the other files, you need to create a fresh conda environment:

`conda create --name lola --file requirements.txt python=3.9.12`

In `tqa`, you will find the code used for the translation quality assessment.  
In `evaluation` there are 2 subfolders: `automatic`, where you can find the code used for the automatic comparative evaluation of the 3 systems (with the correlations and IAA calculations), and `human`, where you will find the material and scripts used for the comparative human evaluation (NLI and FR tasks).  
In `utils`, you can find the Random Generator, and the support script to preprocess the Grade Grinder Corpus (GGC).

NOTE:
- Unfortunately, we cannot share the GGC. To obtain it, you should contact the authors of the original [paper](https://www.semanticscholar.org/paper/Student-Translations-of-Natural-Language-into-The-Barker-Plummer-Cox/28e805aae41255b8515173669ea19faa61e7cb87). Consequently, we cannot upload the bare results involving GGC formulae, i.e., any for the automatic evaluation, and we had to remove the portion of the GGC involved in the human evaluation and in the TQA. Nonetheless, we share all the randomly generated formulae used in the analyses and all the numerical results.
- Some paths will need to be adjusted based on how you organize the files.

## Citation

```
@inproceedings{calo-etal-2022-enhancing,
    title = "Enhancing and Evaluating the Grammatical Framework Approach to Logic-to-Text Generation",
    author = "Cal{\`o}, Eduardo  and
      van der Werf, Elze  and
      Gatt, Albert  and
      van Deemter, Kees",
    booktitle = "Proceedings of the 2nd Workshop on Natural Language Generation, Evaluation, and Metrics (GEM)",
    month = dec,
    year = "2022",
    address = "Abu Dhabi, United Arab Emirates (Hybrid)",
    publisher = "Association for Computational Linguistics",
    url = "https://aclanthology.org/2022.gem-1.13",
    pages = "148--171",
    abstract = "Logic-to-text generation is an important yet underrepresented area of natural language generation (NLG). In particular, most previous works on this topic lack sound evaluation. We address this limitation by building and evaluating a system that generates high-quality English text given a first-order logic (FOL) formula as input. We start by analyzing the performance of Ranta (2011){'}s system. Based on this analysis, we develop an extended version of the system, which we name LoLa, that performs formula simplification based on logical equivalences and syntactic transformations. We carry out an extensive evaluation of LoLa using standard automatic metrics and human evaluation. We compare the results against a baseline and Ranta (2011){'}s system. The results show that LoLa outperforms the other two systems in most aspects.",
}
```
