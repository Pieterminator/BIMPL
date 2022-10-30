# took ggc portion parsable by GF, linked with golden refs, inferenced with the 3 systems, evaluation

from evaluate import load
import pandas as pd
from statistics import fmean

baseline_path = "data/baseline_predictions.txt"
ranta_path = "data/ranta_predictions.txt"
lola_path = "data/lola_predictions.txt"

refs_path = "data/dataset.csv"


bertscore = load("bertscore") # https://huggingface.co/spaces/evaluate-metric/bertscore (roberta, f1, no-baseline, no-idf, no-agg)
sacrebleu = load("sacrebleu") # https://huggingface.co/spaces/evaluate-metric/sacrebleu (easy-reproducible bleu)
meteor = load("meteor") # https://huggingface.co/spaces/evaluate-metric/meteor (unigram matching: surface, stem, wordnet, f1 with more recall)
rouge = load("rouge") # https://huggingface.co/spaces/evaluate-metric/rouge (recall-based bleu, case insensitive by default, 1,2,L grams, no-agg)

systems = ["Baseline", "Ranta", "LoLa"]
metrics = {bertscore: "BERTScore", sacrebleu: "BLEU", meteor: "METEOR", rouge: "ROUGE"}


with open(baseline_path, "r") as b, open(ranta_path, "r") as r, open(lola_path, "r") as l:
  baseline_predictions = [", ".join(s.split(", ")[1:]) for s in list(filter(None, b.read().splitlines()))]
  ranta_predictions = [line.rstrip() for line in r if not line.isspace()]
  lola_predictions = [", ".join(s.split(", ")[2:]) for s in list(filter(None, l.read().splitlines()))]

references = [ref.lower() for ref in pd.read_csv(refs_path, usecols=[0])["exsentnum"].tolist()]
formulae = pd.read_csv(refs_path, usecols=[1])["canonical"].tolist()

assert len(baseline_predictions) == len(ranta_predictions) == len(lola_predictions) == len(references)

predictions = [baseline_predictions, ranta_predictions, lola_predictions]


# evaluation loop

bert_f1 = []
rouge_f1 = []

partial_results = []

for sys_predictions in predictions:

    bert_results = bertscore.compute(predictions=sys_predictions, references=references, lang="en")
    bleu_results = sacrebleu.compute(predictions=sys_predictions, references=references)
    meteor_results = meteor.compute(predictions=sys_predictions, references=references)
    rouge_results = rouge.compute(predictions=sys_predictions, references=references)

    rouge_results_no_agg = rouge.compute(predictions=sys_predictions, references=references, use_aggregator=False)

    bert_score = fmean(bert_results["f1"])
    bleu_score = bleu_results["score"]
    meteor_score = meteor_results["meteor"]
    rouge_score = rouge_results["rougeL"][1][2]

    system_scores = [bert_score, bleu_score, meteor_score, rouge_score]
    partial_results.append(system_scores)

    bert_f1.append(bert_results["f1"])
    rouge_f1.append([rouge_results_no_agg["rougeL"][i][2] for i, _ in enumerate(rouge_results_no_agg["rougeL"])])

results = pd.DataFrame(partial_results, index=systems, columns=metrics.values())
# results.to_csv("data/final_scores.csv")


data = dict(zip(["Formula", "Reference", *systems, "ROUGE-Baseline", "ROUGE-Ranta", "ROUGE-LoLa", "BERTScore-Baseline", "BERTScore-Ranta", "BERTScore-LoLa"],
                [formulae, references, *predictions, *rouge_f1, *bert_f1]))
final_df = pd.DataFrame(data)
# final_df.to_csv("data/results.csv", index=False)

#########

# sms files creation

final_df[["Reference", "Baseline"]].to_csv("data/baseline.tsv", index=False, header=False, sep="\t")
final_df[["Reference", "Ranta"]].to_csv("data/ranta.tsv", index=False, header=False, sep="\t")
final_df[["Reference", "LoLa"]].to_csv("data/lola.tsv", index=False, header=False, sep="\t")

##########

# sbert - sts

from sentence_transformers import SentenceTransformer, util
from torch import diagonal, mean

model = SentenceTransformer('all-distilroberta-v1')

cosines = []
embeddings1 = model.encode(references, convert_to_tensor=True)

for sys_predictions in predictions:

  embeddings2 = model.encode(sys_predictions, convert_to_tensor=True)

  cosine_scores = util.cos_sim(embeddings1, embeddings2)
  cosines.append(cosine_scores)


data_sb = dict(zip(["Formula", "Reference", *systems, "SBERT-Baseline", "SBERT-Ranta", "SBERT-LoLa"],
                [formulae, references, *predictions, *[diagonal(cosine, 0) for cosine in cosines]]))
df = pd.DataFrame(data_sb)
df.to_csv("data/sbert_results.csv", index=False)

results_sb = pd.DataFrame([mean(diagonal(cosine, 0)).item() for cosine in cosines], index=systems, columns=["SBERT"])
# results.to_csv("data/sbert_scores.csv")