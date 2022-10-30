# fluency ranking and NLI alpha IAA and correlation with some automatic metrics

import glob, os
import pandas as pd
import krippendorff
import numpy as np
import scipy.stats
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd

################

# FR IAA


conversions = {"T1": "Translation 1", "T2": "Translation 2", "T3": "Translation 3"}

fr_path = "LoLa/exp2/materials/experimental_items/fr-items123.csv"
fr_items = pd.read_csv(fr_path, index_col=0)

path = "LoLa/exp2/results/CSVs"
all_files = glob.glob(os.path.join(path, "*.csv"))
df = pd.concat((pd.read_csv(f) for f in all_files), ignore_index=True)


# rename columns with systems

df = df[[col for col in df if col.startswith('FR-')]]
new_cols = ["-".join([col.split("-")[0], col.split("-")[1], fr_items[conversions[col.split("-")[-1]]].iloc[int(col.split("-")[1][-1])]]) for col in df.columns]
df.columns = new_cols

# filters

all = [col for col in df if col.startswith('FR-')]
baseline = [col for col in df if col.endswith('-Baseline')]
ranta = [col for col in df if col.endswith('-Ranta')]
lola = [col for col in df if col.endswith('-LoLa')]
ggc = [col for col in df if col.startswith('FR-GGC')]
rg = [col for col in df if col.startswith('FR-RG')]
baseline_ggc = [col for col in df if col.endswith('-Baseline') and col.startswith('FR-GGC')]
ranta_ggc = [col for col in df if col.endswith('-Ranta') and col.startswith('FR-GGC')]
lola_ggc = [col for col in df if col.endswith('-LoLa') and col.startswith('FR-GGC')]
baseline_rg = [col for col in df if col.endswith('-Baseline') and col.startswith('FR-RG')]
ranta_rg = [col for col in df if col.endswith('-Ranta') and col.startswith('FR-RG')]
lola_rg = [col for col in df if col.endswith('-LoLa') and col.startswith('FR-RG')]

filters = [all, baseline, ranta, lola, ggc, rg, baseline_ggc, ranta_ggc, lola_ggc, baseline_rg, ranta_rg, lola_rg]


# compute krippendorff IAA

for f in filters:
    print([name for name in globals() if globals()[name] is f][0], krippendorff.alpha(df[f].values, level_of_measurement="ordinal"))

# all 0.47504110894987583
# baseline 0.44359493959396057
# ranta 0.4137634807675693
# lola 0.2942232251538157
# ggc 0.5315079553199504
# rg 0.4185846390296848
# baseline_ggc 0.3391950626748548
# ranta_ggc 0.4685175878575949
# lola_ggc 0.14159278155706723
# baseline_rg 0.46753289871106385
# ranta_rg 0.3437446200793689
# lola_rg 0.31006083580934185


#######

# compute correlation between (1) average ranking of a GGC translation (no ref for RG) and (2) its score (against a golden reference) in an automatic metric
# this is done with all the metrics that have individual score per pair
# sentence scored from 1-3 by multiple annotators, take average score (rank), score sentence with automatic metric, correlation
# scored 3 less fluent (supposedely translated by baseline) should be more distant from golden ref so a lower score in n-gram based metrics


scores = pd.read_csv("data/all_results.csv")

systems = ["Baseline", "LoLa", "Ranta"]
metrics = ["BERTScore", "ROUGE", "WMS", "SMS", "SBERT"]


# preprocessing

def replace_bulleting(s):
    return s.replace(r'\n\t'+chr(8226), r'\item')

def remove_punctuation(s):
    s = str(s)
    if s.startswith(" "):
        s = s[1:]
    s = s[0].lower() + s[1:]    # lowercase
    s = s.replace(",", " ,")    # add space before comma
    s = s[:-1]                  # remove end sentence period
    s = s.translate(str.maketrans('𝗔𝐁𝐂𝐄', 'abce'))
    return s

def preproc(s):
    s = replace_bulleting(s)
    s = remove_punctuation(s)
    return s

ggc_items = fr_items[systems][:10].applymap(preproc)


# compute stats

for metric in metrics:

    metric_scores = []

    for i in range(len(ggc_items)):
        for sys in systems:
            try:
                score = scores[metric + "-" + sys].iloc[scores.index[scores[sys] == ggc_items[sys].iloc[i]].tolist()[0]]
            except:
                score = np.nan
            metric_scores.append(score)


    mean_rank = df[ggc].mean().sort_index()

    corr = pd.DataFrame({"Average FR rank": mean_rank, "Metric score": metric_scores})
    corr.dropna(inplace=True)
    # print(corr.sort_values("Average FR rank"))

    print(metric)

    pr = scipy.stats.pearsonr(corr["Average FR rank"], corr["Metric score"])
    print("Pearson | score: {} | p-value: {} ".format(pr[0], pr[1]))
    sp = scipy.stats.spearmanr(corr["Average FR rank"], corr["Metric score"])
    print("Spearman | score: {} | p-value: {} ".format(sp[0], sp[1]))
    kt = scipy.stats.kendalltau(corr["Average FR rank"], corr["Metric score"])
    print("Kendall | score: {} | p-value: {} ".format(kt[0], kt[1]))


    plt.clf()
    sns.regplot(x=corr["Metric score"], y=corr["Average FR rank"], line_kws={"color": "red"}).set_title(metric)
    plt.savefig("{}_FR.svg".format(metric), format="svg", transparent=True)

# BERTScore
# Pearson | score: -0.4774802810365462 | p-value: 0.011780641257953849 # token based
# Spearman | score: -0.4133078534710686 | p-value: 0.0321251447417262 
# Kendall | score: -0.3000324447089036 | p-value: 0.03246116925565965 
# ROUGE
# Pearson | score: -0.5053090448149967 | p-value: 0.007176311636177032 # indeed more negative than semantic based + p-value more significant
# Spearman | score: -0.5354343382629374 | p-value: 0.00400064275387538 
# Kendall | score: -0.40238494760047516 | p-value: 0.004515215534270317 
# WMS
# Pearson | score: -0.3422331549521976 | p-value: 0.08057597525205162 
# Spearman | score: -0.4379810461719603 | p-value: 0.022316055979299275 
# Kendall | score: -0.347033179818176 | p-value: 0.012715684774661182 
# SMS
# Pearson | score: -0.3542429906336373 | p-value: 0.06984660873909795 
# Spearman | score: -0.3558978850712013 | p-value: 0.0684589137934809 
# Kendall | score: -0.28870827564705404 | p-value: 0.038184163821379714 
# SBERT
# Pearson | score: -0.34856881980481064 | p-value: 0.07476957493100037
# Spearman | score: -0.338188844494502 | p-value: 0.08445815901629583 
# Kendall | score: -0.24120255358951076 | p-value: 0.08555335612334412


#######

# want to see the correlation between the human judgment and the scores of automatic metrics against a golden ref
# i.e., this sentence was scored x by the metric (it is similar or not, structurally or semantically, to a reference), the human says that it is comprehensible or not, see correlation
# hypo: the higher the metric score (more human-like), the more comprehensible
# sentence scored 0 or 1 by multiple annotators, take average score, score sentence with automatic metric, correlation


scores = pd.read_csv("data/all_results.csv")

systems = ["Baseline", "LoLa", "Ranta"]
metrics = ["BERTScore", "ROUGE", "WMS", "SMS", "SBERT"]


nli_path = "LoLa/exp2/materials/experimental_items"
nli_files = sorted(glob.glob(os.path.join(nli_path, "nli-items*")))

result_path = "LoLa/exp2/results/CSVs"
result_files = sorted(glob.glob(os.path.join(result_path, "*.csv")))

nli_items1, nli_items2, nli_items3 = [pd.read_csv(f, index_col=0) for f in nli_files]
result1, result2, result3 = [pd.read_csv(f, index_col=0) for f in result_files]


nlis_results = list(zip([nli_items1, nli_items2, nli_items3], [result1, result2, result3]))

# preprocessing

def replace_bulleting(s):
    return s.replace(r'\n\t'+chr(8226), r'\item')

def remove_punctuation(s):
    s = str(s)
    if s.startswith(" "):
        s = s[1:]
    s = s[0].lower() + s[1:]    # lowercase
    s = s.replace(",", " ,")    # add space before comma
    s = s[:-1]                  # remove end sentence period
    s = s.translate(str.maketrans('𝗔𝗕𝐁𝗖𝐂𝗗𝗘𝐄𝗙', 'abbccdeef'))
    return s

def preproc(s):
    s = replace_bulleting(s)
    s = remove_punctuation(s)
    return s


# to know nli score of each ggc question of each group

dfs = []

for t in nlis_results:

    items, results = t[0], t[1]

    ggc_items = items[:21]
    mean = results[[col for col in results if col.startswith('NLI-GGC')]].eq('Correct').mul(1).mean().tolist()

    ggc_items["NLIscore"] = mean

    dfs.append(ggc_items)


all_ggc_items = pd.concat(dfs)
all_ggc_items["Translation"] = all_ggc_items["Translation"].apply(preproc)


# compute stats

for metric in metrics:

    metric_scores = []

    for i in range(len(all_ggc_items)):
        try:
            score = scores[metric + "-" + all_ggc_items["System"].iloc[i]].iloc[scores.index[scores[all_ggc_items["System"].iloc[i]] == all_ggc_items["Translation"].iloc[i]].tolist()[0]]
        except:
            score = np.nan
        metric_scores.append(score)

    all_ggc_items[metric] = metric_scores

    corr = pd.DataFrame({"Average NLI score": all_ggc_items["NLIscore"], "Metric score": metric_scores})
    corr.dropna(inplace=True)

    print(metric)

    pr = scipy.stats.pearsonr(corr["Average NLI score"], corr["Metric score"])
    print("Pearson | score: {} | p-value: {} ".format(pr[0], pr[1]))
    sp = scipy.stats.spearmanr(corr["Average NLI score"], corr["Metric score"])
    print("Spearman | score: {} | p-value: {} ".format(sp[0], sp[1]))
    kt = scipy.stats.kendalltau(corr["Average NLI score"], corr["Metric score"])
    print("Kendall | score: {} | p-value: {} ".format(kt[0], kt[1]))


    plt.clf()
    sns.regplot(x=corr["Metric score"], y=corr["Average NLI score"], line_kws={"color": "red"}).set_title(metric)
    plt.savefig("{}_NLI.svg".format(metric), format="svg", transparent=True)

# all_ggc_items.dropna(inplace=True)
# print(all_ggc_items[["Formula", "System", "Translation", "NLIscore", "BERTScore", "ROUGE", "SBERT"]].sort_values("NLIscore"))

# BERTScore
# Pearson | score: 0.04539321764888457 | p-value: 0.7374051656370961 
# Spearman | score: 0.07655102069482898 | p-value: 0.5714108573067325 
# Kendall | score: 0.05503034276687387 | p-value: 0.5706767472189966 
# ROUGE
# Pearson | score: -0.08279906764647466 | p-value: 0.5403241826244005 
# Spearman | score: -0.11855059417987371 | p-value: 0.37977487807496046 
# Kendall | score: -0.0820644769765668 | p-value: 0.39886545020491326 
# WMS
# Pearson | score: 0.1066597702488713 | p-value: 0.4297148298830784 
# Spearman | score: -0.10284491593070925 | p-value: 0.446488750194324 
# Kendall | score: -0.0521640530957301 | p-value: 0.5901467240131051 
# SMS
# Pearson | score: 0.09860913858026811 | p-value: 0.46553056413314226 
# Spearman | score: -0.1609471295306953 | p-value: 0.2316889021251266 
# Kendall | score: -0.0960916767552923 | p-value: 0.32109946659387534 
# SBERT
# Pearson | score: 0.008965169495393083 | p-value: 0.9472287116893009 
# Spearman | score: -0.0033485322693125627 | p-value: 0.9802776380688745 
# Kendall | score: 0.008939618172200495 | p-value: 0.9265832132987569 

## no correlation + only 7 people on average rated each sentence. no significance


### IAA for NLI

df_list = [result[[col for col in result if col.startswith('NLI-GGC')]].eq('Correct').mul(1) for result in [result1, result2, result3]]

offset_x = offset_y = 0
for df in df_list:
    df.index = np.arange(len(df)) + offset_x
    df.columns = np.arange(len(df.columns)) + offset_y

    offset_x += df.index[-1] + 1
    offset_y += df.columns[-1] + 1

matrix = pd.concat(df_list)

k = krippendorff.alpha(matrix, level_of_measurement="nominal") # 0.1809072950221573