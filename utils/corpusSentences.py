import pandas as pd
import random as r
r.seed(5555)

# Open GGC as DataFrame
ggc_df = pd.read_csv("utils/data/dataset.csv", 
                     header=0, 
                     error_bad_lines=False)


sam = ggc_df.sample(n=50, random_state=5555)

pd.DataFrame.to_csv(sam, path_or_buf="utils/data/sample.csv", header=True, index=False)

formulas = list(sam['formula'])
newfs = list()
for f in formulas:
    n = list(f)    
    for i in range(len(n)):
        if (n[i] == '$') and (r.choice(range(2))) == 0:
            n[i] = '%'
    f = "".join(n)
    newfs.append(f)


with(open(r'utils/data/ggc-formulas.tmp', 'w')) as f:
    f.write('\n'.join(newfs))
f.close()