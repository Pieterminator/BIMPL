"""Support Python script to sample data from a GGC dataset. Also
includes a function to conveniently convert files to .csv format.

In the accompanying thesis "Exploring the bi-implication in 
logic-to-text translation: An endeavour to improve upon Ranta's 
rule-based approach", the GGC input formulas that were used for 
testing BIMPL's performance were sampled with n=50 and seed=5555.
If one had access to the particular subset of the GGC dataset that 
was used, one could repoduce the sample.
"""

import pandas as pd
import random as r

def sampleData(file_in, file_out, n=50, seed=None, replace=False):
    """
    Randomly samples n formulas from file_in and outputs them in file_out.
    The size and seed of the sample can be specified. 
    """

    # Open GGC dataset as DataFrame
    ggc_df = pd.read_csv(file_in, 
                        header=0, 
                        on_bad_lines="skip")
    
    # Sample n formulas from the GGC dataset. If a seed has been given as
    # function argument, use that as seed. Else, use the system default
    sam = ggc_df.sample(n=n, random_state=seed)

    # If "replace" parameter is True, replace 50% of material implication
    # occurrences with bi-implications
    formulas = list(sam['formula'])
    if(replace):
        r.seed(seed)
        newfs = list()
        for f in formulas:
            n = list(f)    
            for i in range(len(n)):
                if (n[i] == '$') and (r.choice(range(2))) == 0:
                    n[i] = '%'
            f = "".join(n)
            newfs.append(f)
        formulas = newfs

    # Return a file with the sampled formulas
    with(open(file_out, 'w')) as f:
        f.write('\n'.join(formulas))
    f.close()


def writeCsv(file_in, file_out):
    """
    Reads a file as a dataframe, and outputs a semicolon-delimited file.
    """
    file = pd.read_csv(file_in, on_bad_lines="skip", sep=";")
    file.to_csv(file_out, header=True, index=False,sep=";")

writeCsv(r'src/data/output.txt', r'src/data/output.csv')
# sampleData(r'utils/data/GGC dataset.csv', r'src/data/output.csv', seed=5555, replace=True)
