import pandas as pd
import random as r

def sampleData(file_in, file_out, csv=False, csvFile=r'utils/data/sample.csv', replace=False):
    r.seed(9999)

    # Open GGC dataset as DataFrame
    ggc_df = pd.read_csv(file_in, 
                        header=0, 
                        error_bad_lines=False)
    
    # Sample n formulas from the GGC dataset 
    sam = ggc_df.sample(n=50, random_state=5555)

    # If "csv" parameter is True, save sample as .csv file
    if(csv):
        sam.to_csv(path_or_buf=csvFile, header=True, index=False)

    # If "replace" parameter is True, replace 50% of material implication
    # occurrences with bi-implications and write to .tmp file
    formulas = list(sam['formula'])
    if(replace):
        newfs = list()
        for f in formulas:
            n = list(f)    
            for i in range(len(n)):
                if (n[i] == '$') and (r.choice(range(2))) == 0:
                    n[i] = '%'
            f = "".join(n)
            newfs.append(f)
        formulas = newfs

    with(open(file_out, 'w')) as f:
        f.write('\n'.join(formulas))
    f.close()

def writeCsv(file_in, file_out):
    file = pd.read_csv(file_in, on_bad_lines="skip", sep=";")
    file.to_csv(file_out, header=True, index=False,sep=";")

# sampleData(r'utils/data/GGC dataset.csv', r'utils/data/Test BIMPL 2/ggc-formulas.tmp', csv=False, replace=False)
writeCsv(r'src/data/output.txt', r'utils/data/Test BIMPL 2/original LoLa/rg-output.csv')