from nltk.sem.logic import *
import pandas as pd

read_expr = Expression.fromstring

def autoEquiv(file_in, forms1, forms2, file_out=None):
    # Read file with formulas
    df = pd.read_csv(file_in, error_bad_lines=False)

    # Remove columns that will not be checked for equivalence
    droplist = [c for c in df.columns if not c in [forms1, forms2]]
    df = df.drop(droplist, axis=1)

    # Rewrite formulas from GGC to Prover9 notation
    df.replace(to_replace=r'\$', value="->", inplace=True, regex=True)
    df.replace(to_replace=r'\~', value="-", inplace=True, regex=True)
    df.replace(to_replace=r'\%', value="<->", inplace=True, regex=True)
    df.replace(to_replace=r'T(?!\S)', value="(T | - T)", inplace=True, regex=True)
    df.replace(to_replace=r'\[PTaut\]', value="(T | - T)", inplace=True, regex=True)
    df.replace(to_replace=r'F(?!\S)', value="(F & - F)", inplace=True, regex=True)
    df.replace(to_replace=r'\[PContra\]', value="(F & - F)", inplace=True, regex=True)

    # Transform strings to logic expressions
    df = df.applymap(read_expr)

    # Check for equivalence
    res = list(map(lambda i, b: i.equiv(b), df[forms1], df[forms2]))
    df["Equivalence"] = res

    #Output result
    if (file_out is None):
        print(df["Equivalence"].value_counts())
    else:
        df.to_csv(file_out, header=True, index=False)

def singleEquiv(form1, form2):
    [f1, f2] = map(read_expr, map(changeNot, [form1, form2]))
    print(f1.equiv(f2))

def changeNot(form):
    notation = form.translate(str.maketrans({'$': "->", '~': "-", '%': "<->"}))
    re.sub(r'T(?!\S)', "(T | - T)", notation)
    re.sub(r'\[PTaut\]', "(T | - T)", notation)
    re.sub(r'F(?!\S)', "(T | - T)", notation)
    re.sub(r'\[PContra\]', "(T | - T)", notation)
    return notation

f1 = "-(-Pe(a) -> (Te(b) <-> Pr(c)))"
f2 = "(-Pe(a) -> (Te(b) <-> Pr(c))) -> -(Do(b) <-> Do(b))"
singleEquiv(f1, f2)
# autoEquiv(r'utils/data/equivalence eval/RG.csv', "Input", "BIMPL", file_out=r'utils/data/equivalence eval/test.csv')