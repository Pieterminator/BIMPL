"""Python script to automatically check whether two formulas are equivalent,
using the NLTK implementation of the Prover9 theorem prover.

Note that BIMPL and Prover9 use different precedence and associativity for
the connectives. It is therefore advised to be explicit in the term ordering
by making use of parentheses, rather than relying on either system's internal
term ordering.

Also note that an installation of Prover9 is required to use this script.
The software can be obtained at https://www.cs.unm.edu/~mccune/prover9/.

For Windows users, the latest (although old) version of the Prover9 GUI 
should be installed. Although the program is no longer supported, this
installation does allow command line acces of the prover. For a more 
detailed elaboration on the installation of Prover9, see the the GitHub
issue at https://github.com/nltk/nltk_book/issues/193.
"""

from nltk.sem.logic import *
import pandas as pd

read_expr = Expression.fromstring

def autoEquiv(file_in, forms1, forms2, file_out=None):
    """
    Checks the validity of multiple equivalence equations, 
    presented in a .csv file as input. Outputs the result
    in file_out, or else prints it as console output.
    """
    # Read file with formulas
    df = pd.read_csv(file_in, on_bad_lines="skip")

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
    """
    Checks the validity of a single equivalence equation.
    """
    [f1, f2] = map(read_expr, map(changeNot, [form1, form2]))
    print(f1.equiv(f2))

def changeNot(form):
    """
    Rewrites the formulas from GGC to Prover9 notation. Can be 
    called independently, or from the singleEquiv() function.
    """
    notation = form.translate(str.maketrans({'$': "->", '~': "-", '%': "<->"}))
    re.sub(r'T(?!\S)', "(T | - T)", notation)
    re.sub(r'\[PTaut\]', "(T | - T)", notation)
    re.sub(r'F(?!\S)', "(T | - T)", notation)
    re.sub(r'\[PContra\]', "(T | - T)", notation)
    return notation


# Example proof
f1 = "( P % Q ) % R"
f2 = "( P % Q ) & ( P % R )"
singleEquiv(f1, f2)

# autoEquiv(r'utils/data/Test BIMPL 2/equivalence eval/RG.csv', "Input", "BIMPL", file_out=None)