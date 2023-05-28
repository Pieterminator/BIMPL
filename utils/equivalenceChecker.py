import ttg

def checkEq(atoms, formula1, formula2, latex=False):
    equivalence = '('+ formula1 + ')' +' = '+ '('+ formula2 + ')'
    table = ttg.Truths(atoms, [formula1,formula2, equivalence])

    print(table)
    print(table.valuation())

    if (latex):
        print(table.as_tabulate(index=False, table_format='latex'))


a = ['p', 'r']
f1 = 'p = (p and r)'
f2 = 'p and (p = r)'

checkEq(a, f1, f2, False)