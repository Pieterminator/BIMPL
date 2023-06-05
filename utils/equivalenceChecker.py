import ttg

def singleEq(atoms, formula, latex=False):
    table = ttg.Truths(atoms, [formula])

    print(table)
    print(table.valuation())

    if (latex):
        with(open(r'utils/data/equivalence eval/' + formula + r'.txt', 'w')) as f:
             f.write(table.as_tabulate(index=False, table_format='latex'))
        f.close()


def checkEq(atoms, formula1, formula2, type, latex=False):
    equivalence = '('+ formula1 + ')' +' = '+ '('+ formula2 + ')'
    table = ttg.Truths(atoms, [formula1,formula2, equivalence])

    print(table)
    print(table.valuation())

    if (latex):
        with(open(r'utils/data/equivalence eval/' + type + r'/' + formula1 + r'.txt', 'w')) as f:
             f.write(table.as_tabulate(index=False, table_format='latex'))
        f.close()


a = ['p', 'q', 'r', 's'
     ]
f1 = '(((p => q) and (q => r)) and (r => s)) and (s => p)'
f2 = '((p = q) and (p = r)) and (p = s)'

# singleEq(a, f1, True)
checkEq(a, f1, f2, "GGC formulas", True)

