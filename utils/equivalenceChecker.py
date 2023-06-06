import ttg

def singleEq(atoms, formula, latex=False):
    table = ttg.Truths(atoms, [formula])

    print(table)
    print(table.valuation())

    if (latex):
        with(open(r'utils/data/equivalence eval/' + formula + r'.txt', 'w')) as f:
             f.write(table.as_tabulate(index=False, table_format='latex'))
        f.close()


def checkEq(atoms, formula1, formula2, type, eq=True, latex=False):
    if eq:
        equivalence = '( '+ formula1 + ' )' +' = '+ '( '+ formula2 + ' )'
        table = ttg.Truths(atoms, [formula1,formula2, equivalence])
        print(table.valuation())
    else:
        table = ttg.Truths(atoms, [formula1,formula2])

    print(table)

    if (latex):
        with(open(r'utils/data/equivalence eval/' + type + r'/' + formula1 + r'.txt', 'w')) as f:
             f.write(table.as_tabulate(index=False, table_format='latex'))
        f.close()

a = ['p','q'#,'r'
     ]
f1 = '( p = q ) = ~ q'
f2 = '~ p'

# singleEq(a, f1, True)
checkEq(a, f1, f2, r'RG formulas', eq=True, latex=True)
# print(ttg.Truths(['p', 'q'], ['(p = q)', '~q', '((p = q) = -q)', '~p']))

