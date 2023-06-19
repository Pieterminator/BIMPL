"""Python script to create a list of randomly generated propositional logic 
formulas in GGC notation, for a given test lexicon."""

import string
import random as r
r.seed(1234)

class Lexicon:
    def __init__(self, Kinds):
        self.Kinds = Kinds    # Kind predicates

class RandomGenerator:
    def __init__(self, lexicon, maxdepth):
        self.lex = lexicon
        self.new_cons_i = 0         # index for a new constant
        self.maxdepth = maxdepth    # maximum depth of a generated proposition

    def makeProp(self, n):
        """
        Randomly generate a proposition from the lexicon,
        with depth n.In the case of conjunction, disjunction, 
        implication, and bi-implication, an extra check is done 
        such that the final prop does not include outer brackets, 
        because then it will not be parsable by the GF shell.
        """
        if n == 0:          # termination signal: atomic proposition
            return self.makeAtom()
        else: 
            i = r.choice(range(9))
            if i == 0:      # atomic proposition
                return self.makeAtom()
            
            elif i == 1:    # negation
                return "~ " + self.makeProp(n-1)
            
            elif i == 2:    # conjunction
                if n == self.maxdepth:      # exclude outer brackets
                    return self.makeProp(n-1) + " & " + self.makeProp(n-1)
                else:                       # include outer brackets
                    return "( " + self.makeProp(n-1) + " & " + self.makeProp(n-1) + " )"
            
            elif i == 3:    # disjunction
                if n == self.maxdepth:      # exclude outer brackets
                    return self.makeProp(n-1) + " | " + self.makeProp(n-1)
                else:                       # include outer brackets
                    return "( " + self.makeProp(n-1) + " | " + self.makeProp(n-1) + " )"
            
            elif i <= 5:    # implication
                if n == self.maxdepth:      # exclude outer brackets
                    return self.makeProp(n-1) + " $ " + self.makeProp(n-1)
                else:                       # include outer brackets
                    return "( " + self.makeProp(n-1) + " $ " + self.makeProp(n-1) + " )"
            
            elif i <= 8:    # bi-implication
                if n == self.maxdepth:      # exclude outer brackets
                    return self.makeProp(n-1) + " % " + self.makeProp(n-1)
                else:                       # include outer brackets
                    return "( " + self.makeProp(n-1) + " % " + self.makeProp(n-1) + " )"
    
    def makeAtom(self):
        """
        Randomly generate an atomic proposition from the lexicon,
        with maximum depth n.
        """

        return r.choice(self.lex.Kinds) + " ( " + self.makeCons() + " )"
        
            
    def makeCons(self):
        """
        Generate a constant (new or already used) in the lexicon.
        """
        i = r.randint(0, self.new_cons_i)
        if i == self.new_cons_i:
            self.new_cons_i += 1
        return string.ascii_lowercase[i]
    
#TEST 3 GGC
ggcLexicon = Lexicon(["Dodec", "Student", "Cube", "Prime", "Person", 
                         "Tet", "Pet"])                 #Kinds

# # Test to generate 1 proposition        
# rg = RandomGenerator(ggcLexicon, 3)  
# random_formula = rg.makeProp(3)
# print(random_formula)


# Randomly generate 50 formulas with a certain maximum depth, that are
# not too long and not too short
depth = 3       # maximum depth
formulas = []
rg = RandomGenerator(ggcLexicon, depth)

while len(formulas) < 50:
    prop = rg.makeProp(depth)
    if len(prop) >= 20 and len(prop) <= 100:
        formulas.append(prop)
    rg.new_cons_i = 0   #set again to 0 for generating a new prop

# Write formulas to file with newlines
with(open(r'utils/data/Test BIMPL 1/rg-formulas.tmp', 'w')) as f:
    f.write('\n'.join(formulas))
f.close()

        
        
