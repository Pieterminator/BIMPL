"""Python script to create a list of randomly generated Predicate logic 
formulas in GGC notation, for a given test lexicon. This is a modified 
version of the random formula generator presented in LoLa. The modifications
include the inclusion of the bi-implication and the exclusion of quantifiers
and variables in the generated formulas. Also added the seed argument in the
initialisation of the random generator.

In the accompanying thesis "Exploring the bi-implication in 
logic-to-text translation: An endeavour to improve upon Ranta's 
rule-based approach", the RG input formulas that were used in the
paragraph "Testing for minimisation - LoLa's domain" of subsection 4.1
were sampled with seed=4321. 
For subsection 4.3, the formulas were sampled with seed=9999.
For the paragraph "Testing for minimisation - BIMPL's domain" of subsection 4.1, 
some minor modifications were made to the program. The makeProp function 
was defined to generate a random number between 0 and 8, and the material 
implication was generated if the random number was 4 or 5. Also, the 
bi-implication was generatedif the random number was 6,7, or 8 (as seen 
in the part that was commentedout). The set was then sampled with 
seed=1234."""

import string
import random as r

class Lexicon:
    def __init__(self, Kinds):
        self.Kinds = Kinds    # Kind predicates

class RandomGenerator:
    def __init__(self, lexicon, maxdepth, seed=None):
        self.lex = lexicon
        self.new_cons_i = 0         # index for a new constant
        self.maxdepth = maxdepth    # maximum depth of a generated proposition
        r.seed(seed)                # seed that defines the outcome of the random generator

    def makeProp(self, n):
        """
        Randomly generate a proposition from the lexicon,
        with depth n. In the case of conjunction, disjunction, 
        implication, and bi-implication, an extra check is done 
        such that the final prop does not include outer brackets, 
        because then it will not be parsable by the GF shell.
        """
        if n == 0:          # termination signal: atomic proposition
            return self.makeAtom()
        else: 
            i = r.choice(range(5))          # change 5 to 9 for settings for first round of testing
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
            
            elif i <= 4:    # implication: change 4 to 5 for settings for first round of testing
                if n == self.maxdepth:      # exclude outer brackets
                    return self.makeProp(n-1) + " $ " + self.makeProp(n-1)
                else:                       # include outer brackets
                    return "( " + self.makeProp(n-1) + " $ " + self.makeProp(n-1) + " )"
            
            # elif i <= 8:    # bi-implication: include for setting for first round of testing
            #     if n == self.maxdepth:      # exclude outer brackets
            #         return self.makeProp(n-1) + " % " + self.makeProp(n-1)
            #     else:                       # include outer brackets
            #         return "( " + self.makeProp(n-1) + " % " + self.makeProp(n-1) + " )"
    
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
ggcLexicon = Lexicon(["Dodec", "Student", "Cube", "Prime", "Tet"])                 #Kinds

# Randomly generate 50 formulas with a certain maximum depth, that are
# not too long and not too short
depth = 3       # maximum depth
formulas = []
seed = 9999
rg = RandomGenerator(ggcLexicon, depth, seed)

while len(formulas) < 50:
    prop = rg.makeProp(depth)
    if len(prop) >= 20 and len(prop) <= 100:
        formulas.append(prop)
    rg.new_cons_i = 0   #set again to 0 for generating a new prop

# Write formulas to file with newlines
with(open(r'src/data/rg-formulas.tmp', 'w')) as f:
    f.write('\n'.join(formulas))
f.close()