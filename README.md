# Prover for Superintuitionistic Logics in Haskell

## Installation
```bash
cabal install
```

## Usage
```bash
super LOGIC FILE

#  LOGIC:
#    -cl        Classical Logic
#    -il        Intuitionistic Logic
#    -jn        Jankov Logic
#    -lc        Gödel-Dummett Logic
#    "FORMULA"  Axiomatisation over IL

#  FORMULA:
#    p | ~A | A&B | A|B | A=>B

#  FILE:
#    https://tptp.org/TPTP/SyntaxBNF.html
```

### Examples
```bash
# Jankov
super -jn test/problems/LCL/LCL181+1.p

# Jankov as extension over IPL
super "~p | ~~p" test/problems/LCL/LCL181+1.p

# Gödel-Dummett
super -lc test/problems/LCL/LCL230+1.p
```

## Features
- Based on a the rules of sequent calculi **LJT*** and intuitionistic tableau calculus.
- Once a variable $p$ or $\neg p$ is added to the antecedent, the sequent is substituted with $[\top/p]$ or $[\bot/p]$ respectively.
- Formulas are reduced with boolean simplification rules before proof attempt and during substitutions.
- Formulas of the sequent are analyzed through naive focussing, some may be temporarily locked to restrict backtracking.

## References
1. Dyckhoff (1992). Contraction-free sequent calculi for intuitionistic logic. [doi:10.2307/2275431](https://doi.org/10.2307/2275431)
2. Ferrari (2012). Simplification Rules for Intuitionistic Propositional Tableaux. [doi:10.1145/2159531.2159536](https://doi.org/10.1145/2159531.2159536)
3. Dyckhoff (2016). Intuitionistic Decision Procedures Since Gentzen. [doi:10.1007/978-3-319-29198-7_6](https://doi.org/10.1007/978-3-319-29198-7_6)
