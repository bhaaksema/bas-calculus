# Haskell Tableaux-based Prover for Superintuitionistic Logics

- Based on a mix of the sequent calculi m-G4ip and intuitionistic tableau calculus.
- Once a variable $p$ or $\neg p$ is added to the antecedent, the sequent is substituted with $[\top/p]$ or $[\bot/p]$ respectively.
- Formulae are reduced with boolean simplification rules before proof attempt and during substitutions.
- Inference rules with propositional variables are omitted in favour of simplification and substitution rules.
- If a rule is not applicable to a certain formula of the sequent, that particular formula is excluded in future checks for this rule.

1. Dyckhoff (1992). Contraction-free sequent calculi for intuitionistic logic. [doi:10.2307/2275431](https://doi.org/10.2307/2275431)
2. Ferrari (2012). Simplification Rules for Intuitionistic Propositional Tableaux. [doi:10.1145/2159531.2159536](https://doi.org/10.1145/2159531.2159536)
3. Dyckhoff (2016). Intuitionistic Decision Procedures Since Gentzen. [doi:10.1007/978-3-319-29198-7_6](https://doi.org/10.1007/978-3-319-29198-7_6)
