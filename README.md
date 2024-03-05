# Haskell Tableaux-based Prover for Superintuitionistic Logics

- Based on a mix of the sequent calculi m-G4ip and intuitionistic tableau calculus.
- Once a variable $p$ or $\neg p$ is added to the antecedent, the sequent is substituted with $[\top/p]$ or $[\bot/p]$ respectively.
- Formulae are reduced with boolean simplification rules before proof attempt and during substitutions.
- Inference rules with propositional variables are omitted in favour of simplification and substitution rules.
- If a rule is not applicable to a certain formula of the sequent, that particular formula is excluded in future checks for this rule.

Dyckhoff, R (1992). Contraction-free sequent calculi for intuitionistic logic. The Journal of Symbolic Logic, 57(3):795–807 [doi:10.2307/2275431](https://doi.org/10.2307/2275431)
