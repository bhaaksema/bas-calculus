# Haskell Sequent Calculus Theorem Prover

- Based on a mix of the sequent calculi m-G4ip and intuitionistic tableau calculus.
- The second premise of the $(L\lor)$ rule uses information from the first (as proposed by Weich).
- Once a variable $p$ or $\neg p$ is added to the antecedent, the sequent is substituted with $[\top/p]$ or $[\bot/p]$ respectively.
- Formulae are reduced with boolean simplification rules before proof attempt and during substitutions.
- Rules with propositional variables are omitted in favour of simplification and substitution rules.
- If a rule is not applicable on a certain selection of the sequent, the selected formula is excluded in future selections for this rule.

Dyckhoff, R (1992). Contraction-free sequent calculi for intuitionistic logic. The Journal of Symbolic Logic, 57(3):795–807 [doi:10.2307/2275431](https://doi.org/10.2307/2275431)
