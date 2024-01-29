# Haskell Sequent Calculus Theorem Prover

- Based on the sequent calculi m-G4ip $(Int)$ and G3cp $(Cl)$.
- m-G4ip switches to G3cp when the succedent is $\{\bot\}$.
- The second premise of the $(L\lor)$ rule uses information from the first (as proposed by Weich).
- Once a variable $p$ or $\neg p$ is added to the antecedent, the sequent is substituted with $[\top/p]$ or $[\bot/p]$ respectively.
- Formulae are reduced with boolean simplification rules before proof attempt and during substitutions.
- Axiom ($p\Rightarrow p$) and the $(L0\to)$ rule are omitted in favour of simplification and substitution rules.
- If a rule is not applicable on a certain selection of the sequent, the selected formula is excluded in future selections for this rule.

Dyckhoff, R (1992). Contraction-free sequent calculi for intuitionistic logic. The Journal of Symbolic Logic, 57(3):795–807 [doi:10.2307/2275431](https://doi.org/10.2307/2275431)
