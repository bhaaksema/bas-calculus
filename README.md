# Haskell Sequent Calculus Theorem Prover

- Based on the sequent calculi m-G4ip $(Int)$ and G3cp $(Cl)$.
- m-G4ip switches to G3cp when the succedent is $\{\bot\}$.
- Invertible $(L\to)$ rules of m-G4ip are omitted in favour of substitution rules.
- The second premise of the $(L\lor)$ rule uses information from the first (as proposed by Weich).
- Once a variable $p$ or $\neg p$ is added to the antecedent, the sequent is substituted with $[\top/p]$ or $[\bot/p]$ respectively.
- Formulae are reduced with boolean simplification rules before proof attempt and during substitutions.

Dyckhoff, R (1992). Contraction-free sequent calculi for intuitionistic logic. The Journal of Symbolic Logic, 57(3):795–807 [doi:10.2307/2275431](https://doi.org/10.2307/2275431)
