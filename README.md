# Haskell Sequent Calculus Theorem Prover

- Based on the sequent calculi m-G4ip $(Int)$ and G3cp $(Cl)$.
- m-G4ip switches to G3cp when the succedent is $\{\bot\}$.
- For the $(L\lor)$ rule, the second premise uses information from the first (as proposed by Weich).
- Once an variable $p$ or $\neg p$ is added to the antecedent, the sequent is substituted with $[\top / p]$ or $[\bot / p]$ respectively.
- Formulae are reduced with boolean simplification rules before proof attempt and during substituions.

Dyckhoff, R (1992). Contraction-free sequent calculi for intuitionistic logic. The Journal of Symbolic Logic, 57(3):795–807 [doi:10.2307/2275431](https://doi.org/10.2307/2275431)
