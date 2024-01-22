# m-G4ip, G3cp Haskell

Automated Theorem Prover for Intuitionistic Propositional Logic

Optimisations:
- m-G4ip switches to G3cp (classical logic) when the succedent is $\bot$.
- For the ($L\lor$) rule, the second premise uses information from the first (as proposed by Weich).
- For a proof rule with two premises, skips proving the second premise if it is identical to the first.

Dyckhoff, R (1992). Contraction-free sequent calculi for intuitionistic logic. The Journal of Symbolic Logic, 57(3):795–807 [doi:10.2307/2275431](https://doi.org/10.2307/2275431)
