## 2-Level Type System
The language consists of two levels: the **object language** and the **meta language**.

The **object language** is based on classical Martin-Löf type theory with a single universe.

The **meta language** is based on a non-dependent type theory (we use the simply typed lambda calculus in this implementation) and could be extended to System-F or other polymorphic type theories if needed.

## Inter-level Interaction
$$
\newcommand{\WfObjType}[2]{{#1}\vdash {#2}~\operatorname{o-type}}
\newcommand{\WfMetaType}[2]{{#1}\vdash {#2}~\operatorname{m-type}}

\newcommand{\Inner}[2]{{#1}|{#2}}
$$

Any well-formed open type in the object language can be embedded into the meta language with its context.


$$
\frac{\WfObjType{\Delta}{A}}{\WfMetaType{\Gamma}{\Inner{\Delta}{A}}}
$$





## Inner Elements
* For any $\Delta \vdash A$, $\Delta|A$ is an outer type.
* An inner element of $\Delta | A$ is defined as a 4-tuple $(\Gamma, \Psi, \delta, a)$ where:
    - $\Gamma$ is a well-formed context (possibly containing metavariables): $\vdash \Gamma$.
    - $\Psi$ is a set of well-formed constraints under $\Gamma$: $\Gamma \vdash \Psi$.
    - $\delta$ is a substitution from $\Gamma$ to $\Delta$: $ \Gamma \vdash \delta \Rightarrow \Delta$.
    - If $\Gamma$ satisfies $\Psi$, then $a[\delta]$ is a well-formed inner term of type $A[\delta]$ in the codomain of $\delta$: $\Gamma \vdash_\Psi a[\delta] : A[\delta]$.
* The outer type $\Delta | A$ represents an inner computation that produces an element of type $A$ under context $\Delta$, but may fail.

#### Safety
For an inner element $(\Gamma, \Psi, \delta, a)$, evaluating $\delta$ is safe because it is not justified by $\Psi$, whereas evaluating $a$ is only safe when $\Psi$ is satisfied. Therefore, an inner element must represent the substitution $\delta$ and the term $a$ separately.



    

## Operations of Inner Elements

1. Constrained Map

    $$
    (\Delta|A) \to
    (\Delta\vdash \Psi) \to
    (\Delta,A\vdash_\Psi b : B) \to
    (\Delta,A | B)
    $$
