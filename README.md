# TensorTools

[![tests](https://github.com/srossd/TensorTools/actions/workflows/tests.yml/badge.svg?branch=dev)](https://github.com/srossd/TensorTools/actions/workflows/tests.yml)

A Mathematica package for symbolic tensor manipulation. Tensors are carried
around as symbolic expressions with named, typed indices — so products,
contractions and permutations can be built up abstractly and only turned into
explicit component arrays when you ask for them. It also normal-orders products
of creation and annihilation operators.

## Installation

```mathematica
Get["https://raw.githubusercontent.com/srossd/TensorTools/main/Install.m"]
```

Then load it in any session with

```mathematica
<< TensorTools`
```

## Basics

An index type is declared with its dimension, the alphabet its names are drawn
from, and an offset into that alphabet. A `Tensor` is a list of factors, each a
name followed by its indices, every index `Raised` or `Lowered`:

```mathematica
IndexData[SO3] = Index[3, "Latin", 1];             (* a, b, c, ... running 1..3 *)

x = Tensor[{{"X", Lowered[SO3], Lowered[SO3]}}];
BuildTensor[{"X", Lowered[SO3], Lowered[SO3]}] = SparseArray@Array[xx, {3, 3}];

Indices[x]                          (* {Lowered[SO3], Lowered[SO3]} *)
Components[Contract[x, {{1, 2}}]]   (* xx[1,1] + xx[2,2] + xx[3,3] *)
```

`TensorProduct` juxtaposes factors, `Contract[t, pairs]` contracts index slots by
position, and `TensorPermute[t, perm]` reorders slots: slot `k` of `t` becomes
slot `perm[[k]]` of the result, matching `TensorTranspose`.

```mathematica
Components[TensorPermute[x, {2, 1}]]       (* the transpose of Components[x] *)
```

`Components` gives the component array in the tensor's own slot order;
`CanonicallyOrderedComponents` sorts the slots into a canonical index order
first, which is what makes tensors written different ways comparable.

> **Note.** `TensorPermute` may only permute indices **of the same type**: the
> permutation has to leave the index list unchanged, exchanging only slots that
> carry the same index type in the same raised/lowered position. Everything
> downstream identifies an index by its type, so moving one into a slot of a
> different type would rebuild the factors with the wrong signature. Doing so
> gives a `TensorPermute::indextype` warning.

## Normal ordering

Declare which symbols are annihilators (or creators), give the (anti)commutators
that do not vanish, and `NormalOrder` rewrites a product so the annihilators end
up on the right, generating the contraction terms as it goes.

```mathematica
DeclareAnnihilator["B"];

b     = Tensor[{{"B", Lowered[SO3], Lowered[SO3]}}];
bdag  = Tensor[{{"B\[Dagger]", Lowered[SO3], Lowered[SO3]}}];
delta = Tensor[{{"\[Delta]", Lowered[SO3], Lowered[SO3]}}];
BuildTensor[{"\[Delta]", Lowered[SO3], Lowered[SO3]}] = SparseArray[IdentityMatrix[3]];

(* {B_ab, Bdag_cd} = delta_ad delta_bc,  and B commutes with a delta *)
TensorAnticommutator[b, bdag]  = TensorPermute[TensorProduct[delta, delta], {1, 4, 2, 3}];
TensorAnticommutator[b, delta] = 0;

state = Contract[TensorProduct[bdag, bdag, bdag], {{2, 3}, {4, 5}, {6, 1}}];
conj  = Contract[TensorProduct[b, b, b], {{2, 3}, {4, 5}, {6, 1}}];

NormalOrder[TensorProduct[b, state]]
```

That returns four terms — one with `B` moved past all three `B†`, plus the three
contractions — and in every term the annihilator sits to the right of the
creators. An assignment to `TensorCommutator` / `TensorAnticommutator` is checked
against the indices of the pair it is assigned to, so an incommensurate
(anti)commutator is rejected rather than silently used.

Setting `"VEV" -> True` drops every term that still contains an operator, leaving
the fully contracted vacuum expectation value:

```mathematica
CanonicallyOrderedComponents[NormalOrder[TensorProduct[conj, state], "VEV" -> True]]
(* 72 *)
```

`"Vacuum" -> True` and `"ConjugateVacuum" -> True` apply the annihilator and
creator conditions separately, and `KroneckerReduce` cleans up the Kronecker
deltas left behind by the contractions.

## Symmetries

Index symmetries are declared per tensor name and used to pick a canonical
representative:

```mathematica
DeclareTensorSymmetry["S", {Symmetric[{1, 2}]}];
DeclareTensorSymmetry["F", {Antisymmetric[{1, 2}]}];

s = Tensor[{{"S", Lowered[SO3], Lowered[SO3]}}];
f = Tensor[{{"F", Lowered[SO3], Lowered[SO3]}}];

SymmetryReduce[TensorPermute[s, {2, 1}]]    (* s *)
SymmetryReduce[TensorPermute[f, {2, 1}]]    (* -f *)
```

## Tests

A regression suite lives in [`Tests/RegressionTests.wls`](Tests/RegressionTests.wls).
Run it from the repository root:

```
wolframscript -file Tests/RegressionTests.wls
```

It checks the permutation algebra against `TensorTranspose` and contractions
against `TensorContract` on the underlying arrays, the index/component coherence
invariant, the NCON round trip, `SwapFactors` / `SwapIn`, symmetry reduction,
normal ordering, and `TensorInterpret`. The script exits with a nonzero status if
any check fails.

## Repository layout

`dev` holds the development layout, with the package sources under
`TensorTools/` and the tests under `Tests/`. `main` holds the flattened,
install-ready paclet — the contents of `TensorTools/` at the root — and is built
from `dev` by CI once the tests pass.
