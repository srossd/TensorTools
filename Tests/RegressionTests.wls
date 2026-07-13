#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Regression tests for the TensorTools package.

   Run from the repository root with:

       wolframscript -file Tests/RegressionTests.wls

   The script loads the package from the working directory (not the copy
   installed under $UserBaseDirectory) so that local edits are tested.

   Two conventions shape the tests below.

   1. They are *semantic* wherever possible: rather than comparing an expression
      against a hard-coded normal form, they check the components and the index
      bookkeeping against an independent computation done directly on the
      underlying arrays (TensorTranspose / TensorContract).  Normal forms change
      whenever the internal representation is reworked -- the NCON rewrite
      invalidated most of the old UnitTest.mt expectations that way -- whereas
      the component-level identities below are what the package actually
      promises, and they are what caught the nested-TensorPermute bug.

   2. TensorPermute may only permute indices *of the same type* (see section 3).
      So the tensor used to exercise the permutation algebra, Q, carries four
      indices of one type, which makes all 24 permutations legal.  Its component
      array is filled with distinct symbols rather than numbers, so a wrong
      transposition is still caught even though every slot has the same
      dimension and no shape mismatch would give it away. *)

(* ------------------------------------------------------------------ *)
(* Load the local package                                             *)
(* ------------------------------------------------------------------ *)

repoRoot = DirectoryName[DirectoryName[$InputFileName]];
If[repoRoot === "", repoRoot = Directory[]];
PrependTo[$Path, repoRoot];
Needs["TensorTools`"];

loaded = FindFile["TensorTools`"];
Print["Loaded package from: ", loaded];
forwardSlash[p_] := StringReplace[p, "\\" -> "/"];
If[! StringContainsQ[forwardSlash[loaded], forwardSlash[repoRoot]],
  Print["ERROR: package loaded from ", loaded, ", not the repository checkout (", repoRoot,
        "). Aborting so the tests are not run against a different installed/deployed copy."];
  Exit[1]];

displayName = TensorTools`Private`DisplayName;   (* private: not on the public API *)

(* ------------------------------------------------------------------ *)
(* Minimal test harness                                               *)
(* ------------------------------------------------------------------ *)

$passed = 0;
$failed = 0;

assert[label_, cond_, info_: ""] := If[TrueQ[cond],
   $passed++; Print["  PASS  ", label],
   $failed++; Print["  FAIL  ", label, If[info === "", "", "  (" <> ToString[info, InputForm] <> ")"]]
];

(* True if evaluating expr issues the TensorPermute::indextype warning *)
SetAttributes[warnsIndexType, HoldFirst];
warnsIndexType[expr_] := TrueQ@Quiet[
   Check[expr; False, True, TensorPermute::indextype],
   TensorPermute::indextype];

(* ------------------------------------------------------------------ *)
(* Fixtures                                                           *)
(* ------------------------------------------------------------------ *)

IndexData[SpaceTime]    = Index[4, "Greek", 12];
IndexData[Spinor]       = Index[2, "Greek", 1];
IndexData[DottedSpinor] = Index[2, "Greek", 1, OverDot];
IndexData[SO3]          = Index[3, "Latin", 1];

tensorA = Tensor[{{"A", Raised[SpaceTime], Lowered[Spinor], Lowered[DottedSpinor]}}];
tensorB = Tensor[{{"B", Lowered[Spinor], Lowered[Spinor], Lowered[DottedSpinor]}}];
tensorC = Tensor[{{"C", Lowered[Spinor]}}];
tensorD = Tensor[{{"D", Lowered[Spinor], Lowered[Spinor], Lowered[Spinor], Lowered[DottedSpinor]}}];

(* Q: four indices of one type, so every permutation of its slots is legal.
   Distinct symbolic entries make any wrong transposition visible. *)
qsym = {"Q", Lowered[SO3], Lowered[SO3], Lowered[SO3], Lowered[SO3]};
tensorQ = Tensor[{qsym}];
BuildTensor[qsym] = SparseArray@Array[qq, {3, 3, 3, 3}];
arrQ = Normal[Components[tensorQ]];

(* P: two index types, interleaved.  Its legal permutations are exactly those
   that permute the two SpaceTime slots and the two Spinor slots among
   themselves; the dimensions differ (4 vs 2), so mixing them up would also
   change the shape. *)
psym = {"P", Lowered[SpaceTime], Lowered[Spinor], Lowered[SpaceTime], Lowered[Spinor]};
tensorP = Tensor[{psym}];
BuildTensor[psym] = SparseArray@Array[pp, {4, 2, 4, 2}];
arrP = Normal[Components[tensorP]];

eps = Tensor[{{"\[Epsilon]", Raised[SO3], Raised[SO3], Raised[SO3]}}];
BuildTensor[{"\[Epsilon]", Raised[SO3], Raised[SO3], Raised[SO3]}] = SparseArray@LeviCivitaTensor[3];
arrEps = Normal[Components[eps]];

perms4 = Permutations[Range[4]];

(* the permutations of t that leave its index list unchanged: the legal ones *)
legalPerms[t_] := With[{inds = Indices[t]},
   Select[Permutations[Range[Length[inds]]], inds[[InversePermutation[#]]] === inds &]];

comps[e_] := Normal[Components[e]];
idxTypes[e_] := Indices[e] /. (Raised | Lowered)[i_] :> i;
claimedDims[e_] := IndexData[#][[1]] & /@ idxTypes[e];

(* ------------------------------------------------------------------ *)
(* 1. Structure: Symbolic / Indices / TensorPermutation               *)
(* ------------------------------------------------------------------ *)

Print["=== Structure ==="];

assert["Tensor[{}] == 1", Tensor[{}] === 1];

assert["TensorProduct of two Tensors merges the symbol lists",
   TensorProduct[tensorA, tensorB] === Tensor[{
      {"A", Raised[SpaceTime], Lowered[Spinor], Lowered[DottedSpinor]},
      {"B", Lowered[Spinor], Lowered[Spinor], Lowered[DottedSpinor]}}]];

assert["Symbolic",
   {Symbolic[tensorA], Symbolic[tensorB]} === {
      {{"A", Raised[SpaceTime], Lowered[Spinor], Lowered[DottedSpinor]}},
      {{"B", Lowered[Spinor], Lowered[Spinor], Lowered[DottedSpinor]}}}];

assert["Indices",
   {Indices[tensorA], Indices[tensorB]} === {
      {Raised[SpaceTime], Lowered[Spinor], Lowered[DottedSpinor]},
      {Lowered[Spinor], Lowered[Spinor], Lowered[DottedSpinor]}}];

assert["TensorPermutation of a bare Tensor is the identity",
   {TensorPermutation[tensorA], TensorPermutation[tensorB]} === {{1, 2, 3}, {1, 2, 3}}];

(* DisplayName returns the letter wrapped in HoldForm.  Note the 5th SpaceTime
   index is HoldForm[Pi], not HoldForm[\[Pi]]: DisplayName builds the name with
   ToExpression, which parses the Greek letter \[Pi] as the built-in symbol Pi.
   It still renders as \[Pi] in TraditionalForm, so this is cosmetic -- but it
   does mean the names are not always the symbol you would expect. *)
assert["DisplayName numbers indices from the declared alphabet and offset",
   Table[displayName[idxtype, i], {idxtype, {SpaceTime, Spinor, SO3}}, {i, 5}] === {
      {HoldForm[\[Mu]], HoldForm[\[Nu]], HoldForm[\[Xi]], HoldForm[\[Omicron]], HoldForm[Pi]},
      {HoldForm[\[Alpha]], HoldForm[\[Beta]], HoldForm[\[Gamma]], HoldForm[\[Delta]], HoldForm[\[CurlyEpsilon]]},
      {HoldForm[a], HoldForm[b], HoldForm[c], HoldForm[d], HoldForm[e]}}];

(* ------------------------------------------------------------------ *)
(* 2. Index/component coherence                                       *)
(* ------------------------------------------------------------------ *)

(* The central invariant: the index list and the component array describe the
   same tensor, slot for slot.  Every rewrite rule must preserve it, and the
   nested-TensorPermute bug broke it. *)

Print["=== Index / component coherence ==="];

coherent[label_, e_] := assert["coherent: " <> label,
   Dimensions[Components[e]] === claimedDims[e],
   {Dimensions[Components[e]], claimedDims[e]}];

coherent["Tensor", tensorQ];
coherent["TensorPermute", TensorPermute[tensorQ, {2, 3, 4, 1}]];
coherent["nested TensorPermute", TensorPermute[TensorPermute[tensorQ, {2, 3, 4, 1}], {3, 1, 4, 2}]];
coherent["TensorPermute, two index types", TensorPermute[tensorP, {3, 4, 1, 2}]];
coherent["TensorProduct", TensorProduct[tensorQ, eps]];
coherent["TensorProduct with a permuted factor",
   TensorProduct[eps, TensorPermute[tensorQ, {2, 3, 4, 1}]]];
coherent["Contract", Contract[TensorProduct[eps, eps], {{1, 4}}]];
coherent["Contract of a TensorPermute",
   Contract[TensorPermute[TensorProduct[eps, eps], {6, 5, 4, 3, 2, 1}], {{1, 4}}]];

assert["coherence holds for every permutation of Q",
   AllTrue[perms4, Dimensions[Components[TensorPermute[tensorQ, #]]] === claimedDims[TensorPermute[tensorQ, #]] &]];

(* ------------------------------------------------------------------ *)
(* 3. TensorPermute: the contract                                     *)
(* ------------------------------------------------------------------ *)

(* TensorPermute may only permute indices of the same type: perm must leave the
   index list unchanged, exchanging only slots carrying the same index type in
   the same raised/lowered position.  Everything downstream (the NCON form, and
   so Contract / SwapFactors / SwapIn) identifies an index by its type and
   rebuilds each factor's signature from the permuted slots, so moving an index
   into a slot of a different type rebuilds the factors wrongly.  Out-of-contract
   permutations are warned about, not silently accepted. *)

Print["=== TensorPermute: only same-type indices may be permuted ==="];

assert["every permutation of Q (one index type) is legal",
   Length[legalPerms[tensorQ]] === 24];

assert["only the type-preserving permutations of P are legal",
   Sort[legalPerms[tensorP]] === Sort[{{1, 2, 3, 4}, {1, 4, 3, 2}, {3, 2, 1, 4}, {3, 4, 1, 2}}],
   legalPerms[tensorP]];

assert["a legal permutation leaves the index list unchanged",
   AllTrue[legalPerms[tensorP], Indices[TensorPermute[tensorP, #]] === Indices[tensorP] &]];

assert["a legal permutation is silent",
   ! warnsIndexType[TensorPermute[tensorP, {3, 4, 1, 2}]]];

assert["permuting an index into a slot of another type warns",
   warnsIndexType[TensorPermute[tensorP, {2, 1, 3, 4}]]];

assert["swapping a raised index with a lowered one of the same type warns",
   warnsIndexType[TensorPermute[Tensor[{{"R", Raised[SO3], Lowered[SO3]}}], {2, 1}]]];

assert["the warning also fires when a bad permutation is reached by nesting",
   warnsIndexType[TensorPermute[TensorPermute[tensorP, {3, 4, 1, 2}], {2, 1, 3, 4}]]];

assert["a warned-about permutation still evaluates (its Components are defined)",
   Quiet[comps[TensorPermute[tensorP, {2, 1, 3, 4}]], TensorPermute::indextype]
      === Transpose[arrP, {2, 1, 3, 4}]];

(* ------------------------------------------------------------------ *)
(* 4. TensorPermute algebra                                           *)
(* ------------------------------------------------------------------ *)

(* TensorPermute[t, perm] is *defined* by its Components: it is
   TensorTranspose[Components[t], perm], i.e. slot k of t becomes slot perm[[k]]
   of the result.  Everything below is checked against that definition, applied
   directly to the underlying array. *)

Print["=== TensorPermute algebra ==="];

assert["the identity permutation is a no-op",
   TensorPermute[tensorQ, {1, 2, 3, 4}] === tensorQ];

assert["TensorPermute[0, perm] == 0", TensorPermute[0, {2, 1}] === 0];

assert["scalars factor out",
   TensorPermute[7 tensorQ, {2, 3, 4, 1}] === 7 TensorPermute[tensorQ, {2, 3, 4, 1}]];

assert["TensorPermute is linear",
   TensorPermute[tensorQ + tensorQ, {2, 3, 4, 1}] === 2 TensorPermute[tensorQ, {2, 3, 4, 1}]];

assert["a single permute transposes the components (all 24)",
   AllTrue[perms4, comps[TensorPermute[tensorQ, #]] === Transpose[arrQ, #] &]];

(* --- the regression: nesting must compose the permutations as p2[[p1]] --- *)

(* Worked example, spelled out so the composite is human-checkable.  Slot 1 of Q
   goes to position 2 under p1, and position 2 goes to position 1 under p2, so
   slot 1 ends up at position 1: the composite starts with 1.  Composing the
   other way round would give {4,2,1,3}, which is the bug this guards. *)
assert["nested TensorPermute composes as p2[[p1]] (worked example)",
   TensorPermute[TensorPermute[tensorQ, {2, 3, 4, 1}], {3, 1, 4, 2}]
      === TensorPermute[tensorQ, {1, 4, 2, 3}],
   TensorPermute[TensorPermute[tensorQ, {2, 3, 4, 1}], {3, 1, 4, 2}]];

(* The two composition orders disagree for 456 of these 576 pairs; the 120 that
   agree are the commuting ones.  p and InversePermutation[p] always commute,
   which is why a permute/unpermute round trip never caught this. *)
assert["nested TensorPermute == two successive transposes (all 576 pairs)",
   AllTrue[Tuples[{perms4, perms4}],
      comps[TensorPermute[TensorPermute[tensorQ, #[[1]]], #[[2]]]]
         === Transpose[Transpose[arrQ, #[[1]]], #[[2]]] &]];

assert["the two composition orders really do differ (so the test above has teeth)",
   Length[Select[Tuples[{perms4, perms4}], #[[1]][[#[[2]]]] =!= #[[2]][[#[[1]]]] &]] === 456];

SeedRandom[7];
assert["three-fold nesting is associative (300 random triples)",
   AllTrue[Table[RandomChoice[perms4, 3], {300}],
      comps[TensorPermute[TensorPermute[TensorPermute[tensorQ, #[[1]]], #[[2]]], #[[3]]]]
         === Transpose[Transpose[Transpose[arrQ, #[[1]]], #[[2]]], #[[3]]] &]];

assert["permuting by p then by InversePermutation[p] returns the original",
   AllTrue[perms4, TensorPermute[TensorPermute[tensorQ, #], InversePermutation[#]] === tensorQ &]];

(* the same, for a tensor with two index types, over its legal permutations *)
assert["nesting composes correctly for a two-index-type tensor too",
   AllTrue[Tuples[{legalPerms[tensorP], legalPerms[tensorP]}],
      comps[TensorPermute[TensorPermute[tensorP, #[[1]]], #[[2]]]]
         === Transpose[Transpose[arrP, #[[1]]], #[[2]]] &]];

(* --- TensorPermutation reports where each underlying slot ended up --- *)

(* TensorPermutation[e][[k]] is the position, in e, of slot k of the underlying
   symbolic tensor product -- the same convention as TensorPermute's perm
   argument.  DeleteFactor and deltas[] both rely on reading it that way. *)
assert["TensorPermutation locates each underlying slot (all 24)",
   AllTrue[perms4,
      With[{e = TensorPermute[tensorQ, #]},
         Indices[e][[TensorPermutation[e]]] === Indices[Tensor[Symbolic[e]]]] &]];

assert["TensorPermutation of a permuted tensor is the permutation itself",
   AllTrue[perms4, TensorPermutation[TensorPermute[tensorQ, #]] === # &]];

(* ------------------------------------------------------------------ *)
(* 5. TensorProduct with permuted factors                             *)
(* ------------------------------------------------------------------ *)

(* TensorTools.m pulls a TensorPermute out through a TensorProduct by shifting
   the permutation.  Check the result against the arrays. *)

Print["=== TensorProduct with permuted factors ==="];

assert["TensorProduct[Tensor, TensorPermute]",
   comps[TensorProduct[eps, TensorPermute[tensorQ, {2, 3, 4, 1}]]]
      === TensorProduct[arrEps, Transpose[arrQ, {2, 3, 4, 1}]]];

assert["TensorProduct[TensorPermute, Tensor]",
   comps[TensorProduct[TensorPermute[tensorQ, {2, 3, 4, 1}], eps]]
      === TensorProduct[Transpose[arrQ, {2, 3, 4, 1}], arrEps]];

assert["TensorProduct[TensorPermute, TensorPermute]",
   comps[TensorProduct[TensorPermute[tensorQ, {2, 3, 4, 1}], TensorPermute[eps, {3, 1, 2}]]]
      === TensorProduct[Transpose[arrQ, {2, 3, 4, 1}], Transpose[arrEps, {3, 1, 2}]]];

(* ------------------------------------------------------------------ *)
(* 6. Contract                                                        *)
(* ------------------------------------------------------------------ *)

Print["=== Contract ==="];

assert["Contract[t, {}] is a no-op", Contract[tensorQ, {}] === tensorQ];

assert["fully contracted epsilon network evaluates to -6",
   Components@Contract[TensorProduct[eps, eps, eps, eps],
      {{1, 4}, {2, 7}, {3, 10}, {5, 8}, {9, 11}, {12, 6}}] === -6];

assert["Contract matches TensorContract on the components",
   comps[Contract[TensorProduct[eps, eps], {{1, 4}}]]
      === TensorContract[TensorProduct[arrEps, arrEps], {{1, 4}}]];

(* Contract[TensorPermute[...]] is rewritten through NCON/FromNCON, which is
   where a permutation can easily get inverted. *)
assert["Contract of a TensorPermute matches TensorContract of the transposed components",
   With[{p = {6, 5, 4, 3, 2, 1}, pairs = {{1, 4}}},
      comps[Contract[TensorPermute[TensorProduct[eps, eps], p], pairs]]
         === TensorContract[Transpose[TensorProduct[arrEps, arrEps], p], pairs]]];

assert["Contract of a TensorPermute, over every legal permutation of Q",
   AllTrue[perms4,
      comps[Contract[TensorPermute[tensorQ, #], {{1, 2}}]]
         === TensorContract[Transpose[arrQ, #], {{1, 2}}] &]];

assert["nested Contract accumulates the pairs",
   comps[Contract[Contract[TensorProduct[eps, eps], {{1, 4}}], {{1, 3}}]]
      === TensorContract[TensorContract[TensorProduct[arrEps, arrEps], {{1, 4}}], {{1, 3}}]];

(* ------------------------------------------------------------------ *)
(* 7. CanonicallyOrderedComponents                                    *)
(* ------------------------------------------------------------------ *)

Print["=== CanonicallyOrderedComponents ==="];

assert["COC of an already-canonical tensor is just its components",
   Normal[CanonicallyOrderedComponents[tensorQ]] === arrQ];

assert["COC sorts the slots into canonical index order",
   Normal[CanonicallyOrderedComponents[tensorP]]
      === TensorTranspose[arrP, InversePermutation[Ordering[Indices[tensorP]]]]];

assert["COC of a scalar multiple factors out",
   Normal[CanonicallyOrderedComponents[3 tensorQ]] === 3 arrQ];

(* ------------------------------------------------------------------ *)
(* 8. SwapFactors                                                     *)
(* ------------------------------------------------------------------ *)

(* SwapFactors reorders the *factors* of a tensor product.  The result is the
   same abstract tensor, but not necessarily with its free slots in the same
   order: the free index sequence follows the new factor order.  So the contract
   is that the abstract tensor is unchanged -- its canonically ordered
   components agree -- and that the result stays coherent. *)

Print["=== SwapFactors ==="];

swapPreserves[label_, e_, spec__] := Module[{s = SwapFactors[e, spec]},
   assert["SwapFactors preserves the abstract tensor: " <> label,
      Normal[CanonicallyOrderedComponents[s]] === Normal[CanonicallyOrderedComponents[e]]];
   assert["SwapFactors returns a coherent tensor: " <> label,
      Dimensions[Components[s]] === claimedDims[s], {Dimensions[Components[s]], claimedDims[s]}];
   assert["SwapFactors preserves the multiset of free indices: " <> label,
      Sort[Indices[s]] === Sort[Indices[e]]];
   s];

swapPreserves["adjacent transposition of 2 factors", TensorProduct[tensorQ, eps], 1, 2];
swapPreserves["reversal of 4 factors", TensorProduct[eps, tensorQ, eps, tensorQ], {4, 3, 2, 1}];
swapPreserves["contracted network", Contract[TensorProduct[eps, eps], {{1, 4}}], 1, 2];
swapPreserves["already-permuted tensor", TensorPermute[TensorProduct[tensorQ, eps], {4, 3, 2, 1, 5, 6, 7}], 1, 2];

assert["SwapFactors reorders the symbol list",
   Symbolic[SwapFactors[TensorProduct[tensorA, tensorB], 1, 2]]
      === Reverse[Symbolic[TensorProduct[tensorA, tensorB]]]];

(* ------------------------------------------------------------------ *)
(* 9. SwapIn                                                          *)
(* ------------------------------------------------------------------ *)

Print["=== SwapIn ==="];

assert["SwapIn replaces a factor range with a commensurate tensor",
   SwapIn[TensorProduct[tensorC, tensorB], {1, 2}, tensorD] === tensorD];

assert["SwapIn through a TensorPermute keeps the outer permutation",
   SwapIn[TensorPermute[TensorProduct[tensorC, tensorB], {2, 1, 3, 4}], {1, 2}, tensorD]
      === TensorPermute[tensorD, {2, 1, 3, 4}]];

assert["SwapIn[0] == 0", SwapIn[TensorProduct[tensorC, tensorB], {1, 2}, 0] === 0];

assert["SwapIn pulls out scalar factors",
   SwapIn[TensorProduct[tensorC, tensorB], {1, 2}, 5 tensorD] === 5 tensorD];

(* SwapIn must be additive in the replacement: NormalOrder feeds it sums. *)
assert["SwapIn is additive in the replacement",
   SwapIn[TensorProduct[tensorC, tensorB], {1, 2}, tensorD + 2 tensorD] === 3 tensorD];

(* ------------------------------------------------------------------ *)
(* 10. Symmetries                                                     *)
(* ------------------------------------------------------------------ *)

Print["=== Symmetries ==="];

symSym  = {"S", Lowered[SO3], Lowered[SO3]};
asymSym = {"F", Lowered[SO3], Lowered[SO3]};
tensorS = Tensor[{symSym}];
tensorF = Tensor[{asymSym}];
DeclareTensorSymmetry["S", {Symmetric[{1, 2}]}];
DeclareTensorSymmetry["F", {Antisymmetric[{1, 2}]}];
BuildTensor[symSym]  = SparseArray[Table[ss[Min[i, j], Max[i, j]], {i, 3}, {j, 3}]];
BuildTensor[asymSym] = SparseArray[Table[Sign[j - i] ff[Min[i, j], Max[i, j]], {i, 3}, {j, 3}]];

assert["TensorSymmetries reads the declaration",
   TensorSymmetries[tensorS] === {Symmetric[{1, 2}]}];

assert["SymmetryReduce sends a symmetric tensor's transpose back to itself",
   SymmetryReduce[TensorPermute[tensorS, {2, 1}]] === tensorS];

assert["SymmetryReduce picks up the sign for an antisymmetric tensor",
   SymmetryReduce[TensorPermute[tensorF, {2, 1}]] === -tensorF];

assert["SymmetryReduce preserves the components (symmetric)",
   Normal[CanonicallyOrderedComponents[SymmetryReduce[TensorPermute[tensorS, {2, 1}]]]]
      === Normal[CanonicallyOrderedComponents[TensorPermute[tensorS, {2, 1}]]]];

assert["SymmetryReduce preserves the components (antisymmetric)",
   Normal[CanonicallyOrderedComponents[SymmetryReduce[TensorPermute[tensorF, {2, 1}]]]]
      === Normal[CanonicallyOrderedComponents[TensorPermute[tensorF, {2, 1}]]]];

assert["TensorSymmetries of a TensorPermute moves the symmetric slots",
   TensorSymmetries[TensorPermute[TensorProduct[tensorS, tensorF], {3, 4, 1, 2}]]
      === {Symmetric[{3, 4}], Antisymmetric[{1, 2}]}];

(* ------------------------------------------------------------------ *)
(* 11. NCON round trip                                                *)
(* ------------------------------------------------------------------ *)

(* NCON is the internal normal form that Contract[TensorPermute[...]],
   SwapFactors and SwapIn all route through.  FromNCON[NCON[e]] must give back
   the same abstract tensor. *)

Print["=== NCON round trip ==="];

nconRoundTrip[label_, e_] := Module[{r = FromNCON[NCON[e]]},
   assert["NCON round trip preserves the indices: " <> label, Indices[r] === Indices[e], {Indices[r], Indices[e]}];
   assert["NCON round trip preserves the components: " <> label, comps[r] === comps[e]]];

nconRoundTrip["Tensor", tensorQ];
nconRoundTrip["TensorProduct", TensorProduct[eps, tensorQ]];
nconRoundTrip["TensorPermute", TensorPermute[tensorQ, {2, 3, 4, 1}]];
nconRoundTrip["TensorPermute, two index types", TensorPermute[tensorP, {3, 4, 1, 2}]];
nconRoundTrip["Contract", Contract[TensorProduct[eps, eps], {{1, 4}}]];
nconRoundTrip["Contract of a TensorPermute",
   Contract[TensorPermute[TensorProduct[eps, eps], {6, 5, 4, 3, 2, 1}], {{1, 4}}]];

(* ------------------------------------------------------------------ *)
(* 12. NormalOrder                                                    *)
(* ------------------------------------------------------------------ *)

(* These use TensorCommutator / TensorAnticommutator.  The old test suite used
   Commutator / Anticommutator, which are now *protected System symbols*, so its
   definitions silently failed to take and NormalOrder quietly became a no-op. *)

Print["=== NormalOrder ==="];

DeclareAnnihilator["B"];
opB     = Tensor[{{"B", Lowered[SO3], Lowered[SO3]}}];
opBdag  = Tensor[{{"Bdag", Lowered[SO3], Lowered[SO3]}}];
opDelta = Tensor[{{"\[Delta]", Lowered[SO3], Lowered[SO3]}}];
BuildTensor[{"\[Delta]", Lowered[SO3], Lowered[SO3]}] = SparseArray[IdentityMatrix[3]];

TensorAnticommutator[opB, opBdag]  = TensorPermute[TensorProduct[opDelta, opDelta], {1, 4, 2, 3}];
TensorAnticommutator[opB, opDelta] = 0;

state = Contract[TensorProduct[opBdag, opBdag, opBdag], {{2, 3}, {4, 5}, {6, 1}}];
conj  = Contract[TensorProduct[opB, opB, opB], {{2, 3}, {4, 5}, {6, 1}}];

no = NormalOrder[TensorProduct[opB, state]];

assert["NormalOrder[B . state] expands into 4 terms", Length[no] === 4, Length[no]];

orderedQ[t_] := ! MatchQ[Symbolic[t][[;; , 1]], {___, "B", "Bdag", ___}];
assert["every term of NormalOrder[B . state] is normal ordered",
   AllTrue[List @@ no, orderedQ[# /. n_?NumericQ x_ :> x] &]];

assert["NormalOrder leaves an already-ordered expression alone",
   NormalOrder[state] === state];

assert["a term ending in an annihilator is killed by the vacuum",
   NormalOrder[TensorProduct[state, opB], "Vacuum" -> True] === 0];

assert["the fully contracted VEV <conj|state> evaluates to 72",
   CanonicallyOrderedComponents[NormalOrder[TensorProduct[conj, state], "VEV" -> True]] === 72];

(* --- assigning a (anti)commutator validates that it carries the right indices --- *)

(* The guard on these Set rules was inverted ($vg... is only ever True *inside*
   the Block), so the check never ran.  With it fixed, the accepted cases below
   must keep working: a vanishing (anti)commutator, and a sum. *)

assert["a vanishing anticommutator is accepted",
   TensorAnticommutator[opB, opDelta] === 0];

assert["an anticommutator with the right indices is accepted",
   Head[TensorAnticommutator[opB, opBdag]] =!= TensorAnticommutator];

opU = Tensor[{{"U", Lowered[SO3], Lowered[SO3]}}];
opV = Tensor[{{"V", Lowered[SO3], Lowered[SO3]}}];
opW = Tensor[{{"W", Lowered[SO3]}}];

(* both terms carry the same four indices as [U, V] *)
TensorCommutator[opU, opV] = TensorPermute[TensorProduct[opDelta, opDelta], {1, 4, 2, 3}]
   + TensorProduct[opDelta, opDelta];
assert["a sum is accepted when every term carries the right indices",
   Head[TensorCommutator[opU, opV]] =!= TensorCommutator];

assert["an incommensurate anticommutator is rejected with a message",
   TrueQ@Quiet[
      Check[TensorAnticommutator[opU, opW] = opDelta; False, True,
         TensorAnticommutator::incommensurate],
      TensorAnticommutator::incommensurate]];

assert["...and is not assigned",
   Head[TensorAnticommutator[opU, opW]] === TensorAnticommutator];

(* ------------------------------------------------------------------ *)
(* 13. KroneckerReduce                                                *)
(* ------------------------------------------------------------------ *)

Print["=== KroneckerReduce ==="];

tensorX = Tensor[{{"X", Raised[SO3], Lowered[SO3]}}];
BuildTensor[{"X", Raised[SO3], Lowered[SO3]}] = SparseArray@Array[xx, {3, 3}];

withDelta = Contract[TensorProduct[Kronecker[SO3], tensorX], {{2, 3}}];
reduced   = KroneckerReduce[withDelta];

assert["KroneckerReduce removes the Kronecker delta",
   FreeQ[Symbolic[reduced], "\[Delta]"], Symbolic[reduced]];
assert["KroneckerReduce preserves the components",
   comps[reduced] === comps[withDelta]];
assert["a delta contracted with itself gives the dimension",
   Components[Contract[Kronecker[SO3], {{1, 2}}]] === 3];

(* ------------------------------------------------------------------ *)
(* 14. TensorInterpret                                                *)
(* ------------------------------------------------------------------ *)

(* TensorInterpret reads index notation: positive integers are free slots (in
   the written order), repeated negative integers are contracted pairs. *)

Print["=== TensorInterpret ==="];

assert["TensorInterpret[X[1,2]] is X itself",
   TensorInterpret[tensorX[1, 2]] === tensorX];

assert["TensorInterpret contracts repeated negative indices",
   comps[TensorInterpret[tensorX[1, -1] tensorX[-1, 2]]] === comps[tensorX] . comps[tensorX]];

(* the Set form assigns the components of a new primitive tensor *)
tensorX2 = Tensor[{{"X2", Raised[SO3], Lowered[SO3]}}];
TensorInterpret[tensorX2[1, 2] = tensorX[1, -1] tensorX[-1, 2]];
assert["TensorInterpret[lhs = rhs] defines the components of lhs",
   comps[tensorX2] === comps[tensorX] . comps[tensorX]];

(* Writing X[2,1] asks for a raised index and a lowered one to trade places,
   which is outside TensorPermute's contract; it must warn rather than quietly
   produce something Contract would mis-handle. *)
assert["TensorInterpret[X[2,1]] warns: it would swap a raised index with a lowered one",
   warnsIndexType[TensorInterpret[tensorX[2, 1]]]];

(* ------------------------------------------------------------------ *)
(* Summary                                                            *)
(* ------------------------------------------------------------------ *)

Print["================================================================"];
Print[ToString[$passed], " passed, ", ToString[$failed], " failed."];
If[$failed > 0, Exit[1], Exit[0]];
