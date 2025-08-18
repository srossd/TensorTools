(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Mar 6, 2023 *)

BeginPackage["TensorTools`"]
(* Exported symbols added here with SymbolName::usage *) 

Begin["`Private`"]
(* Implementation of the package *)

$TensorHeads = {Tensor, TensorPermute, Contract, TP};
AddTensorHead[head_Symbol] := If[!MemberQ[$TensorHeads, head], AppendTo[$TensorHeads, head]];

Index[dim_, alphabet_] := Index[dim, alphabet, 1];
Index[dim_, alphabet_, offset_] := Index[dim, alphabet, offset, Identity];
Unprotect[Set];
Set[IndexData[idxtype_], i_Index] /; !TrueQ[$vgIndexData] := Block[{$vgIndexData = True}, idxtype = idxtype; IndexData[idxtype] = i;];
Protect[Set];

DisplayName[Raised[idx_], ct_] := DisplayName[idx, ct];
DisplayName[Lowered[idx_], ct_] := DisplayName[idx, ct];
DisplayName[idx_, ct_] := 
  With[{data = IndexData[idx]}, With[{s = data[[4]][Alphabet[data[[2]]][[ct + data[[3]] - 1]]]}, If[!StringQ[s], s, ToExpression[s, TraditionalForm, HoldForm] /. Null -> ""]]];

DisplayTemplate[Tensor[names_]] := DisplayTemplate[names];

concat[a : Except[_Row], b_] := Row[{a, b}];
concat[Row[{stuff___}], b_] := Row[{stuff, b}];
sub[a : Except[_Row], b_] := Subscript[a, b];
sub[Row[{stuff___, last_}], b_] := Row[{stuff, sub[last, b]}];
sup[a : Except[_Row], b_] := Superscript[a, b];
sup[Row[{stuff___, last_}], b_] := Row[{stuff, sup[last, b]}];
DisplayTemplate[names_List] := Block[{lowercts, ctr, ii},
   lowercts = Counts[Cases[names, _Lowered, All]];
   ctr[_] = 0;
   ii = 1;
   Fold[Which[MatchQ[#2, _Lowered], sub, MatchQ[#2, _Raised], sup, 
         True, concat][#1, 
        Which[
           MatchQ[#2, _Lowered], 
           dn[ii++, #2, ctr[#2] = ctr[#2] + 1],
           MatchQ[#2, _Raised],
           dn[ii++, #2, If[KeyExistsQ[lowercts, Lowered @@ #2], lowercts[Lowered @@ #2], 0] + (ctr[#2] = ctr[#2] + 1)],           
           True, #2
         ]] &, 
      Replace[Flatten[names], expr_?StringQ :> Quiet[Check[ToExpression[expr, TraditionalForm, HoldForm], expr]], {1}]] //. {Subscript[Subscript[a_, b_], c_] :> 
       Subscript[a, Row[{b, c}]], 
      Superscript[Superscript[a_, b_], c_] :> 
       Superscript[a, Row[{b, c}]]} /. 
    Row[{x___, f_, "(", inner___, ")", y___}] :> 
     Row[{x, f[Row[{inner}]], y}]
   ];
   
Format[t_Tensor, TraditionalForm] := 
  DisplayTemplate[t] /. 
   dn[_, a_, b_] | adn[_, a_, b_] :> DisplayName[a, b];
Symbolic[Tensor[names_]] := names;
Symbolic[a_ b_] /; FreeQ[a, Alternatives @@ $TensorHeads] := 
  Symbolic[b];
InactiveComponents[Tensor[{}]] := 1;
InactiveComponents[Tensor[{name_}]] := BuildTensor[{name}];
InactiveComponents[Tensor[names_]] /; Length[names] > 1 := 
  TP @@ (BuildTensor /@ names);
Indices[Tensor[names_]] := Join @@ (Rest /@ names);
TensorPermutation[t_Tensor] := Range@Length[Indices[t]];
TensorPermutation[a_ b_] /; FreeQ[a, Alternatives @@ $TensorHeads] := TensorPermutation[b];
TensorPermutation[a_] /; FreeQ[a, Alternatives @@ $TensorHeads] := {};

ContractedPairs[t_Tensor] := {};

Indices[a_ b_] /; FreeQ[a, Alternatives @@ $TensorHeads] := Indices[b];
Indices[a_] /; FreeQ[a, Alternatives @@ $TensorHeads] := {};

SetAttributes[TP, Flat];
Components[t_] := 
 Activate[
   InactiveComponents[t /. TensorProduct -> Distribute@*TP] /. Times -> Inactive[Times] /. 
    TP -> Inactive[TensorProduct] /. TensorProduct -> TP //. 
  	  Inactive[TP][x__, y_, z___] | Inactive[TP][x___, y_, z__] /; ! ArrayQ[y] && !ListQ[y] && FreeQ[y, Alternatives @@ $TensorHeads] :> Inactive[Times][y, TP[x, z]] /. 
        TP -> Inactive[TensorProduct]]

If[Head[explicitRules] === Symbol, explicitRules = {}];
AddExplicitRule[rule_] := AppendTo[explicitRules, rule];
Explicit[x_] := x //. explicitRules;
InactiveComponents[a_ b_] /; FreeQ[a, Alternatives @@ $TensorHeads] := 
  Explicit[a] InactiveComponents[b];
InactiveComponents[a_] /; FreeQ[a, Alternatives @@ $TensorHeads] := 
  Explicit[a];
  
Quiet[Unprotect[TensorProduct];
TensorProduct[y___, a_ b_, c_, z___] /; FreeQ[a, Alternatives @@ $TensorHeads] := 
  a TensorProduct[b, c];
TensorProduct[y___, a_, b_ c_, z___] /; FreeQ[b, Alternatives @@ $TensorHeads] := 
  b TensorProduct[a, c];
Format[t_TensorProduct, TraditionalForm] /; ! FreeQ[t, Alternatives @@ $TensorHeads] := 
  DisplayTemplate[t] /. 
   dn[_, a_, b_] | adn[_, a_, b_] :> DisplayName[a, b];
t : TensorProduct[x___, y_Plus, z___] := 
  Activate[Distribute[Inactive[TensorProduct][x, y, z]]];
(*TensorProduct[x___,y_,z___]/;!ArrayQ[y]&&FreeQ[y,Tensor]:=y \
TensorProduct[x,z];*)
Protect[TensorProduct];];

Unprotect[TensorProduct];
TensorProduct[y___, a_ b_, c_, z___] /; FreeQ[a, Alternatives @@ $TensorHeads] := 
  a TensorProduct[y, b, c, z];
TensorProduct[y___, a_, b_ c_, z___] /; FreeQ[b, Alternatives @@ $TensorHeads] := 
  b TensorProduct[y, a, c, z];
Format[t_TensorProduct, TraditionalForm] /; ! FreeQ[t, Alternatives @@ $TensorHeads] := 
  DisplayTemplate[t] /. 
   dn[_, a_, b_] | adn[_, a_, b_] :> DisplayName[a, b];
t : TensorProduct[x___, y_Plus, z___] := 
  Activate[Distribute[Inactive[TensorProduct][x, y, z]]];
(*TensorProduct[x___,y_,z___]/;!ArrayQ[y]&&FreeQ[y,Tensor]:=y \
TensorProduct[x,z];*)
Protect[TensorProduct];

Tensor /: TensorProduct[t1_Tensor, t2_Tensor] := 
  Tensor[Join[Symbolic[t1], Symbolic[t2]]];
Symbolic[TensorProduct[t1_, t2_]] := 
  Join[Symbolic[t1], Symbolic[t2]];
Indices[tp_TensorProduct] := Join @@ (Indices /@ (List @@ tp));
TensorPermutation[TP[t : Except[_TP]]] := TensorPermutation[t];
TensorPermutation[TensorProduct[t1_, t2__]] := 
  Join[TensorPermutation[t1], 
   TensorPermutation[TensorProduct[t2]] + Length[TensorPermutation[t1]]];
TensorPermutation[TP[t1_, t2__]] := 
  Join[TensorPermutation[t1], TensorPermutation[TP[t2]] + Length[TensorPermutation[t1]]];
InactiveComponents[TensorProduct[t1_, t2_]] := 
  TP @@ (InactiveComponents /@ {t1, t2});
DisplayTemplate[tp : TensorProduct[t1_, t2_]] := 
  DisplayTemplate[Symbolic[tp]];
  
ClearAll[Contract];
reindex[template_] := 
  template /. 
    dn[i_, type_, j_] :> 
     dn[i, type, 
     If[Head[type] === Raised, Length@Cases[template, dn[_, Lowered @@ type, _], All, Heads -> True], 0] + 
      Position[
        SortBy[Cases[template, dn[m_, type, n_] :> {m, n}, All, 
           Heads -> True], First][[;; , 2]], j][[1, 1]]] /. 
   adn[i_, type_, j_] :> 
    adn[i, type, 
     Length@Cases[template, dn[_, _[type[[1]]], _], All, Heads -> True] + 
      Position[
        SortBy[DeleteDuplicates@
           Cases[template, adn[m_, _[type[[1]]], n_] :> {m, n}, All, 
            Heads -> True], First][[;; , 2]], j][[1, 1]]];
DisplayTemplate[Contract[t_, pairs_, OptionsPattern[]]] := 
  Module[{template = DisplayTemplate[t], dns, dnsreplace},
   dns = SortBy[Cases[template, _dn, All, Heads -> True], First];
   dnsreplace = dns;
   dnsreplace[[Max /@ pairs]] = dns[[Min /@ pairs]];
   dnsreplace[[Flatten[pairs]]] = 
    Style[#, Lighter@Gray] & /@ dnsreplace[[Flatten[pairs]]] /. 
     dn -> adn;
   reindex[template /. Thread[dns -> dnsreplace]]
   ];
Format[t_Contract, TraditionalForm] := 
  DisplayTemplate[t] /. 
   dn[_, a_, b_] | adn[_, a_, b_] :> DisplayName[a, b];

shift[n_, vals_] := 
  If[n < Min[vals], 0, 1 + shift[n + 1, DeleteCases[vals, Min[vals]]]];
Options[Contract] = {"Shift" -> True};
Contract[0, _, OptionsPattern[]] := 0;
Contract[a_ t_, pairs_, opt : OptionsPattern[]] /; FreeQ[a, Alternatives @@ $TensorHeads] :=
   a Contract[t, pairs, opt];
Contract[a_ + b_, pairs_, opt : OptionsPattern[]] := 
  Contract[a, pairs, opt] + Contract[b, pairs, opt];
Contract[Contract[t_, pairs1_, opt : OptionsPattern[]], pairs2_, 
   opt2 : OptionsPattern[]] := 
  Contract[t, 
   Join[pairs1, 
    pairs2 /. 
     n_Integer :> 
      n + Boole[TrueQ@OptionValue[Contract, opt2, "Shift"]] shift[n, 
         Flatten[pairs1]]], opt];
Symbolic[Contract[t_, _, OptionsPattern[]]] := Symbolic[t];
InactiveComponents[Contract[t_, pairs_, opt : OptionsPattern[]]] := 
  TensorContract[InactiveComponents[t], pairs];
Indices[Contract[t_, pairs_, OptionsPattern[]]] := 
  Delete[Indices[t], List /@ Flatten[pairs]];
TensorPermutation[Contract[t_, pairs_, OptionsPattern[]]] := 
  DeleteCases[TensorPermutation[t], Alternatives @@ Flatten[pairs]] /. 
   n_Integer :> n - Length@Select[Flatten[pairs], # <= n &];
Contract[t_, {}, OptionsPattern[]] := t;
ContractedPairs[Contract[t_, pairs_]] := pairs;

Tensor /: 
  TensorProduct[t1_Tensor, 
   Contract[t2_, pairs_, opt : OptionsPattern[]]] := 
  Contract[TensorProduct[t1, t2], pairs + Length[Indices[t1]], opt];
Tensor /: 
  TensorProduct[Contract[t1_, pairs_, opt : OptionsPattern[]], 
   t2_Tensor] := Contract[TensorProduct[t1, t2], pairs, opt];
Contract /: 
  TensorProduct[Contract[t1_, pairs1_, opt : OptionsPattern[]], 
   Contract[t2_, pairs2_, opt2 : OptionsPattern[]]] := 
  Contract[TensorProduct[t1, t2], 
   Join[pairs1, pairs2 + Length[Indices[t1]]], opt];
Contract[t_Tensor, pairs_, opt : OptionsPattern[]] /; !OptionValue[Contract, opt, "Shift"] := Contract[t, pairs];

ClearAll[TensorPermute];
Options[TensorPermute] = {"IncludeContracted" -> False};

DisplayTemplate[TensorPermute[t_, perm_, OptionsPattern[]]] := 
  Module[{template = DisplayTemplate[t], dns, dnsreplace},
   dns = Cases[template, _dn, All, Heads -> True];
   dnsreplace = dns[[perm]];
   template /. Thread[dns -> dnsreplace]
   ];
Format[t_TensorPermute, TraditionalForm] := 
  DisplayTemplate[t] /. 
   dn[_, a_, b_] | adn[_, a_, b_] :> DisplayName[a, b];

TensorPermute[0, _, OptionsPattern[]] := 0;
TensorPermute[a_ t_, perm_, opt : OptionsPattern[]] /; 
  FreeQ[a, Alternatives @@ $TensorHeads] := a TensorPermute[t, perm, opt]; 
TensorPermute[a_ + b_, perm_] := 
 TensorPermute[a, perm] + TensorPermute[b, perm]
Symbolic[TensorPermute[t_, _, OptionsPattern[]]] := Symbolic[t];
InactiveComponents[TensorPermute[t_, perm_, OptionsPattern[]]] := 
  TensorTranspose[InactiveComponents[t], perm];
Indices[TensorPermute[t_, perm_, 
    OptionsPattern[]]] := (If[(Indices[t][[
        InversePermutation@perm]] /. 
       Lowered | Raised -> Identity) =!= (Indices[t] /. 
       Lowered | Raised -> Identity), 
    Print["Index permutation mismatch: ",Indices[t][[InversePermutation@perm]] /. 
      Lowered | Raised -> Identity, 
     Indices[t] /. Lowered | Raised -> Identity]]; 
   Indices[t][[InversePermutation@perm]]);
TensorPermutation[TensorPermute[t_, perm_, OptionsPattern[]]] := 
  TensorPermutation[t][[perm]];
TensorPermute[t_, perm_List, OptionsPattern[]] /; OrderedQ[perm] := 
  t;
TensorPermute[TensorPermute[t_, p1_], p2_, OptionsPattern[]] := TensorPermute[t, p1[[p2]]];
ContractedPairs[TensorPermute[t_, perm_, OptionsPattern[]]] := ContractedPairs[t];
TensorPermute[Contract[t_, pairs_], perm_, opt : OptionsPattern[]] := 
  Contract[
   TensorPermute[t, 
    If[OptionValue[TensorPermute, opt, "IncludeContracted"], perm, 
     riffleIn[perm, Flatten@pairs /. x_Integer :> InversePermutation[TensorPermutation[t]][[x]]]]], pairs];

Tensor /: 
  TensorProduct[t1_Tensor, 
   TensorPermute[t2_, perm_, OptionsPattern[]]] := 
  TensorPermute[TensorProduct[t1, t2], 
   Join[Range@Length[Indices[t1]], perm + Length[Indices[t1]]]];
Tensor /: 
  TensorProduct[TensorPermute[t1_, perm_, OptionsPattern[]], 
   t2_Tensor] := 
  TensorPermute[TensorProduct[t1, t2], 
   Join[perm, Range@Length[Indices[t2]] + Length[perm]]];
TensorPermute /: 
  TensorProduct[TensorPermute[t1_, perm1_, OptionsPattern[]], 
   TensorPermute[t2_, perm2_, OptionsPattern[]]] := 
  TensorPermute[TensorProduct[t1, t2], 
   Join[perm1, perm2 + Length[Indices[t1]]]];
   
InactiveComponents[TP[a__]] := TP @@ (InactiveComponents /@ {a});
Indices[TP[a__]] := Indices[TensorProduct[a]];
Unprotect[SparseArray];
SparseArray[TP[x___]] := TP[x];
SparseArray[0] = 0;
SparseArray[t_TensorTranspose] := Inactive[SparseArray][t];
Protect[SparseArray];

TensorSymmetries[_] := {};
Options[SymmetryPermutations] = {"Minimal" -> True};
SymmetryPermutations[symmetries : Except[(_Symmetric | _Antisymmetric | {_Cycles, _Integer})], opt: OptionsPattern[]] := If[OptionValue["Minimal"],
   Join @@ (SymmetryPermutations[#, opt] & /@ symmetries),
   {PermutationProduct @@ #[[;;, 1]], Times @@ #[[;;, 2]]} & /@ Tuples[SymmetryPermutations[#, opt] & /@ symmetries]
];
SymmetryPermutations[symmetry: (_Symmetric | _Antisymmetric | {_Cycles, _Integer}), OptionsPattern[]] := symmetry /. If[!TrueQ[OptionValue["Minimal"]], {
   Symmetric[xs_] :> 
    Table[{FindPermutation[xs, xs[[p]]] /. n_Integer :> xs[[n]], 1}, {p, Permutations@Range@Length[xs]}],
   Antisymmetric[xs_] :> 
    Table[{FindPermutation[xs, xs[[p]]] /. n_Integer :> xs[[n]], Signature[p]}, {p, Permutations@Range@Length[xs]}],
   cycles_ :> Table[{PermutationPower[cycles[[1]], n], cycles[[2]]^n}, {n, PermutationOrder[cycles[[1]]]}]
   }, {
  	Symmetric[xs_] :> Table[{Cycles[{xs[[i ;; i + 1]]}], 1}, {i, Length[xs] - 1}],
	Antisymmetric[xs_] :> Table[{Cycles[{xs[[i ;; i + 1]]}], -1}, {i, Length[xs] - 1}],
	cycles_ :> {cycles}
   }];

DeclareTensorSymmetry[name_, symmetry_] := (TensorSymmetries[name] = symmetry); 

TensorSymmetries[t_Tensor] := With[{symb=Symbolic[t]},
   Flatten[Table[TensorSymmetries[symb[[i, 1]]] /. (h:Cycles | Symmetric | Antisymmetric)[xs_] :> 
	    h[xs + Total[Length /@ symb[[;; i - 1]] - 1]], 
	 {i, Length[symb]}], 
   1]
];
TensorSymmetries[Contract[t_, pairs_]] := {#[[1]] /. n_Integer :> n - Length@Select[Flatten[pairs], # < n &], #[[2]]} & /@ 
	Select[TensorSymmetries[t], Intersection[#[[1,1,1]], Flatten[pairs]] == {} &];
	
TensorSymmetries[TensorPermute[t_, perm_]] := TensorSymmetries[t] /. {
  {Cycles[xs_], sign_} :> {Cycles[Table[perm[[x]], {x, xs}]], sign},
  Symmetric[xs_] :> Symmetric[Sort[perm[[xs]]]],
  Antisymmetric[xs_] :> Antisymmetric[Sort[perm[[xs]]]]
};


SymmetryReduce[t_Tensor] := t;
SymmetryReduce[Contract[t_Tensor, pairs_]] := Module[{ninds, generators, rules, allperms, minimal},
   ninds = Total[Length /@ Symbolic[t] - 1];
   rules = Join[Rule @@@ pairs, Rule @@@ (Reverse /@ pairs)];
   generators = 
    Join[TensorSymmetries[t], 
     Cases[TensorSymmetries[t], {Cycles[xs_], sign_} /; 
        AllTrue[Flatten[xs], MemberQ[Keys[rules], #] &] :> {Cycles[
         xs /. rules], sign}]];
   allperms = SymmetryPermutations[generators, "Minimal" -> False];
   If[MemberQ[allperms, {Cycles[{}], -1}], 0,
   minimal = 
    MinimalBy[allperms, PermutationList[#[[1]], ninds] &][[1]];
   minimal[[2]] Contract[
     TensorPermute[t, PermutationList[minimal[[1]], ninds]], 
     pairs]
   ]
   ];
SymmetryReduce[Contract[TensorPermute[t_, perm_], pairs_]] := 
  Module[{ninds, generators, rules, allperms, minimal},
   ninds = Total[Length /@ Symbolic[t] - 1];
   rules = Join[Rule @@@ pairs, Rule @@@ (Reverse /@ pairs)] /. x_Integer :> InversePermutation[perm][[x]];
   generators = 
    Join[TensorSymmetries[t], 
     Cases[TensorSymmetries[t], {Cycles[xs_], sign_} /; 
        AllTrue[Flatten[xs], MemberQ[Keys[rules], #] &] :> {Cycles[
         xs /. rules], sign}]];
   allperms = SymmetryPermutations[generators, "Minimal" -> False];
   If[MemberQ[allperms, {Cycles[{}], -1}], 0,
   minimal = 
    MinimalBy[allperms, perm[[PermutationList[#[[1]], ninds]]] &][[1]];
   minimal[[2]] Contract[
     TensorPermute[t, perm[[PermutationList[minimal[[1]], ninds]]]], 
     pairs]
   ]
   ];
SymmetryReduce[TensorPermute[t_, perm_]] := 
  Module[{ninds, generators, allperms, minimal},
   ninds = Total[Length /@ Symbolic[t] - 1];
   generators = TensorSymmetries[t];
   allperms = SymmetryPermutations[generators, "Minimal" -> False];
   minimal = 
    MinimalBy[allperms, perm[[PermutationList[#[[1]], ninds]]] &][[1]];
   minimal[[2]] TensorPermute[t, 
     perm[[PermutationList[minimal[[1]], ninds]]]]];

SymmetryReduce[a_ b_] /; FreeQ[a, Alternatives @@ $TensorHeads] := a SymmetryReduce[b];
SymmetryReduce[a_ + b_] := SymmetryReduce[a] + SymmetryReduce[b];
   
BuildTensor[{names : (_List)..}] := 
  TensorProduct @@ (BuildTensor /@ {names});
  
Kronecker[idx_] := Tensor[{{"\[Delta]", Raised[idx], Lowered[idx]}}];
BuildTensor[{"\[Delta]", Raised[idx_], Lowered[idx_]}] := SparseArray[IdentityMatrix[IndexData[idx][[1]]]];
 
CanonicallyOrderedComponents::incommensurate = "The terms of `` are incommensurate";
CanonicallyOrderedComponents[t_] /; MatchQ[t, Alternatives @@ (Blank /@ $TensorHeads)] := TensorTranspose[Components[t], InversePermutation[Ordering@Indices[t]]];

CanonicallyOrderedComponents[a_ b_, opt : OptionsPattern[]] /; FreeQ[a, Alternatives @@ $TensorHeads] := Explicit[a] CanonicallyOrderedComponents[b, opt];
CanonicallyOrderedComponents[a_, opt : OptionsPattern[]] /; FreeQ[a, Alternatives @@ $TensorHeads] := Explicit[a];
   
CanonicallyOrderedComponents[expr : Plus[a_, rest__]] := With[{allterms = List @@ Expand[expr]},
    If[! SameQ @@ (Sort@*Indices /@ allterms), Message[CanonicallyOrderedComponents::incommensurate, expr],
	Total[CanonicallyOrderedComponents[#] & /@ allterms]]
];

subpermute[perm_, start_, n_] := 
  ReplacePart[Range[n], 
   Thread[Range[start, 
      start + Length[perm] - 1] -> (start + perm - 1)]];
riffleIn[perm_, 
   vals_] := (Fold[Insert[#1, #2, #2] &, 
    perm /. n_Integer :> n + shift[n, vals], Sort@vals]);

ClearAll[SwapIn];
SwapIn::incommensurate = 
  "The tensor `1` is incommensurate with the replacement `2`";
Options[SwapIn] = {"Contraction" -> {}};
SwapIn[_, _, 0, OptionsPattern[]] := 0;

SwapIn[t_Tensor, {mini_, maxi_}, replacement_Tensor, 
    opt : OptionsPattern[]] /; 
   If[Sort[Indices[
       Tensor[Select[Symbolic[t], Length[#] > 1 &][[
         mini ;; maxi]]]]] === 
     Sort[Delete[Indices[replacement], 
       List /@ Flatten[OptionValue[SwapIn, opt, "Contraction"]]]], 
    True, Message[SwapIn::incommensurate, 
     TraditionalForm[Tensor[Symbolic[t][[mini ;; maxi]]]],
     TraditionalForm[replacement]]; False] := 
  Tensor[Delete[
    FlattenAt[Insert[Symbolic[t], Symbolic[replacement], maxi + 1], 
     maxi + 1], List /@ Range[mini, maxi]]];

SwapIn[TensorProduct[t1_, rest__], {mini_, maxi_}, replacement_] := 
  If[mini <= Length[Symbolic[t1]], 
   TensorProduct[SwapIn[t1, {mini, maxi}, replacement], rest], 
   TensorProduct[t1, 
    SwapIn[TensorProduct[rest], {mini, maxi} - Length[Symbolic[t1]], 
     replacement]]];
     
withCounts[xs_] := 
  Thread[{Table[1 + Count[xs[[;; i - 1]], xs[[i]]], {i, Length[xs]}], 
    xs}];
countsToPerm[pairs_] := 
 With[{pi = PositionIndex[pairs[[;; , 2]]]}, 
  Table[pi[pair[[2]]][[pair[[1]]]], {pair, pairs}]]

SwapIn[TensorPermute[t_, perm_], {mini_, maxi_}, a_. replacement_Tensor, opt : OptionsPattern[]] /; If[
   Sort[Indices[Tensor[Select[Select[Symbolic[t], Length[#] > 1 &], Length[#] > 1 &][[mini ;; maxi]]]]] === Sort[Delete[Indices[replacement], List /@ Flatten[OptionValue[SwapIn, opt, "Contraction"]]]], 
    True, 
    Message[SwapIn::incommensurate, TraditionalForm[Tensor[Symbolic[t][[mini ;; maxi]]]], TraditionalForm[replacement]]; False] := 
  With[{ssp = subpermute[
     Ordering[Delete[Indices[replacement], List /@ Flatten[OptionValue["Contraction"]]]][[
        InversePermutation@Ordering[Indices[Tensor[Select[Symbolic[t], Length[#] > 1 &][[mini ;; maxi]]]]]
      ]], 
      Total[(Length /@ Select[Symbolic[t], Length[#] > 1 &][[;; mini - 1]]) - 1] + 1, Length@Indices[t]]
   },
   TensorPermute[
      SwapIn[t, {mini, maxi}, a replacement, opt],
    riffleIn[
       countsToPerm[(withCounts[Indices[t]][[perm]])[[InversePermutation[ssp]]]], 
     Flatten[OptionValue["Contraction"]] + 
      Total[(Length /@ Symbolic[t][[;; mini - 1]]) - 1]]]
   ];
     
SwapIn[Contract[t_, pairs_, OptionsPattern[]], {mini_, maxi_}, 
   a_. replacement_Tensor, opt : OptionsPattern[]] /; 
  If[Sort[Indices[
      Tensor[Select[Symbolic[t], Length[#] > 1 &][[mini ;; maxi]]]]] ===
     Sort[Delete[Indices[replacement], 
      List /@ Flatten[OptionValue[SwapIn, opt, "Contraction"]]]], 
   True, Message[SwapIn::incommensurate, 
    TraditionalForm[
     Tensor[Select[Symbolic[t], Length[#] > 1 &][[mini ;; maxi]]]], 
    TraditionalForm[replacement]]; False] := 
 With[{inner = SwapIn[t, {mini, maxi}, a replacement, opt], 
   ssp = subpermute[
     Ordering[
       Delete[Indices[replacement], 
        List /@ Flatten[
          OptionValue["Contraction"]]]][[InversePermutation@
        Ordering[
         Indices[Tensor[
           Select[Symbolic[t], Length[#] > 1 &][[mini ;; maxi]]]]]]], 
     Total[(Length /@ 
          Select[Symbolic[t], Length[#] > 1 &][[;; mini - 1]]) - 1] + 
      1, Length@Indices[t]]},
  Contract[inner, 
   pairs /. 
       n_Integer :> InversePermutation[TensorPermutation[t]][[n]] /. 
      n_Integer :> ssp[[n]] /. 
     n_Integer :> 
      n + shift[n, 
        Flatten[OptionValue["Contraction"]] + 
         Total[(Length /@ 
             Select[Symbolic[t], Length[#] > 1 &][[;; mini - 1]]) - 
           1]] /. n_Integer :> TensorPermutation[inner][[n]]]
  ]

SwapIn[t_, {mini_, maxi_}, 
    a_. c : Contract[replacement_, pairs_, OptionsPattern[]], 
    opt : OptionsPattern[]] /; 
   If[OptionValue[SwapIn, opt, "Contraction"] =!= {} || 
     Sort[Indices[
        Tensor[Select[Symbolic[t], Length[#] > 1 &][[
          mini ;; maxi]]]]] === 
      Sort[Indices[Contract[replacement, pairs]]], True, 
    Message[SwapIn::incommensurate, 
     TraditionalForm[
      Tensor[Select[Symbolic[t], Length[#] > 1 &][[mini ;; maxi]]]], 
     TraditionalForm[c]]; False] := 
  Contract[
   SwapIn[t, {mini, maxi}, a replacement, "Contraction" -> pairs], 
   pairs + Total[(Length /@ 
        Select[Symbolic[t], Length[#] > 1 &][[;; mini - 1]]) - 1], 
   "Shift" -> False];

SwapIn[t_, {mini_, maxi_}, 
    a_. TensorPermute[replacement_, perm_, OptionsPattern[]], 
    opt : OptionsPattern[]] /; 
   If[OptionValue[SwapIn, opt, "Contraction"] =!= {} || 
     Sort[Indices[
        Tensor[Select[Symbolic[t], Length[#] > 1 &][[
          mini ;; maxi]]]]] === Sort[Indices[replacement]], True, 
    Message[SwapIn::incommensurate, TraditionalForm[t], 
     TraditionalForm[replacement]]; False] := 
  TensorPermute[
   SwapIn[t, {mini, maxi}, a replacement, 
    "Contraction" -> (OptionValue["Contraction"] /. 
       Thread[Range[Length[Indices[replacement]]] -> perm])],
   subpermute[perm, 
    With[{x = 
       Total[(Length /@ 
           Select[Symbolic[t], Length[#] > 1 &][[;; mini - 1]]) - 1]},
      x + 1 - 0 Length@Select[Flatten@ContractedPairs[t], # <= x &]], 
    Total[(Length /@ Symbolic[t]) - 1] + 
     Length@Flatten@OptionValue["Contraction"] - 
     0 Length@Flatten@ContractedPairs[t]], "IncludeContracted" -> True];

SwapIn[t_, rg_, a_ b_, opt : OptionsPattern[]] /; 
   FreeQ[a, Tensor] && FreeQ[t, TensorDerivative] := 
  a SwapIn[t, rg, b, opt];
SwapIn[t_, rg_, a_ + b_, opt : OptionsPattern[]] := 
  SwapIn[t, rg, a, opt] + SwapIn[t, rg, b, opt];
SwapIn[a_ t_, rg_, b_, opt : OptionsPattern[]] /; FreeQ[a, Tensor] := 
  a SwapIn[t, rg, b, opt];
  
indexPermutation[inds1_, inds2_] := 
  Range[Length[inds1] + Length[inds2]] /. 
   Join @@ Table[
     With[{p1 = Position[inds1, ind][[;; , 1]], 
       p2 = Position[inds2, ind][[;; , 1]]}, 
      Thread[Join[p2, p1 + Length[inds2]] -> 
        RotateLeft[Join[p2, p1 + Length[inds2]], Length[p1]]]], {ind, 
      DeleteDuplicates[Join[inds1, inds2]]}];
SwapFactors[t_Tensor | t_TensorPermute, i_Integer] := 
  TensorPermute[
   TensorPermute[
    Tensor[Symbolic[t][[subpermute[{2, 1}, i, Length@Symbolic[t]]]]], 
    subpermute[
     indexPermutation[Symbolic[t][[i, 2 ;;]] (*/. {Raised -> Identity, Lowered -> Identity}*), 
      Symbolic[t][[i + 1, 2 ;;]] (*/. {Raised -> Identity, Lowered -> Identity}*)], 
     Total[Length /@ Symbolic[t][[;; i - 1]] - 1] + 1, 
     Length@Indices[t]]], (TensorPermutation[t] /. 
      n_Integer :> 
       Which[Total[(Length /@ Symbolic[t][[;; i - 1]]) - 1] < n <= 
         Total[(Length /@ Symbolic[t][[;; i]]) - 1], 
        n + Length[Symbolic[t][[i + 1]]] - 1, 
        Total[(Length /@ Symbolic[t][[;; i]]) - 1] < n <= 
         Total[(Length /@ Symbolic[t][[;; i + 1]]) - 1], 
        n - (Length[Symbolic[t][[i]]] - 1), True, n])[[
    subpermute[
     Join[Range[Length[Symbolic[t][[i + 1]]] - 1] + 
       Length[Symbolic[t][[i]]] - 1, 
      Range[Length[Symbolic[t][[i]]] - 1]], 
     Total[(Length /@ Symbolic[t][[;; i - 1]]) - 1] + 1, 
     Length@Indices[t]]]]];
SwapFactors[Contract[t_, pairs_, opt : OptionsPattern[]], i_Integer] := 
  With[{sf = SwapFactors[t, i]},
   Contract[sf, 
    pairs /. 
     n_Integer :> 
      With[{m = InversePermutation[TensorPermutation[t]][[n]]}, 
       TensorPermutation[sf][[
        Which[Total[(Length /@ Symbolic[t][[;; i - 1]]) - 1] < m <= 
          Total[(Length /@ Symbolic[t][[;; i]]) - 1], 
         m + Length[Symbolic[t][[i + 1]]] - 1, 
         Total[(Length /@ Symbolic[t][[;; i]]) - 1] < m <= 
          Total[(Length /@ Symbolic[t][[;; i + 1]]) - 1], 
         m - (Length[Symbolic[t][[i]]] - 1), True, m]]]
       ], opt]
   ];
SwapFactors[a_ b_, i_] /; FreeQ[a, Alternatives @@ $TensorHeads] := a SwapFactors[b, i];

SwapFactors[t_, i_, j_] := 
  Which[i == j, t, i < j, SwapFactors[SwapFactors[t, i], i + 1, j], 
   i > j, SwapFactors[SwapFactors[t, i - 1], i - 1, j]];
   
firstInversion[p_] := 
  p /. {x___, a_, b_, ___} /; a > b :> Length[{x}] + 1;
swapList[perm_] /; OrderedQ[perm] := {};
swapList[perm_] := With[{fi = firstInversion[perm]},
  Append[
   swapList[
    perm[[PermutationList[Cycles[{{fi, fi + 1}}], Length[perm]]]]], fi]
  ]
  
SwapFactors[t_, perm_List] := Fold[SwapFactors, t, swapList[perm]];

$Annihilators = {};
$Creators = {};

DeclareAnnihilator[name_String] := If[!MemberQ[$Annihilators, name], AppendTo[$Annihilators, name]];
DeclareCreator[name_String] := If[!MemberQ[$Creators, name], AppendTo[$Creators, name]];

Commutator::incommensurate = "The commutator `1` is incommensurate with [`2`, `3`]";
Anticommutator::incommensurate = "The anticommutator `1` is incommensurate with {`2`, `3`}";
Unprotect[Set];
Set[Commutator[t1_, t2_], t3_] /; TrueQ[$vgCommutator] := Block[{$vgCommutator = True}, 
	If[Sort[Join[Indices[t1], Indices[t2]]] === Sort[Indices[t3]],
		Commutator[t1, t2] = t3,
		Message[Commutator::incommensurate, TraditionalForm[t3], TraditionalForm[t1], TraditionalForm[t2]]
	]
];
Set[Anticommutator[t1_, t2_], t3_] /; TrueQ[$vgAnticommutator] := Block[{$vgAnticommutator = True}, 
	If[Sort[Join[Indices[t1], Indices[t2]]] === Sort[Indices[t3]],
		Anticommutator[t1, t2] = t3,
		Message[Anticommutator::incommensurate, TraditionalForm[t3], TraditionalForm[t1], TraditionalForm[t2]]
	]
];
Protect[Set];

Options[NormalOrder] = {"Vacuum" -> False, "ConjugateVacuum" -> False, "VEV" -> False, "Callback" -> Identity};
NormalOrder[a_ + b_, opt: OptionsPattern[]] := NormalOrder[a, opt] + NormalOrder[b, opt];
NormalOrder[a_ b_, opt: OptionsPattern[]] /; FreeQ[a, Alternatives @@ (Blank /@ $TensorHeads)] := a NormalOrder[b, opt];
NormalOrder[a_, opt: OptionsPattern[]] /; FreeQ[a, Alternatives @@ (Blank /@ $TensorHeads)] := a;
NormalOrder[t : (_Tensor | _Contract | _TensorPermute | _TP), opt: OptionsPattern[]] := With[{symb = Symbolic[t]},
	Which[
		(OptionValue["Vacuum"] || OptionValue["VEV"]) && MemberQ[$Annihilators, symb[[-1,1]]], 0,
		(OptionValue["ConjugateVacuum"] || OptionValue["VEV"]) && MemberQ[$Creators, symb[[1,1]]], 0,
		True, If[MatchQ[symb, {___, a_List, b_List, ___} /; (MemberQ[$Annihilators, a[[1]]] || MemberQ[$Creators, b[[1]]]) && (Head[Commutator[Tensor[{a}], Tensor[{b}]]] =!= Commutator || Head[Anticommutator[Tensor[{a}], Tensor[{b}]]] =!= Anticommutator)],
			With[{fp = First@Cases[symb, {x___, a_List, b_List, ___} /; (MemberQ[$Annihilators, a[[1]]] || MemberQ[$Creators, b[[1]]]) && (Head[Commutator[Tensor[{a}], Tensor[{b}]]] =!= Commutator || Head[Anticommutator[Tensor[{a}], Tensor[{b}]]] =!= Anticommutator) :> Length[{x}] + 1, All]},
				If[Head[Commutator[Tensor[{symb[[fp]]}], Tensor[{symb[[fp+1]]}]]] =!= Commutator,
					NormalOrder[OptionValue["Callback"][SwapIn[t, {fp, fp + 1}, Commutator[Tensor[{symb[[fp]]}], Tensor[{symb[[fp + 1]]}]]]] + SwapFactors[t, fp], opt],
					NormalOrder[OptionValue["Callback"][SwapIn[t, {fp, fp + 1}, Anticommutator[Tensor[{symb[[fp]]}], Tensor[{symb[[fp + 1]]}]]]] - SwapFactors[t, fp], opt]
				]
			],
			t
		]
	]
];

reduceList[xs_] := InversePermutation[Ordering[xs]];

deltas[t_Tensor | t_TensorPermute] := 
 Module[{symb, deltaPositions, perm = TensorPermutation[t], 
   unpermed},
  symb = Symbolic[t];
  deltaPositions = 
   Select[Range@Length[symb], 
    MatchQ[symb[[#]], {"\[Delta]", Raised[i_], Lowered[i_]}] &];
  unpermed = 
   Table[{i, Total[Length /@ symb[[;; i - 1]] - 1] + {1, 2}}, {i, 
     deltaPositions}];
  unpermed /. {a_Integer, b_Integer} :> {perm[[a]], perm[[b]]}
  ]

other[pairs_, i_] := 
  First@Cases[pairs, {OrderlessPatternSequence[i, x_]} :> x];
  
DeleteFactor[t_Tensor | t_TensorPermute, i_, rules_ : {}] := 
 Module[{symb, rg, perm},
  symb = Symbolic[t];
  rg = Range[Total[Length /@ symb[[;; i - 1]] - 1] + 1, 
    Total[Length /@ symb[[;; i]] - 1]];
  perm = TensorPermutation[t];
  TensorPermute[Tensor[Delete[symb, i]], 
   reduceList[Delete[perm, List /@ rg] /. rules]]
  ]
   
KroneckerReduce[Contract[t_, pairs_], offset_ : 1] := 
  With[{ds = deltas[t]},
   If[offset > Length[ds], Contract[t, pairs],
    Switch[MemberQ[Flatten[pairs], #] & /@ ds[[offset, 2]],
     {False, False},
     KroneckerReduce[Contract[t, pairs], offset + 1],
     {True, False},
     KroneckerReduce[
      Contract[
       DeleteFactor[t, 
        ds[[offset, 1]], {other[pairs, ds[[offset, 2, 1]]] -> 
          ds[[offset, 2, 2]]}], 
       Select[pairs, ! MemberQ[#, ds[[offset, 2, 1]]] &] /. 
        n_Integer :> 
         n - Boole[n > ds[[offset, 2, 1]]] - 
          Boole[n > ds[[offset, 2, 2]]]], offset],
     {False, True},
     KroneckerReduce[
      Contract[
       DeleteFactor[t, 
        ds[[offset, 1]], {other[pairs, ds[[offset, 2, 2]]] -> 
          ds[[offset, 2, 1]]}], 
       Select[pairs, ! MemberQ[#, ds[[offset, 2, 2]]] &] /. 
        n_Integer :> 
         n - Boole[n > ds[[offset, 2, 1]]] - 
          Boole[n > ds[[offset, 2, 2]]]], offset],
     {True, True},
     KroneckerReduce[
      If[other[pairs, ds[[offset, 2, 1]]] == ds[[offset, 2, 2]],
       IndexData[Indices[t][[ds[[offset, 2, 1]], 1]]][[1]] Contract[
         DeleteFactor[t, ds[[offset, 1]]], 
         Select[pairs, ! MemberQ[#, ds[[offset, 2, 1]]] &] /. 
          n_Integer :> 
           n - Boole[n > ds[[offset, 2, 1]]] - 
            Boole[n > ds[[offset, 2, 2]]]],
       Contract[DeleteFactor[t, ds[[offset, 1]]], 
        Append[Select[
           pairs, ! MemberQ[#, ds[[offset, 2, 1]]] && ! MemberQ[#, ds[[offset, 2, 2]]] &], {other[pairs, 
            ds[[offset, 2, 1]]], 
           other[pairs, ds[[offset, 2, 2]]]}] /. 
         n_Integer :> 
          n - Boole[n > ds[[offset, 2, 1]]] - 
           Boole[n > ds[[offset, 2, 2]]]]
       ], offset]
     ]
    ]
   ];
KroneckerReduce[t_Tensor | t_TensorPermute, offset_ : 1] := t;
KroneckerReduce[a_ , offset_ : 1] /; 
   FreeQ[a, Alternatives @@ TensorTools`Private`$TensorHeads] := a ;
KroneckerReduce[a_ b_, offset_ : 1] /; 
   FreeQ[a, Alternatives @@ TensorTools`Private`$TensorHeads] := 
  a KroneckerReduce[b, offset];
KroneckerReduce[a_ + b_, offset_ : 1] := 
  KroneckerReduce[a, offset] + KroneckerReduce[b, offset];
Tensor[{}] = 1;

SetAttributes[TensorInterpret, {HoldFirst, Listable}];
TensorInterpret[expr_Plus] := Module[{held},
   held = Hold[expr];
   ReleaseHold[Replace[held, x_ :> TensorInterpret[x], {2}]]
];
TensorInterpret[a_ b_] /; 
   FreeQ[a, Alternatives @@ TensorTools`Private`$TensorHeads] := 
  a TensorInterpret[b];
  
TensorInterpret::nonpr = "Cannot assign to non-primitive tensor ``";
TensorInterpret::incommensurate = "LHS `` is not commensurate with RHS ``";
TensorInterpret[expr_Set] := Module[{held, head, rhs, perm, rhsIndices}, 
   held = Hold[expr];
   head = ReleaseHold[held /. HoldPattern[h_[___] = _] :> h];
   perm = ReleaseHold[held /. HoldPattern[h_[xs___Integer] = _] :> {xs}];
   rhs  = ReleaseHold[held /. HoldPattern[_ = b_] :> TensorInterpret[b]];
   rhsIndices = Indices@Last@Cases[rhs, (Alternatives @@ $TensorHeads)[___], {0,2}];
   Which[
    ContractedPairs[head] != {} || Length[Symbolic[head]] > 1, 
    	Message[TensorInterpret::nonpr, TraditionalForm[head]], 
 	Sort[Indices[head]] != Sort[rhsIndices], 
    	Message[TensorInterpret::incommensurate, TraditionalForm[head], TraditionalForm[rhs]], 
    True, 
    	BuildTensor[Symbolic[head][[1]]] = TensorTranspose[CanonicallyOrderedComponents[rhs], FindPermutation[Sort[rhsIndices], Indices[head][[perm]]]];
    ]
 ];

TensorInterpret::pos = 
  "Positive indices should be unique integers 1..n";
TensorInterpret::neg = 
  "Negative indices should come in pairs of integers -1..-n";
TensorInterpret[expr_] := 
 Module[{held, heads, nums, pairs, pos, neg, perm},
  held = Hold[expr];
  heads = If[held[[1,0]] === Times, Identity, List][held /. {Times -> List, h_[___Integer] :> h}];
  nums = Cases[held, _Integer, {If[held[[1,0]] === Times, 3, 2]}];
  neg = Select[nums, # < 0 &];
  pairs = Position[nums, #][[;; , 1]] & /@ DeleteDuplicates[neg];
  pos = Select[nums, # > 0 &];
  perm = InversePermutation@Ordering[Select[nums, # > 0 &]];
  Which[Length[pos] > 
     0 && (Length[pos] != Length[DeleteDuplicates[pos]] || 
      Max[pos] != Length[pos] || ! AllTrue[pos, MatchQ[#, _Integer] &]),
   Message[TensorInterpret::pos],
   Length[neg] > 
     0 && (! AllTrue[pairs, Length[#] == 2 &] || ! AllTrue[neg, MatchQ[#, _Integer] &] || 
      Min[neg] != -Length[neg]/2),
   Message[TensorInterpret::neg],
   True,
   TensorPermute[Contract[TensorProduct @@ ReleaseHold[heads], pairs],
     perm]
   ]
  ]

End[]

EndPackage[]

