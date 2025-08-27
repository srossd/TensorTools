(* Wolfram Language package *)

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
SymmetryReduce[Contract[t_Tensor, pairs_]] := Module[{ninds, generators, rules, allperms},
   ninds = Total[Length /@ Symbolic[t] - 1];
   rules = Join[Rule @@@ pairs, Rule @@@ (Reverse /@ pairs)];
   generators = 
    Join[TensorSymmetries[t], 
     Cases[TensorSymmetries[t], {Cycles[xs_], sign_} /; 
        AllTrue[Flatten[xs], MemberQ[Keys[rules], #] &] :> {Cycles[
         xs /. rules], sign}]];
   allperms = SymmetryPermutations[generators, "Minimal" -> False];
   If[MemberQ[allperms, {Cycles[{}], -1}], 0,
   First@Sort[Table[minimal[[2]] Contract[
     TensorPermute[t, PermutationList[minimal[[1]], ninds]], 
     pairs], {minimal, allperms}]]
   ]
   ];
SymmetryReduce[TensorPermute[t_Tensor, perm_]] := 
  Module[{ninds, generators, allperms},
   ninds = Total[Length /@ Symbolic[t] - 1];
   generators = TensorSymmetries[t];
   allperms = SymmetryPermutations[generators, "Minimal" -> False];
   First@Sort[Table[minimal[[2]] TensorPermute[t, 
     perm[[PermutationList[minimal[[1]], ninds]]]], {minimal, allperms}]]
  ];
     
SymmetryReduce[TensorPermute[Contract[t_, pairs_], perm_]] := Module[{ninds, generators, rules, insideperm, allperms},
   ninds = Total[Length /@ Symbolic[t] - 1];
   rules = Join[Rule @@@ pairs, Rule @@@ (Reverse /@ pairs)];
   generators = 
    Join[TensorSymmetries[t], 
     Cases[TensorSymmetries[t], {Cycles[xs_], sign_} /; 
        AllTrue[Flatten[xs], MemberQ[Keys[rules], #] &] :> {Cycles[
         xs /. rules], sign}]];
   allperms = SymmetryPermutations[generators, "Minimal" -> False];
   insideperm = riffleIn[perm, Flatten[pairs]];
   If[MemberQ[allperms, {Cycles[{}], -1}], 0,
   First@Sort[Table[minimal[[2]] Contract[
     TensorPermute[t, PermutationProduct[PermutationList[minimal[[1]], ninds], insideperm]], 
     pairs], {minimal, allperms}]]
   ]
   ];

SymmetryReduce[a_ b_] /; FreeQ[a, Alternatives @@ $TensorHeads] := a SymmetryReduce[b];
SymmetryReduce[a_ + b_] := SymmetryReduce[a] + SymmetryReduce[b];