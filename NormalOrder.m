(* Wolfram Language package *)

$Annihilators = {};
$Creators = {};

DeclareAnnihilator[name_String] := If[!MemberQ[$Annihilators, name], AppendTo[$Annihilators, name]];
DeclareCreator[name_String] := If[!MemberQ[$Creators, name], AppendTo[$Creators, name]];

TensorCommutator::incommensurate = "The commutator `1` is incommensurate with [`2`, `3`]";
TensorAnticommutator::incommensurate = "The anticommutator `1` is incommensurate with {`2`, `3`}";
Unprotect[Set];
Set[TensorCommutator[t1_, t2_], t3_] /; TrueQ[$vgTensorCommutator] := Block[{$vgTensorCommutator = True}, 
	If[Sort[Join[Indices[t1], Indices[t2]]] === Sort[Indices[t3]],
		TensorCommutator[t1, t2] = t3,
		Message[TensorCommutator::incommensurate, TraditionalForm[t3], TraditionalForm[t1], TraditionalForm[t2]]
	]
];
Set[TensorAnticommutator[t1_, t2_], t3_] /; TrueQ[$vgTensorAnticommutator] := Block[{$vgTensorAnticommutator = True}, 
	If[Sort[Join[Indices[t1], Indices[t2]]] === Sort[Indices[t3]],
		TensorAnticommutator[t1, t2] = t3,
		Message[TensorAnticommutator::incommensurate, TraditionalForm[t3], TraditionalForm[t1], TraditionalForm[t2]]
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
		True, If[MatchQ[symb, {___, a_List, b_List, ___} /; (MemberQ[$Annihilators, a[[1]]] || MemberQ[$Creators, b[[1]]]) && (Head[TensorCommutator[Tensor[{a}], Tensor[{b}]]] =!= TensorCommutator || Head[TensorAnticommutator[Tensor[{a}], Tensor[{b}]]] =!= TensorAnticommutator)],
			With[{fp = First@Cases[symb, {x___, a_List, b_List, ___} /; (MemberQ[$Annihilators, a[[1]]] || MemberQ[$Creators, b[[1]]]) && (Head[TensorCommutator[Tensor[{a}], Tensor[{b}]]] =!= TensorCommutator || Head[TensorAnticommutator[Tensor[{a}], Tensor[{b}]]] =!= TensorAnticommutator) :> Length[{x}] + 1, All]},
				If[Head[TensorCommutator[Tensor[{symb[[fp]]}], Tensor[{symb[[fp+1]]}]]] =!= TensorCommutator,
					NormalOrder[OptionValue["Callback"][SwapIn[t, {fp, fp + 1}, TensorCommutator[Tensor[{symb[[fp]]}], Tensor[{symb[[fp + 1]]}]]]] + SwapFactors[t, fp], opt],
					NormalOrder[OptionValue["Callback"][SwapIn[t, {fp, fp + 1}, TensorAnticommutator[Tensor[{symb[[fp]]}], Tensor[{symb[[fp + 1]]}]]]] - SwapFactors[t, fp], opt]
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