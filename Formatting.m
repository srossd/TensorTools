(* Wolfram Language package *)

DisplayName[Raised[idx_], ct_] := DisplayName[idx, ct];
DisplayName[Lowered[idx_], ct_] := DisplayName[idx, ct];
DisplayName[idx_, ct_] := With[{data = IndexData[idx]}, 
   With[{s = data[[4]][Alphabet[data[[2]]][[ct + data[[3]] - 1]]]}, 
      If[!StringQ[s], s, ToExpression[s, TraditionalForm, HoldForm] /. Null -> ""]
   ]
];


  
tensorFormat[t_] := Module[{ncon, inds, maxLowers, maxUppers},
   ncon = NCON[t];
   inds = Flatten[ncon[[;;, 2]], 1];
   maxUppers = Max[#[[;;, 1]]] & /@ GroupBy[Select[inds, #[[1]] >= 1 && #[[2,0]] === Raised &],  #[[2,1]] &];
   maxLowers = Max[#[[;;, 1]]] & /@ GroupBy[Select[inds, #[[1]] >= 1 && #[[2,0]] === Lowered &], #[[2,1]] &];
   Row[Replace[
      ncon 
      	/. {i_Integer, indtype : (_Lowered | _Raised)} /; i < 0 :> {((maxLowers[indtype[[1]]] + maxUppers[indtype[[1]]]) /. _?MissingQ -> 0) - i, indtype, 0}
      	/. {i_Integer, indtype_Raised} :> {(maxLowers[indtype[[1]]] /. _?MissingQ -> 0) + i, indtype},
      {h_, inds_} :> Fold[
         If[#2[[2,0]] === Raised, Superscript, Subscript][#1, If[Length[#2] == 3, Function[x, Style[x, Gray]], Identity] @ DisplayName[#2[[2,1]], #2[[1]]]] &,
         h,
         inds
      ],
      {1}]
   ] //. {Subscript[Subscript[a_, b_], c_] :> Subscript[a, Row[{b, c}]], Superscript[Superscript[a_, b_], c_] :> Superscript[a, Row[{b, c}]]}
];

Format[t_Tensor, TraditionalForm] := tensorFormat[t]; 
Format[t_Contract, TraditionalForm] := tensorFormat[t]; 
Format[t_TensorPermute, TraditionalForm] := tensorFormat[t]; 
Unprotect[TensorProduct];
Format[t_TensorProduct, TraditionalForm] /; ! FreeQ[t, Alternatives @@ $TensorHeads] := tensorFormat[t];
Protect[TensorProduct];
