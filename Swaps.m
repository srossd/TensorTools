(* Wolfram Language package *)

SwapFactors[a_ b_, i__] /; FreeQ[a, Alternatives @@ $TensorHeads] := a SwapFactors[b, i];
SwapFactors[t_Tensor | t_TensorPermute | t_Contract, i_Integer] := SwapFactors[t, Cycles[{{i, i + 1}}]];
SwapFactors[t_Tensor | t_TensorPermute | t_Contract, i_, j_] := SwapFactors[t, Cycles[{Range[i, j, Sign[j - i]]}]];

SwapFactors[t_Tensor | t_TensorPermute | t_Contract, perm_] := Module[{ncon},
   ncon = NCON[t];
   FromNCON[ncon[[PermutationList[perm, Length[ncon]]]]]
];

SwapIn[t_, {mini_, maxi_}, 0] := 0;
SwapIn[t_, {mini_, maxi_}, a_ b_] /; FreeQ[a, Alternatives @@ $TensorHeads] := a SwapIn[t, {mini, maxi}, b];
SwapIn[a_ t_, {mini_, maxi_}, b_] /; FreeQ[a, Alternatives @@ $TensorHeads] := a SwapIn[t, {mini, maxi}, b];
SwapIn[t_, {mini_, maxi_}, a_ + b_] := SwapIn[t, {mini, maxi}, a] + SwapIn[t, {mini, maxi}, b];

SwapIn::incommensurate = "The replacement `1` is incommensurate with `2` between factors `3` and `4`";
SwapIn[t_, {mini_, maxi_}, replacement_] := Module[{ncon, ncon2, grps, grps2, rules, maxneg},
  ncon = NCON[t];
  ncon2 = NCON[replacement]; 
  grps = Keys[Select[Counts[#], Function[x, x == 1]]] & /@ GroupBy[Flatten[ncon[[mini;;maxi, 2]], 1], Last];
  grps2 = Sort /@ GroupBy[Select[Flatten[ncon2[[;;, 2]], 1], #[[1]] > 0 &], Last];
  maxneg = Min[Append[Cases[Flatten[ncon[[;;, 2]], 1][[;;,1]], i_Integer /; i < 0], 0]];
  
  If[Sort[Keys[grps]] =!= Sort[Keys[grps2]] || AnyTrue[Keys[grps], Length[grps[#]] != Length[grps2[#]] &],
     Message[SwapIn::incommensurate, replacement, t, mini, maxi],
     
  	 rules = Join @@ Table[Thread[grps2[k] -> grps[k]], {k, Keys[grps]}];
  
  	FromNCON[Join[ncon[[;;mini - 1]], ncon2 /. rules /. {i_Integer, ind_} /; i < 0 :> {maxneg + i, ind}, ncon[[maxi + 1;;]]]]
  ]
];

DeleteFactor[t_Tensor | t_TensorPermute, i_, rules_ : {}] := 
 Module[{symb, rg, perm},
  symb = Symbolic[t];
  rg = Range[Total[Length /@ symb[[;; i - 1]] - 1] + 1, 
    Total[Length /@ symb[[;; i]] - 1]];
  perm = TensorPermutation[t];
  TensorPermute[Tensor[Delete[symb, i]], 
   reduceList[Delete[perm, List /@ rg] /. rules]]
  ]