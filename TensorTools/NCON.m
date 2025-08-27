(* Wolfram Language package *)

reindexNCON[ncon_] := Module[{inds, grps, rules},
  inds = DeleteDuplicates[Flatten[ncon[[;; , 2]], 1]];
  grps = GroupBy[inds, Last];
  rules = 
   Flatten@Table[
     Thread[Sort[grps[k]] -> 
       Thread[{Join[Range[-Count[grps[k][[;; , 1]], x_ /; x < 0], -1],
           Range[1, Count[grps[k][[;; , 1]], x_ /; x > 0]]], 
         grps[k][[;; , 2]]}]], {k, Keys[grps]}];
  ncon /. rules
  ]

SetAttributes[NCON, HoldFirst];
NCON[t_Tensor] := Module[{heads, inds},
   inds = partition[withCounts[Indices[t]], (Length /@ Symbolic[t]) - 1];
   heads = Symbolic[t][[;; , 1]];
   Table[{heads[[i]], inds[[i]]}, {i, Length[heads]}]
   ];
NCON[t_TensorProduct] := Module[{heads, inds},
   inds = partition[withCounts[Indices[t]], (Length /@ Symbolic[t]) - 1];
   heads = Symbolic[t][[;; , 1]];
   Table[{heads[[i]], inds[[i]]}, {i, Length[heads]}]
   ];
NCON[TensorPermute[t_, perm_]] := Module[{inds},
   inds = withCounts[Indices[t]];
   NCON[t] /. Thread[inds -> inds[[perm]]]
   ];
NCON[Contract[t_, pairs_]] := Module[{inds, rules},
  inds = withCounts[Indices[t]];
  rules = 
   Flatten@Table[
     Thread[inds[[pairs[[i]]]] -> 
       Table[{-100 - i, inds[[pairs[[i, j]], 2]]}, {j, 2}]], {i, 
      Length[pairs]}];
  reindexNCON[NCON[t] /. rules]
  ]

FromNCON[initncon_] := 
 Module[{ncon, base, heads, inds, pairs, pos, neg, perm},
  ncon = reindexNCON[initncon];
  base = Tensor[Replace[ncon, {h_, i_} :> Prepend[i[[;; , 2]], h], {1}]];
  heads = ncon[[;; , 1]];
  inds = Flatten[ncon[[;; , 2]], 1];
  neg = Select[inds, #[[1]] < 0 &];
  pairs = Position[inds /. (Lowered | Raised) -> Identity, #][[;;, 1]] & /@ DeleteDuplicates[neg /. (Lowered | Raised) -> Identity];
  pos = Select[inds, #[[1]] > 0 &];
  perm = PermutationList[FindPermutation[grpsort[pos], pos], Length[pos]];
  TensorPermute[Contract[base, pairs], InversePermutation@perm]
]