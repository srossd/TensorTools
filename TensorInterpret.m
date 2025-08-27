(* Wolfram Language package *)

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
    !FreeQ[head, Contract] || Length[Symbolic[head]] > 1, 
    	Message[TensorInterpret::nonpr, TraditionalForm[head]], 
 	Sort[Indices[head]] =!= Sort[rhsIndices], 
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