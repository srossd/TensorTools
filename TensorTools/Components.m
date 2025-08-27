(* Wolfram Language package *)

InactiveComponents[Tensor[{}]] := 1;
InactiveComponents[Tensor[{name_}]] := BuildTensor[{name}];
InactiveComponents[Tensor[names_]] /; Length[names] > 1 := TP @@ (BuildTensor /@ names);

InactiveComponents[Contract[t_, pairs_]] := TensorContract[InactiveComponents[t], pairs];
InactiveComponents[TensorPermute[t_, perm_]] := TensorTranspose[InactiveComponents[t], perm];

InactiveComponents[TP[a__]] := TP @@ (InactiveComponents /@ {a});
Indices[TP[a__]] := Indices[TensorProduct[a]];
Unprotect[SparseArray];
SparseArray[TP[x___]] := TP[x];
SparseArray[0] = 0;
Protect[SparseArray];

SetAttributes[TP, Flat];
Components[t_] := 
 Activate[
   InactiveComponents[t /. TensorProduct -> Distribute@*TP] 
   	/. Times -> Inactive[Times] 
   	/. TP -> Inactive[TensorProduct] 
   	/. TensorProduct -> TP 
   	//. Inactive[TP][x__, y_, z___] | Inactive[TP][x___, y_, z__] 
   		/; ! ArrayQ[y] && !ListQ[y] && FreeQ[y, Alternatives @@ $TensorHeads] 
   		:> Inactive[Times][y, TP[x, z]] 
   	/. TP -> Inactive[TensorProduct] 
 ]
 
If[Head[explicitRules] === Symbol, explicitRules = {}];
AddExplicitRule[rule_] := AppendTo[explicitRules, rule];
Explicit[x_] := x //. explicitRules;
InactiveComponents[a_ b_] /; FreeQ[a, Alternatives @@ $TensorHeads] := Explicit[a] InactiveComponents[b];
InactiveComponents[a_] /; FreeQ[a, Alternatives @@ $TensorHeads] := Explicit[a];

BuildTensor[{names : (_List)..}] := TensorProduct @@ (BuildTensor /@ {names});
  
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