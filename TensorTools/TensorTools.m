(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Mar 6, 2023 *)

$TensorHeads = {Tensor, TensorPermute, Contract, TP};
AddTensorHead[head_Symbol] := If[!MemberQ[$TensorHeads, head], AppendTo[$TensorHeads, head]];

Tensor[{}] = 1;
Symbolic[a_ b_] /; FreeQ[a, Alternatives @@ $TensorHeads] := Symbolic[b];
Indices[a_ b_] /; FreeQ[a, Alternatives @@ $TensorHeads] := Indices[b];
Symbolic[a_] /; FreeQ[a, Alternatives @@ $TensorHeads] := {};
Indices[a_] /; FreeQ[a, Alternatives @@ $TensorHeads] := {};

Symbolic[Tensor[names_]] := names;
Indices[Tensor[names_]] := Join @@ (Rest /@ names);

Symbolic[TensorPermute[t_, _]] := Symbolic[t];
Indices[TensorPermute[t_, perm_]] := Indices[t][[InversePermutation@perm]];
TensorPermute[a_ t_, perm_] /; FreeQ[a, Alternatives @@ $TensorHeads] := a TensorPermute[t, perm]; 
TensorPermute[a_ + b_, perm_] := TensorPermute[a, perm] + TensorPermute[b, perm];

Symbolic[Contract[t_, _]] := Symbolic[t];
Indices[Contract[t_, pairs_]] := Delete[Indices[t], List /@ Flatten[pairs]];
Contract[a_ t_, pairs_] /; FreeQ[a, Alternatives @@ $TensorHeads] := a Contract[t, pairs]; 
Contract[a_ + b_, pairs_] := Contract[a, pairs] + Contract[b, pairs];

Index[dim_, alphabet_] := Index[dim, alphabet, 1];
Index[dim_, alphabet_, offset_] := Index[dim, alphabet, offset, Identity];
Unprotect[Set];
Set[IndexData[idxtype_], i_Index] /; !TrueQ[$vgIndexData] := Block[{$vgIndexData = True}, idxtype = idxtype; IndexData[idxtype] = i;];
Protect[Set];

(* for some reason this complains about the Unprotect the first time, but works fine the second time *)
Quiet[
Unprotect[TensorProduct];
TensorProduct[y___, a_ b_, c_, z___] /; FreeQ[a, Alternatives @@ $TensorHeads] := a TensorProduct[y, b, c, z];
TensorProduct[y___, a_, b_ c_, z___] /; FreeQ[b, Alternatives @@ $TensorHeads] := b TensorProduct[y, a, c, z];
t : TensorProduct[x___, y_Plus, z___] := Activate[Distribute[Inactive[TensorProduct][x, y, z]]];
Protect[TensorProduct];
]

Unprotect[TensorProduct];
TensorProduct[y___, a_ b_, c_, z___] /; FreeQ[a, Alternatives @@ $TensorHeads] := a TensorProduct[y, b, c, z];
TensorProduct[y___, a_, b_ c_, z___] /; FreeQ[b, Alternatives @@ $TensorHeads] := b TensorProduct[y, a, c, z];
t : TensorProduct[x___, y_Plus, z___] := Activate[Distribute[Inactive[TensorProduct][x, y, z]]];
Protect[TensorProduct];

Symbolic[TensorProduct[t1_, t2_]] := Join[Symbolic[t1], Symbolic[t2]];
Indices[tp_TensorProduct] := Join @@ (Indices /@ (List @@ tp));

Tensor /: TensorProduct[t1_Tensor, t2_Tensor] := Tensor[Join[Symbolic[t1], Symbolic[t2]]];
Tensor /: TensorProduct[t1_Tensor, Contract[t2_, pairs_]] := Contract[TensorProduct[t1, t2], pairs + Length[Indices[t1]]];
Tensor /: TensorProduct[Contract[t1_, pairs_], t2_Tensor] := Contract[TensorProduct[t1, t2], pairs];
Contract /: TensorProduct[Contract[t1_, pairs1_], Contract[t2_, pairs2_]] := Contract[TensorProduct[t1, t2], Join[pairs1, pairs2 + Length[Indices[t1]]]];
Tensor /: TensorProduct[t1_Tensor, TensorPermute[t2_, perm_]] := TensorPermute[TensorProduct[t1, t2], Join[Range@Length[Indices[t1]], perm + Length[Indices[t1]]]];
Tensor /: TensorProduct[TensorPermute[t1_, perm_], t2_Tensor] := TensorPermute[TensorProduct[t1, t2], Join[perm, Range@Length[Indices[t2]] + Length[perm]]];
TensorPermute /: TensorProduct[TensorPermute[t1_, perm1_], TensorPermute[t2_, perm2_]] := TensorPermute[TensorProduct[t1, t2], Join[perm1, perm2 + Length[Indices[t1]]]];

Contract[t_, {}] := t;
TensorPermute[t_, perm_List] := t /; OrderedQ[perm];
Contract[Contract[t_, pairs1_], pairs2_] := Contract[t, Join[pairs1, pairs2 /. n_Integer :> n + shift[n, Flatten[pairs1]]]];
TensorPermute[TensorPermute[t_, p1_], p2_] := TensorPermute[t, p1[[p2]]];
Contract[TensorPermute[t_, perm_], pairs_] := FromNCON[NCON[Contract[TensorPermute[t, perm], pairs]]];

TensorPermutation[t_Tensor] := Range@Length[Indices[t]];
TensorPermutation[a_ b_] /; FreeQ[a, Alternatives @@ $TensorHeads] := TensorPermutation[b];
TensorPermutation[a_] /; FreeQ[a, Alternatives @@ $TensorHeads] := {};
TensorPermutation[TP[t : Except[_TP]]] := TensorPermutation[t];
TensorPermutation[TensorProduct[t1_, t2__]] := Join[TensorPermutation[t1], TensorPermutation[TensorProduct[t2]] + Length[TensorPermutation[t1]]];
TensorPermutation[TP[t1_, t2__]] := Join[TensorPermutation[t1], TensorPermutation[TP[t2]] + Length[TensorPermutation[t1]]];
TensorPermutation[Contract[t_, pairs_]] := DeleteCases[TensorPermutation[t], Alternatives @@ Flatten[pairs]] /. n_Integer :> n - Length@Select[Flatten[pairs], # <= n &];
TensorPermutation[TensorPermute[t_, perm_]] := TensorPermutation[t][[perm]];