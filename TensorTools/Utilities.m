(* Wolfram Language package *)

(* an amount by which n has to shift to account for deleted indices vals *)
shift[n_, vals_] := If[n < Min[vals], 0, 1 + shift[n + 1, DeleteCases[vals, Min[vals]]]];


riffleIn[perm_, vals_] := (Fold[Insert[#1, #2, #2] &, perm /. n_Integer :> n + shift[n, vals], Sort@vals]);

(* breaks up a long list into lists of lengths lens *)
partition[{}, {}] := {};
partition[xs_, lens_] := Prepend[partition[xs[[lens[[1]] + 1 ;;]], lens[[2 ;;]]], xs[[;; lens[[1]]]]];

(* sorts {{1, a}, {2, b}, {1, b} ... } keeping groups a, b in place *)
grpsort[list_] := Module[{grps, rules},
  grps = GroupBy[list, Last];
  rules = 
   Flatten@Table[
     Thread[grps[k] -> 
       Thread[{Sort[grps[k][[;; , 1]]], grps[k][[;; , 2]]}]], {k, 
      Keys[grps]}];
  list /. rules
  ]
  

(* takes {a, a, b, ...} and makes {{1, a}, {2, a}, {1, b}, ...} *)
withCounts[xs_] := Thread[{Table[1 + Count[xs[[;; i - 1]], xs[[i]]], {i, Length[xs]}], xs}];