(* ::Package:: *)

(* Wolfram Language Package *)

BeginPackage["TensorTools`"]
(* Exported symbols added here with SymbolName::usage *)  


Index::usage = "Index[\!\(\*
StyleBox[\"dimension\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"alphabet\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"offset\",\nFontSlant->\"Italic\"]\)] represents an index with values in Range[\!\(\*
StyleBox[\"dimension\",\nFontSlant->\"Italic\"]\)], displayed using symbols in Alphabet[\!\(\*
StyleBox[\"alphabet\",\nFontSlant->\"Italic\"]\)] starting at index \!\(\*
StyleBox[\"offset\",\nFontSlant->\"Italic\"]\).\n" <>
           "Index[\!\(\*
StyleBox[\"dimension\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"alphabet\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"offset\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"display\",\nFontSlant->\"Italic\"]\)] wraps the displayed indices using the function \!\(\*
StyleBox[\"display\",\nFontSlant->\"Italic\"]\).";

IndexData::usage = "IndexData[\!\(\*
StyleBox[\"indextype\",\nFontSlant->\"Italic\"]\)] can be set to an Index to specify \!\(\*
StyleBox[\"indextype\",\nFontSlant->\"Italic\"]\).";

Raised::usage = "Raised[\!\(\*
StyleBox[\"indextype\",\nFontSlant->\"Italic\"]\)] represents a raised index of type \!\(\*
StyleBox[\"indextype\",\nFontSlant->\"Italic\"]\).";
Lowered::usage = "Lowered[\!\(\*
StyleBox[\"indextype\",\nFontSlant->\"Italic\"]\)] represents a raised index of type \!\(\*
StyleBox[\"indextype\",\nFontSlant->\"Italic\"]\).";

DisplayName::usage = "DisplayName[\!\(\*
StyleBox[\"indextype\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)] gives the displayed form of the \*
StyleBox[\(\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)th\)] index of type \!\(\*
StyleBox[\"indextype\",\nFontSlant->\"Italic\"]\).";

Components::usage = "Components[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\)] gives the components of \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\) as a SparseArray, with indices in the order they appear in the unpermuted form of tensor.";

InactiveComponents::usage = "InactiveComponents[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\)] gives the components of \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\) with tensor products left inactive.";

Symbolic::usage = "Symbolic[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\)] gives the list of the symbols and indices appearing in \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\).";
Indices::usage = "Indices[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\)] gives a list of the indices appearing in \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\).";
TensorPermutation::usage = "TensorPermutation[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\)] gives the permutation of the indices appearing in \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\).";
ContractedPairs::usage = "ContractedPairs[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\)] gives a list of pairs of indices contracted in \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\).";

Tensor::usage = "Tensor[{{\!\(\*
StyleBox[\"name1\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"i1\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"j1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"...\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"}\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"{\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"name2\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"i2\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"j2\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"...\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"}\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"...\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"}\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)represents a tensor product with tensors named \!\(\*
StyleBox[\"name1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"name2\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"...\",\nFontSlant->\"Italic\"]\) with indices \!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"i1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"j1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"...\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"i2\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"j2\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"...\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"...\",\nFontSlant->\"Italic\"]\).";
Contract::usage = "Contract[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"pairs\",\nFontSlant->\"Italic\"]\)] represents \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\) with the indices in the list \!\(\*
StyleBox[\"pairs\",\nFontSlant->\"Italic\"]\) contracted.";
TensorPermute::usage = "TensorPermute[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"permutation\",\nFontSlant->\"Italic\"]\)] represents \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\) with its indices ordered as in \!\(\*
StyleBox[\"permutation\",\nFontSlant->\"Italic\"]\).";

CanonicallyOrderedComponents::usage = "CanonicallyOrderedComponents[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\)] gives the components of the tensor expression \!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\) as a SparseArray with its indices in a canonical order.";

BuildTensor::usage = "BuildTensor[{\!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\), ...}] can be set to the SparseArray of components of Tensor[{{\!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\), ...}}].";

SwapFactors::usage = "SwapFactors[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)] swaps the \*
StyleBox[\(\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)th\)] and (\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)+1)th factors in \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\).\n"<>
					 "SwapFactors[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\)] swaps the \*
StyleBox[\(\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)th\)] and \*
StyleBox[\(\!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\)th\)] factors in \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\).\n"<>
					 "SwapFactors[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\), {\!\(\*
StyleBox[\"i1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"i2\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)...}] puts the factors of \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\) in the order {\!\(\*
StyleBox[\"i1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"i2\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"...\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"}\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontSlant->\"Plain\"]\)";

DeclareTensorSymmetry::usage = "DeclareTensorSymmetry[\!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"symmetry\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)enforces that tensors named \!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)will have symmetry \!\(\*
StyleBox[\"symmetry\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\".\",\nFontSlant->\"Italic\"]\)";

SymmetryPermutations::usage = "SymmetryPermutations[\!\(\*
StyleBox[\"symmetry\",\nFontSlant->\"Italic\"]\)] gives a list of pairs of permutations and signs corresponding to \!\(\*
StyleBox[\"symmetry\",\nFontSlant->\"Italic\"]\).";

TensorSymmetries::usage = "TensorSymmetries[\!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\)] gives the symmetry of the tensor named \!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\)
TensorSymmetries[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\)] gives a list of symmetry generators of \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\).";

SymmetryReduce::usage = "SymmetryReduce[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\)] uses any known symmetries to put the indices of \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\) into a canonical order.";

SwapIn::usage = "SwapIn[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\), {\!\(\*
StyleBox[\"from\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"to\",\nFontSlant->\"Italic\"]\)}, \!\(\*
StyleBox[\"replacement\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\) replaces factors from \!\(\*
StyleBox[\"from\",\nFontSlant->\"Italic\"]\) to \!\(\*
StyleBox[\"to\",\nFontSlant->\"Italic\"]\) in \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\) with \!\(\*
StyleBox[\"replacement\",\nFontSlant->\"Italic\"]\).";

AddTensorHead::usage = "AddTensorHead[\!\(\*
StyleBox[\"head\",\nFontSlant->\"Italic\"]\)] declares that symbols with head \!\(\*
StyleBox[\"head\",\nFontSlant->\"Italic\"]\) should be treated as tensors.";

DeclareAnnihilator::usage = "DeclareAnnihilator[\!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\)] declares that a tensor named \!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\) is an annihilator, and should be (anti)commuted to the right by NormalOrder.";
DeclareCreator::usage = "DeclareCreator[\!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\)] declares that a tensor named \!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\) is a creator, and should be (anti)commuted to the left by NormalOrder.";

Commutator::usage = "Commutator[\!\(\*
StyleBox[\"tensor1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"tensor2\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\) can be set to the commutator of \!\(\*
StyleBox[\"tensor1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)and \!\(\*
StyleBox[\"tensor2\",\nFontSlant->\"Italic\"]\) to be used by NormalOrder.";
Anticommutator::usage = "AntiCommutator[\!\(\*
StyleBox[\"tensor1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"tensor2\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\) can be set to the anticommutator of \!\(\*
StyleBox[\"tensor1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)and \!\(\*
StyleBox[\"tensor2\",\nFontSlant->\"Italic\"]\) to be used by NormalOrder.";

NormalOrder::usage = "NormalOrder[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\)] puts the terms of \!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)in normal order."

Explicit::usage = "Explicit[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\)] puts certain abbreviated expressions into a more explicit form.";

AddExplicitRule::usage = "AddExplicitRule[\!\(\*
StyleBox[\"rule\",\nFontSlant->\"Italic\"]\)] adds \!\(\*
StyleBox[\"rule\",\nFontSlant->\"Italic\"]\) to the list of transformations used when calling Explicit.";

Kronecker::usage = "Kronecker[\!\(\*
StyleBox[\"idx\",\nFontSlant->\"Italic\"]\)] gives the Kronecker tensor with index type \!\(\*
StyleBox[\"idx\",\nFontSlant->\"Italic\"]\).";

KroneckerReduce::usage = "KroneckerReduce[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\)] reduces any instances of contracted Kronecker tensors in \!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\).";

DeleteFactor::usage = "DeleteFactor[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)] deletes the \*
StyleBox[\(\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)th\)] factor from \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\).";


EndPackage[]
