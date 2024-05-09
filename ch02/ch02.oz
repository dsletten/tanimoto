%%%
%%%   Name:               ch02.oz
%%%
%%%   Started:            Sat May 22 05:22:30 2004
%%%   Modifications:
%%%
%%%   Purpose:
%%%
%%%
%%%
%%%   Calling Sequence:
%%%
%%%
%%%   Inputs:
%%%
%%%   Outputs:
%%%
%%%   Example:
%%%
%%%   Notes:
%%%
%%%

%%
%%    Ex. 6
%%
declare
fun {Exify Expr}
   case Expr
   of nil then nil
   [] H|T then
      {Exify H}|{Exify T}
   else x
   end
end
{Browse {Exify [a b|c x y nil z]}}

%%
%%    Ex. 7
%%
declare
fun {Replace S1 S2 S3}
   if S1 == nil then nil
   elseif S1 == S2 then S3
   else
      case S1
      of H|T then
	 {Replace H S2 S3}|{Replace T S2 S3}
      else S1
      end
   end
end
{Browse {Replace [[this 1] contains [2 occurrences [this 1]]]
	 [this 1] [that one]}}

%%
%%    Ex. 8
%%
declare
fun {Cubes N}
   fun {CubesAux I Result}
      if I == 0 then Result
      else {CubesAux I-1 {Pow I 3}|Result}
      end
   end
in
   {CubesAux N nil}
end
{Browse {Cubes 15}}

declare
fun {Cubes N}
   {Map {List.number 1 N 1} fun {$ X} {Pow X 3} end}
end
{Browse {Cubes 15}}

%%
%%    Ex. 10
%%
declare
fun {Sum Ilist}
   case Ilist
   of nil then 0
   [] H|T then
      H + {Sum T}
   end
end
{Browse {Sum [3 7 11 13]}}

%%
%%    Ex. 11
%%
declare
fun {Palindrome L}
   L == {Reverse L}
end
{Browse {Palindrome [a b a c a b a]}}
{Browse {Palindrome [a b a c d a b a]}}

%%
%%    Ex. 13
%%
declare
fun {DoubleElts L}
   {Map L fun {$ Elt} [Elt Elt] end}
end
{Browse {DoubleElts [x y [z w]]}}

%%
%%    Ex. 14
%%
declare
fun {EqualElts L}
   case L
   of nil then true
   [] _|nil then true
   [] H|T then
      if H == T.1 then {EqualElts T}
      else false
      end
   end
end
{Browse {EqualElts [a a a a]}}
{Browse {EqualElts [a a a b a]}}
{Browse {EqualElts [[a b] [a b] [a b]]}}

declare
fun {EqualElts L}
   H|T = L
in
   {All T fun {$ Elt} Elt == H end}
end
{Browse {EqualElts [a a a a]}}
{Browse {EqualElts [a a a b a]}}
{Browse {EqualElts [[a b] [a b] [a b]]}}

%%
%%    Ex. 15
%%
declare
fun {TreeDepth L}
   case L
   of H|T then 1 + {Max {TreeDepth H} {TreeDepth T}}
   else 0
   end
end
{ForAll [a nil [a b c] [[[a] b] c [[d] e]] [a [b] [c [d]]]]
 proc {$ L} {Browse {TreeDepth L}} end}

declare
fun {QuasiBalancedP L}
   {EqualElts {Map L Length}} andthen
   {EqualElts {Map L TreeDepth}}
end
{Browse {QuasiBalancedP [[a b] [c d] [e f]]}}
{Browse {QuasiBalancedP [[a b] [c d] [e f g]]}}
{Browse {QuasiBalancedP [[a b] [c d] [e [f]]]}}
