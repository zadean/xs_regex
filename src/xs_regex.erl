%% -------------------------------------------------------------------
%%
%% xs_regex - XML Schema regex translation
%%
%% Copyright (c) 2017-2018 Zachary N. Dean  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(xs_regex).

%% FLAGS - 'https://www.w3.org/TR/xpath-functions-31/#flags'
%% s: If present, the match operates in "dot-all" mode. (Perl calls this the 
%%    single-line mode.) If the s flag is not specified, the meta-character . 
%%    matches any character except a newline (#x0A) or carriage return (#x0D) 
%%    character. In dot-all mode, the meta-character . matches any character 
%%    whatsoever. Suppose the input contains "hello" and "world" on two lines. 
%%    This will not be matched by the regular expression "hello.*world" 
%%    unless dot-all mode is enabled.
%% m: If present, the match operates in multi-line mode. By default, the 
%%    meta-character ^ matches the start of the entire string, while $ matches 
%%    the end of the entire string. In multi-line mode, ^ matches the start of 
%%    any line (that is, the start of the entire string, and the position 
%%    immediately after a newline character other than a newline that appears 
%%    as the last character in the string), while $ matches the end of any line 
%%    (that is, the position immediately before a newline character, and the end 
%%    of the entire string if there is no newline character at the end of the 
%%    string). Newline here means the character #x0A only.
%% i: If present, the match operates in case-insensitive mode. The detailed 
%%    rules are as follows. In these rules, a character C2 is considered to 
%%    be a case-variant of another character C1 if the following XPath 
%%    expression returns true when the two characters are considered as strings 
%%    of length one, and the -Unicode codepoint collation- is used:
%%    Note that the case-variants of a character under this definition are 
%%    always single characters.
%%    When a normal character (Char) is used as an atom, it represents the set 
%%    containing that character and all its case-variants. For example, 
%%    the regular expression "z" will match both "z" and "Z".
%%    A character range (production charRange in the XSD 1.0 grammar, replaced 
%%    by productions charRange and singleChar in XSD 1.1) represents the set 
%%    containing all the characters that it would match in the absence of the 
%%    "i" flag, together with their case-variants. For example, the regular 
%%    expression "[A-Z]" will match all the letters A-Z and all the letters 
%%    a-z. It will also match certain other characters such as #x212A 
%%    (KELVIN SIGN), since fn:lower-case("#x212A") is "k".
%%    This rule applies also to a character range used in a character class 
%%    subtraction (charClassSub): thus [A-Z-[IO]] will match characters such 
%%    as "A", "B", "a", and "b", but will not match "I", "O", "i", or "o".
%%    The rule also applies to a character range used as part of a negative 
%%    character group: thus [^Q] will match every character except "Q" and "q" 
%%    (these being the only case-variants of "Q" in Unicode).
%%    A back-reference is compared using case-blind comparison: that is, each 
%%    character must either be the same as the corresponding character of the 
%%    previously matched string, or must be a case-variant of that character. 
%%    For example, the strings "Mum", "mom", "Dad", and "DUD" all match the 
%%    regular expression "([md])[aeiou]\1" when the "i" flag is used.
%%    All other constructs are unaffected by the "i" flag. For example, "\p{Lu}" 
%%    continues to match upper-case letters only.
%% x: If present, whitespace characters (#x9, #xA, #xD and #x20) in the regular 
%%    expression are removed prior to matching with one exception: whitespace 
%%    characters within character class expressions (charClassExpr) are not 
%%    removed. This flag can be used, for example, to break up long regular 
%%    expressions into readable lines.
%% q: if present, all characters in the regular expression are treated as 
%%    representing themselves, not as metacharacters. In effect, every 
%%    character that would normally have a special meaning in a regular 
%%    expression is implicitly escaped by preceding it with a backslash.
%%    Furthermore, when this flag is present, the characters $ and \ have no 
%%    special significance when used in the replacement string supplied to 
%%    the fn:replace function.
%%    This flag can be used in conjunction with the i flag. If it is used 
%%    together with the m, s, or x flag, that flag has no effect.

%% ====================================================================
%% API functions
%% ====================================================================
-export([normalize/1,
         translate/1]).

-export([compile/2]).

-export([transform_replace/2]).
-export([simple_escape/1]).
-export([get_depth/1]).

-type regex()  :: list(branch()).
-type branch() :: list({branch,piece()}).
-type piece()  :: {piece, re_atom(), one | quantifier()}.
-type re_atom() :: '^' | '$' | {char, char()} | char_class() | 
                   {paren, regex()} | {nc_paren, regex()} | 
                   {back_ref, integer()}.
-type char_class() :: string() | char_class_esc().
-type char_class_esc() :: {char_class, string()} | 
                          {neg_char_class, string()} |
                          char_group().
-type char_group() :: {group,     list(group_part())} |
                      {neg_group, list(group_part())} |
                      {subtract, char_group(), char_group()}.
-type group_part() :: {range, integer(), integer()} | {value, integer()} .
-type quantifier() :: {q, string()}.

%% Normalizes hex and decimal XML Character references in a string.
%% Example: "AB&#x20;C"   -> "AB C"
%%          "AB&#65296;C" -> [65, 66, 65296, 67]
-spec normalize(string()) -> {ok, string()}.
normalize(String) ->
   {ok, xs_regex_util:decode_string(String)}.

%% Translates an XML Schema regex string into Erlang flavor. 
-spec translate(string()) -> {ok, string()} | {error, _}.
translate([]) -> {ok, []};
translate(String) ->
   % Should not fail
   {ok,Tokens,_} = xs_regex_scanner:string(String),
   %io:format("~p~n",[Tokens]),
   case xs_regex_parser:parse(Tokens) of
      {ok,Tree} ->
         %io:format("~p~n",[Tree]),
         case translate_1(Tree) of
            {error,_} = Err ->
               Err;
            Trans ->
               {ok, Trans}
         end;
      {error,{_,_,O}} ->
         {error, {invalid_regex, lists:flatten(O)}}
   end.

%% simply escapes the backslash escape character in a string
-spec simple_escape(string()) -> string().
simple_escape([]) -> [];
simple_escape([$\\|T]) ->
   [$\\,$\\|simple_escape(T)];
simple_escape([H|T]) ->
   [H|simple_escape(T)].

%% Transforms an XML replacement string ('$' followed by decimal digit) 
%% to Erlang flavor ( \g{N} ). Uses depth to ensure correct capture.
-spec transform_replace(string(), non_neg_integer()) -> 
         {ok, string()} | {error, invalid_replacement}.
transform_replace(String, Depth) ->
   case transform_repl1(String,Depth) of
      {error,_} = Err ->
         Err;
      Repl ->
         {ok, Repl}
   end.

%% Returns the capturing pattern depth of a pattern.
%% Needed when transforming a replacement pattern.
-spec get_depth(string()) -> {ok, non_neg_integer()}.
get_depth(String) ->
   {ok, get_depth(String,0)}.

%% Matching the zero-length-string is (sometimes) an error in XQuery, 
%% so can be tested here. 

%% Takes a transformed regular expression as a string and a 
%% string of flag characters. 
%% Flag characters can only be "s, m, i, x and q". See comment above.
%% Returns {MatchesZeroLengthString, MP}
-spec compile(string(),string()) -> {boolean(), any()} | {error, _}.
compile(Expr0,Flags) ->
   try
      FlagList1 = regex_flags(Flags),
      X = lists:member(extended, FlagList1),
      FlagList = FlagList1 ++ [{newline, any}, unicode, ucp, no_start_optimize],
      Opts = FlagList -- [do_qe],
      Q = [ok || do_qe <- FlagList],
      Expr = if X ->
                   strip_esc_ws(Expr0);
                true ->
                   Expr0
             end,
      Expr1 = if Q == [] -> 
                    {ok, Tr} = translate(Expr),
                    Tr;
                 true -> "\\Q" ++ Expr ++ "\\E"
              end,
      {ok, MP} = re:compile(Expr1, Opts),
      case catch re:run("",MP) of
         nomatch ->
            {false,MP};
         {match,_} ->
            {true,MP};
         _ ->
            {false,MP}
      end
   catch 
      _:{error, {invalid_flag, _}} = E ->
         E;
      _:E -> 
         {error, {invalid_regex, E}}
   end.

%% ====================================================================
%% Internal functions
%% ====================================================================
-spec translate_1(regex()) -> string() | {group, group_part()} | {error, _}.
translate_1([H|T]) when not is_integer(H) -> % regex()
   translate_1(H) ++ lists:flatten(["|" ++ translate_1(X) || X <- T]);
translate_1([H|_] = Str) when is_integer(H) -> % string()
   Str;

translate_1({branch,Pieces}) ->
   HasBackRef = lists:any(fun(P) -> is_back_ref(P) end, Pieces),
   NewPieces = 
      if HasBackRef ->
            check_back_refs(Pieces);
         true ->
            Pieces
      end,
   {Hd,Tl,Rest} = maybe_strip_anchors(NewPieces),
   Out = [translate_1(X) || X <- Rest], 
   Hd ++ lists:flatten(Out) ++ Tl;

translate_1({piece, Atom, Quant}) ->
   Quant1 = check_quantifier(Quant),
   translate_1(Atom) ++ Quant1;
translate_1('^') -> "\\^";
translate_1('$') -> "\\$";
translate_1({back_ref,Int}) ->
   "\\g{" ++ integer_to_list(Int) ++ "}";
translate_1({char,C}) when is_list(C) -> C;
translate_1({char,C}) -> [C];
translate_1({char_class,Cc}) -> 
   Range = xs_regex_util:range(Cc),
   "(?-i:[" ++ xs_regex_util:range_to_regex(Range) ++ "])";
translate_1({neg_char_class,Cc}) -> 
   Range = xs_regex_util:range(Cc),
   "(?-i:[^" ++ xs_regex_util:range_to_regex(Range) ++ "])";
translate_1({paren,RegEx}) ->
   "("++ translate_1(RegEx) ++")";
translate_1({nc_paren,RegEx} ) ->
   "(?:"++ translate_1(RegEx) ++")";
translate_1({RegEx,{q,Quant}} ) ->
   translate_1(RegEx) ++ Quant;

translate_1({group,_} = G) ->
   translate_group(G);
translate_1({neg_group,_} = G) ->
   translate_group(G);
translate_1({subtract,G1,G2}) ->
   translate_group({subtract,
                    combine_group(G1),
                    combine_group(G2)});
translate_1(Int) when is_integer(Int) ->
   Int;
translate_1(Tree) ->
   {error, {unknown,Tree}}.

maybe_strip_anchors([{piece,'^',_}]) -> {"^","",[]};
maybe_strip_anchors([{piece,'^',_}|Pieces]) ->
   Hd = "^",
   case lists:last(Pieces) of
      {piece,'$',_} ->
         {Hd,"$",lists:droplast(Pieces)};
      _ ->
         {Hd,"",Pieces}
   end;
maybe_strip_anchors(Pieces) ->
   case lists:last(Pieces) of
      {piece,'$',_} ->
         {"","$",lists:droplast(Pieces)};
      _ ->
         {"","",Pieces}
   end.

translate_group({group, G}) ->
   ok = no_back_ref(G),
   case is_all_value(G) of
      {true,Str} ->
         "["++Str++"]";
      false ->
         NewGroup = combine_group({group, G}),
         translate_group(NewGroup)
   end;
translate_group({neg_group, [{neg_char_class,Neg}]}) -> % double negative
   translate_group({group, [{char_class,Neg}]});
translate_group({neg_group, G}) ->
   ok = no_back_ref(G),
   case is_all_value(G) of
      {true,Str} ->
         "[^"++Str++"]";
      false ->
         NewGroup = combine_group({neg_group, G}),
         translate_group(NewGroup)
   end;
translate_group({subtract,{group,_} = G1,{neg_group,_} = G2}) ->
   S1 = translate_group_as_set(G1),
   S2 = translate_group_as_set(G2),
   S3 = xs_regex_util:intersection(S1, S2), % sub neg == intersect
   "[" ++ xs_regex_util:range_to_regex(S3) ++ "]";
translate_group({subtract,{neg_group,R0} = G1,{group,_} = G2}) ->
   S1 = translate_group_as_set(G1),
   S2 = translate_group_as_set(G2),
   S3 = xs_regex_util:subtract(S1, S2), % sub pos from neg == neg sub
   case xs_regex_util:range_to_regex(S3) of
      [] ->
         "^[" ++ xs_regex_util:range_to_regex(R0) ++ "]";
      M ->
         "^[" ++ M ++ "]"
   end;
translate_group({subtract,G1,G2}) ->
   S1 = translate_group_as_set(G1),
   S2 = translate_group_as_set(G2),
   S3 = xs_regex_util:subtract(S1, S2),
   case xs_regex_util:range_to_regex(S3) of
      [] ->
         "";
      M ->
         "[" ++ M ++ "]"
   end;
translate_group(_) ->
   {error, translate_group}.

combine_group({Type,List}) when Type == group;
                                Type == neg_group ->
   List1 = lists:map(fun({neg_char_class,C}) ->
                           G = xs_regex_util:range(C),
                           {neg_group, G};
                        ({char_class,C}) ->
                           G = xs_regex_util:range(C),
                           {group, G};
                        (O) ->
                           O
                     end, List),
   Negatives = [N || N <- List1, element(1, N) == neg_group],
   Positives = [N || N <- List1, element(1, N) == group],
   Rest = [N || N <- List1, 
                element(1, N) =/= group, 
                element(1, N) =/= neg_group],
   Pos = lists:foldl(fun({group,R},Acc) ->
                           S = xs_regex_util:range_to_set(R),
                           xs_regex_util:union(Acc, S)
                     end, xs_regex_util:range_to_set(Rest), Positives),
   if Negatives == [] ->
         {Type,Pos};
      true ->
         Neg = lists:foldl(fun({neg_group,R},Acc) ->
                                 S = xs_regex_util:range_to_set(R),
                                 xs_regex_util:union(Acc, S)
                           end, xs_regex_util:range_to_set([]), Negatives),
         Rest1 = xs_regex_util:subtract(Neg, Pos),
         if Type == group ->
               {neg_group,Rest1};
            true ->
               {Type,Rest1}
         end
   end;
combine_group(List) ->
   Fun = fun({char_class,Name}) ->
               xs_regex_util:range(Name);
            ({value,_} = V) ->
               [V];
            ({range,_,_} = R) ->
               [R]
         end,
   Ranges = lists:map(Fun, List),
   SetHd = xs_regex_util:range_to_set(hd(Ranges)),
   lists:foldl(fun(R,A) ->
                     S = xs_regex_util:range_to_set(R),
                     xs_regex_util:union(A,S)
               end, SetHd, tl(Ranges)).

translate_group_as_set({neg_group,_} = G) ->
   {_,R} = combine_group(G),
   xs_regex_util:range_to_set(R);
translate_group_as_set({group,_} = G) ->
   {_,R} = combine_group(G),
   xs_regex_util:range_to_set(R).

is_all_value(G) ->
   All = lists:all(fun({value,_}) ->
                         true;
                      ({range,_,_}) ->
                         true;
                      (_) ->
                         false
                   end, G),
   if All ->
         {true, xs_regex_util:range_to_regex(G)};
      true ->
         false
   end.

no_back_ref([]) -> ok;
no_back_ref([{value,$\\},{value,N}|_]) when N >= $0, N =< $9 ->
   {error, group_backref};
no_back_ref([_|T]) ->
   no_back_ref(T).

check_quantifier(one) -> "";
check_quantifier({q,"{," ++ _}) ->
   {error, no_min};
check_quantifier({q,Quant}) ->
   Quant.

is_back_ref({_,{back_ref,_},_}) -> true;
is_back_ref(_) -> false.

count_capturing_patterns(Pieces) ->
   count_capturing_patterns(Pieces, 0).

count_capturing_patterns(Term, Cnt) when is_list(Term) ->
   Cnts = [count_capturing_patterns(T, 0) || T <- Term],
   lists:foldl(fun(I,To) ->
                     I + To
               end, Cnt, Cnts);
count_capturing_patterns({branch,Term}, Cnt) ->
   count_capturing_patterns(Term, Cnt);
count_capturing_patterns({paren,Term}, Cnt) ->
   count_capturing_patterns(Term, Cnt + 1);
count_capturing_patterns(Term, Cnt) when is_tuple(Term) ->
   Cnts = [count_capturing_patterns(T, 0) || T <- tuple_to_list(Term)],
   lists:foldl(fun(I,To) ->
                     I + To
               end, Cnt, Cnts);
count_capturing_patterns(_, Cnt) ->
   Cnt.

check_back_refs(Pieces) ->
   check_back_refs(Pieces,[]).

check_back_refs([],Acc) ->
   lists:reverse(Acc);
check_back_refs([{piece,{back_ref,N},one} = H|T], Acc) ->
   C = count_capturing_patterns(Acc),
   if N > C, N < 10 ->
         {error, badbackref};
      N > C ->
         Rem = N rem 10,
         Div = N div 10,
         if Div == 0 ->
               {error, badbackref};
            true ->
               check_back_refs([{piece,{back_ref,Div},one},
                                {piece,{char,integer_to_list(Rem)},one}|T], Acc)
         end;
      true ->
         check_back_refs(T, [H|Acc])
   end;
check_back_refs([H|T],Acc) ->
   check_back_refs(T, [H|Acc]).

regex_flags([]) -> [dollar_endonly];
regex_flags(Flags) ->
   % uses a map to only save last occurance
   Fun = fun(Char, Map) ->
               case Char of
                  $s -> maps:put(s, [dollar_endonly,dotall], Map);
                  $m -> maps:put(m, multiline, Map);
                  $i -> maps:put(i, caseless, Map);
                  $x -> maps:put(x, extended, Map);
                  $q -> maps:put(q, do_qe, Map);
                  C ->
                     throw({error, {invalid_flag, C}})
               end
         end,        
   M = lists:foldl(Fun, #{}, Flags),
   lists:flatten(maps:values(M)).

get_depth([],D) -> D;
get_depth([$)|T],D) ->
   get_depth(T,D+1);
get_depth([_|T],D) ->
   get_depth(T,D).

transform_repl1([],_) -> [];
transform_repl1([$\\,$$|T],D) ->
   [$\\,$$|transform_repl1(T,D)];
transform_repl1([$\\,$\\|T],D) ->
   [$\\,$\\|transform_repl1(T,D)];
transform_repl1([$\\|_],_) ->
   {error, invalid_replacement};
transform_repl1([$$,H2|T],D) when H2 >= $0, H2 =< $9 ->
   {Nums,Rest} = get_digits(T,[]),
   Int = list_to_integer([H2|Nums]),
   {NewInt,Tail} = chop_to(Int, D, []),
   BR = "\\g{" ++ NewInt ++ "}" ++ Tail,
   BR ++ transform_repl1(Rest,D);
transform_repl1([$$|_],_) ->
   {error, invalid_replacement};
transform_repl1([H|T],D) ->
   [H|transform_repl1(T,D)].

get_digits([],Acc) -> 
   {lists:reverse(Acc),[]};
get_digits([H|T],Acc) when H >= $0, H =< $9 -> 
   get_digits(T,[H|Acc]);
get_digits([H|T],Acc) -> 
   {lists:reverse(Acc),[H|T]}.

%returns {IntAsList,Tail}
chop_to(Int,Max,Acc) when Int > Max ->
   Next = Int div 10,
   Rem = integer_to_list(Int rem 10),
   chop_to(Next,Max,Rem ++ Acc);
chop_to(Int,_Max,Acc) ->
   {integer_to_list(Int), Acc}.

strip_esc_ws([]) -> [];
strip_esc_ws([$[|T]) -> 
   [$[|no_strip_esc_ws(T)];
strip_esc_ws([H|T]) when H == 16#9;
                         H == 16#A;
                         H == 16#D;
                         H == 16#20 ->
   strip_esc_ws(T);
strip_esc_ws([H|T]) -> 
   [H|strip_esc_ws(T)].

no_strip_esc_ws([]) -> [];
no_strip_esc_ws([$]|T]) -> 
   [$]|strip_esc_ws(T)];
no_strip_esc_ws([H|T]) -> 
   [H|no_strip_esc_ws(T)].
                            
