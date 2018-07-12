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

%% @doc  Module for unicode range lookup and combination.

-module(xs_regex_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([range/1]).

-export([range_to_set/1,
         range_to_regex/1,
         decode_string/1]).

-export([intersection/2,
         union/2,
         subtract/2,
         symmetric_diff/2]).

-type range() :: list({range, non_neg_integer(), non_neg_integer()} |
                      {value, non_neg_integer()}).
-type regex() :: string().

%% When given a range, returns its regex string representation.
-spec range_to_regex(range()) -> regex().
range_to_regex(Range) ->
   %io:format("~p~n",[Range]),
   Fun = fun({value,Val},Acc) ->
               hex(Val) ++ Acc;
            ({range,Val,Val},Acc) ->
               hex(Val) ++ Acc;
            ({range,Min,Max},Acc) ->
               hex(Min) ++ "-" ++ hex(Max) ++ Acc
         end,
   lists:foldr(Fun, "", Range).

%% Sorts a range.
-spec range_to_set(range()) -> range().
range_to_set(Range) -> lists:keysort(2, Range).

% A∩B Intersection of two ranges.
-spec intersection(range(), range()) -> range().
intersection([],_) -> [];
intersection(_,[]) -> [];
% simple values
intersection([{value,V1}|T1],[{value,V2}|T2]) when V1 < V2  ->
   intersection(T1,[{value,V2}|T2]);
intersection([{value,V1}|T1],[{value,V2}|T2]) when V1 > V2  ->
   intersection([{value,V1}|T1],T2);
intersection([{value,V1}|T1],[{value,_}|T2]) -> % ==
   [{value,V1}|intersection(T1,T2)];
% value | range
intersection([{value,V1}|T1],[{range,V2,_}|_] = R2) when V1 < V2 ->
   intersection(T1,R2);
intersection([{value,V1}|_] = R1,[{range,_,M2}|T2]) when V1 > M2 ->
   intersection(R1,T2);
intersection([{value,V1}|T1],[{range,_,M2}|T2]) when V1 == M2 ->
   [{value,V1}|intersection(T1,T2)];
intersection([{value,V1}|T1],[{range,V2,_}|_] = R2) when V1 == V2 ->
   [{value,V1}|intersection(T1,R2)];
intersection([{value,V1}|T1],[{range,V2,M2}|T2]) when V1 >= V2, V1 =< M2 ->
   [{value,V1}|intersection(T1,[{range,V1,M2}|T2])];
% range | value = switch
intersection([{range,_,_}|_] = R1, [{value,_}|_] = R2) ->
   intersection(R2,R1);

% range | range
intersection([{range,_,M1}|T1], [{range,V2,_}|_] = R2) when M1 < V2 ->
   intersection(T1,R2);
intersection([{range,_,M1}|T1], [{range,V2,_}|_] = R2) when M1 == V2 ->
   [{value,M1}|intersection(T1,R2)];
intersection([{range,V1,_}|_] = R1, [{range,_,M2}|T2]) when V1 > M2 ->
   intersection(R1,T2);
intersection([{range,V1,_}|_] = R1, [{range,_,M2}|T2]) when V1 == M2 ->
   [{value,M2}|intersection(R1,T2)];
intersection([{range,V1,M1}|T1], [{range,V2,M2}|_] = R2) when V1 >= V2, M1 =< M2 ->
   [{range,V1,M1}|intersection(T1,R2)];
intersection([{range,V1,M1}|_] = R1, [{range,V2,M2}|T2]) when V2 >= V1, M2 =< M1 ->
   [{range,V2,M2}|intersection(R1,T2)];
intersection([{range,V1,M1}|T1], [{range,V2,M2}|T2]) when V1 < V2, M1 < M2 ->
   [{range,V2,M1}|intersection(T1,[{range,M1,M2}|T2])];
intersection([{range,V1,M1}|T1], [{range,V2,M2}|T2]) when V2 < V1, M2 < M1 ->
   [{range,V1,M2}|intersection([{range,M2,M1}|T1],T2)];
intersection(Set1,Set2) ->
   throw({error,Set1,Set2}).


%% A∪B Union of two ranges.
-spec union(range(), range()) -> range().
union([],R2) -> R2;
union(R1,[]) -> R1;

% value | value
union([{value,V1}|T1],[{value,V2}|T2]) when V1 + 1 == V2 ->
   union(T1,[{range,V1,V2}|T2]);
union([{value,V1}|T1],[{value,V2}|T2]) when V1 < V2 ->
   [{value,V1}|union(T1,[{value,V2}|T2])];
union([{value,V1}|T1],[{value,V2}|T2]) when V1 == V2 + 1 ->
   union([{range,V2,V1}|T1],T2);
union([{value,V1}|T1],[{value,V2}|T2]) when V1 > V2 ->
   [{value,V2}|union([{value,V1}|T1],T2)];

union([{value,V1}|T1],[{value,_}|T2]) ->
   [{value,V1}|union(T1,T2)];

% value | range
union([{value,V1}|T1],[{range,V2,M2}|T2]) when V1 + 1 == V2 ->
   union(T1, [{range,V1,M2}|T2]);
union([{value,V1}|T1],[{range,V2,M2}|T2]) when M2 + 1 == V1 ->
   union([{range,V2,V1}|T1], T2);
union([{value,V1}|T1],[{range,V2,M2}|_] = R2) when V1 >= V2, V1 =< M2 ->
   union(T1,R2);
union([{value,V1}|T1],[{range,V2,_}|_] = R2) when V1 < V2 ->
   [{value,V1}|union(T1,R2)];
union([{value,V1}|_] = R1,[{range,V2,M2}|T2]) when V1 > M2 ->
   [{range,V2,M2}|union(R1,T2)];

% range | value = switch
union([{range,_,_}|_] = R1,[{value,_}|_] = R2) ->
   union(R2,R1);

% range | range
union([{range,V1,M1}|T1],[{range,V2,M2}|T2]) when M1 + 1 < V2 ->
   [{range,V1,M1}|union(T1,[{range,V2,M2}|T2])];

union([{range,V1,M1}|T1],[{range,V2,M2}|T2]) when M2 + 1 < V1 ->
   [{range,V2,M2}|union([{range,V1,M1}|T1],T2)];

union([{range,V1,M1}|T1],[{range,V2,M2}|T2]) when V1 < V2, M1 + 1 =< M2 ->
   union(T1,[{range,V1,M2}|T2]);

union([{range,V1,M1}|T1],[{range,V2,M2}|T2]) when V1 >= V2 + 1, M1 > M2 ->
   union([{range,V2,M1}|T1],T2);

union([{range,V1,M1}|T1],[{range,V2,M2}|_T2] = R2) when V1 >= V2, M1 =< M2 ->
   union(T1,R2);

union([{range,V1,M1}|_T1] = R1,[{range,V2,M2}|T2]) when V2 >= V1, M2 =< M1 ->
   union(R1,T2);

% other
union(R1,R2) -> 
   throw({unknown,R1,R2}).

%% A-B Range A minus Range B.
-spec subtract(range(), range()) -> range().
subtract(R1,[]) -> R1;
subtract([],_) -> [];

subtract([{value,V1}|_] = R1,[{value,V2}|T2]) when V1 > V2 -> 
   subtract(R1,T2);
subtract([{value,V1}|T1],[{value,V2}|_] = R2) when V1 < V2 -> 
   [{value,V1}|subtract(T1,R2)];
subtract([{value,_}|T1],[{value,_}|T2]) -> 
   subtract(T1,T2);

subtract([{value,V1}|T1],[{range,V2,_}|_] = R2) when V1 < V2 -> 
   [{value,V1}|subtract(T1,R2)];
subtract([{value,V1}|_] = R1,[{range,_,M2}|T2]) when V1 > M2 -> 
   subtract(R1,T2);
subtract([{value,V1}|T1],[{range,V2,M2}|_] = R2) when V1 >= V2, V1 =< M2 -> 
   subtract(T1,R2);

subtract([{range,V1,M1}|T1], [{value,V2}|_] = R2) when M1 < V2 -> 
   [{range,V1,M1}|subtract(T1,R2)];
subtract([{range,V1,_}|_] = R1, [{value,V2}|T2]) when V1 > V2 -> 
   subtract(R1,T2);
subtract([{range,V1,M1}|T1], [{value,V2}|T2]) when M1 == V2, M1 == V1 + 1 -> 
   [{value,V1}|subtract(T1,T2)];
subtract([{range,V1,M1}|T1], [{value,V2}|T2]) when M1 == V2 -> 
   [{range,V1,M1 - 1}|subtract(T1,T2)];
subtract([{range,V1,M1}|T1], [{value,V2}|T2]) when V1 == V2, M1 == V1 + 1 -> 
   subtract([{value,M1}|T1],T2);
subtract([{range,V1,M1}|T1], [{value,V2}|T2]) when V1 == V2 -> 
   subtract([{range,V1 + 1,M1}|T1],T2);


subtract([{range,V1,M1}|T1], [{value,V2}|T2]) when V2 == V1 + 1, V2 == M1 - 1 -> 
   [{value,V1}|subtract([{value,M1}|T1],T2)];
subtract([{range,V1,M1}|T1], [{value,V2}|T2]) when V2 == V1 + 1, V2 =< M1 -> 
   [{value,V1}|subtract([{range,V2 + 1,M1}|T1],T2)];
subtract([{range,V1,M1}|T1], [{value,V2}|T2]) when V2 > V1, V2 == M1 - 1 -> 
   [{range,V1, V2 - 1}|subtract([{value,M1}|T1],T2)];
subtract([{range,V1,M1}|T1], [{value,V2}|T2]) when V2 > V1, V2 =< M1 -> 
   [{range,V1,V2 - 1}|subtract([{range,V2 + 1,M1}|T1],T2)];

% range | range
subtract([{range,V1,M1}|T1], [{range,V2,_}|_] = R2) when M1 < V2 -> 
   [{range,V1,M1}|subtract(T1,R2)];
subtract([{range,V1,_}|_] = R1, [{range,_,M2}|T2]) when V1 > M2 -> 
   subtract(R1,T2);
subtract([{range,V1,M1}|T1], [{range,V2,M2}|_] = R2) when V1 >= V2, M1 =< M2 -> 
   subtract(T1,R2);

subtract([{range,V1,M1}|T1], [{range,V2,M2}|T2]) when V1 == V2, M1 == M2 -> 
   subtract(T1,T2);
subtract([{range,V1,M1}|T1], [{range,V2,M2}|T2]) when V1 == V2, M1 == M2 + 1 -> 
   subtract([{value,M1}|T1],T2);
subtract([{range,V1,M1}|T1], [{range,V2,M2}|T2]) when V1 == V2, M1 > M2 -> 
   subtract([{range,M2 + 1, M1}|T1],T2);
subtract([{range,V1,M1}|T1], [{range,V2,M2}|_] = R2) when V1 == V2, M1 < M2 -> 
   subtract(T1,R2);

subtract([{range,V1,M1}|T1], [{range,V2,M2}|_] = R2) when V1 + 1 == V2, M1 == M2 + 1 -> 
   [{value,V1}|subtract([{value,M1}|T1],R2)];
subtract([{range,V1,M1}|T1], [{range,V2,M2}|_] = R2) when V1 + 1 == V2, M1 =< M2 -> 
   [{value,V1}|subtract(T1,R2)];
subtract([{range,V1,M1}|T1], [{range,V2,M2}|T2]) when V1 + 1 == V2 -> 
   [{value,V1}|subtract([{range,M2 + 1, M1}|T1],T2)];

subtract([{range,V1,M1}|T1], [{range,V2,M2}|_] = R2) when V1 < V2, M1 == M2 + 1 -> 
   [{range,V1, V2 - 1}|subtract([{value,M1}|T1],R2)];
subtract([{range,V1,M1}|T1], [{range,V2,M2}|_] = R2) when V1 < V2, M1 =< M2 -> 
   [{range,V1, V2 - 1}|subtract(T1,R2)];
subtract([{range,V1,M1}|T1], [{range,V2,M2}|T2]) when V1 < V2 -> 
   [{range,V1, V2 - 1}|subtract([{range,M2 + 1, M1}|T1],T2)];

subtract([{range,V1,M1}|T1], [{range,V2,M2}|T2]) when V1 > V2, M1 == M2 + 1 -> 
   subtract([{value,M1}|T1],T2);
subtract([{range,V1,M1}|T1], [{range,V2,M2}|T2]) when V1 > V2, M1 > M2 -> 
   subtract([{range,M2 + 1, M1}|T1],T2);

subtract(Set1,Set2) ->
   throw({error,{subtract,Set1,Set2}}).

%% A⊖B Symmetric difference of two ranges.
-spec symmetric_diff(range(), range()) -> range().
symmetric_diff(Set1,Set2) ->
   SetU = union(Set1, Set2),
   SetI = intersection(Set1, Set2),
   subtract(SetU, SetI).

hex(Int) when $a =< Int, Int =< $z -> [Int];
hex(Int) when $0 =< Int, Int =< $9 -> [Int];
hex(Int) when $A =< Int, Int =< $Z -> [Int];
hex(Int) ->
   "\\x{"++integer_to_list(Int,16)++"}".

%% Replaces character references with their codepoints in a string.
-spec decode_string(string()) -> string().
decode_string(Str) ->
   decode_string(Str,[]).

decode_string([], Acc) ->
   lists:reverse(Acc);
decode_string("&apos;" ++ T, Acc) ->
   decode_string(T, [$'|Acc]);
decode_string("&quot;" ++ T, Acc) ->
   decode_string(T, [$"|Acc]);
decode_string("&amp;" ++ T, Acc) ->
   decode_string(T, [$&|Acc]);
decode_string("&gt;" ++ T, Acc) ->
   decode_string(T, [$>|Acc]);
decode_string("&lt;" ++ T, Acc) ->
   decode_string(T, [$<|Acc]);
decode_string("&#x" ++ T, Acc) ->
   {CP,T1} = scan_hex_char_ref(T, []),
   decode_string(T1, [CP|Acc]);
decode_string("&#" ++ T, Acc) ->
   {CP,T1} = scan_dec_char_ref(T, []),
   decode_string(T1, [CP|Acc]);
decode_string([H|T], Acc) ->
    decode_string(T, [H|Acc]).

scan_dec_char_ref([H|T], Acc) when H >= $0, H =< $9  ->
  scan_dec_char_ref(T, [H|Acc]);
scan_dec_char_ref([H|T], Acc) when H == $; ->
   {list_to_integer(lists:reverse(Acc)),T}.

scan_hex_char_ref([H|T], Acc) when H >= $0, H =< $9;
                                   H >= $a, H =< $f;
                                   H >= $A, H =< $F ->
   scan_hex_char_ref(T, [H|Acc]);
scan_hex_char_ref([H|T], Acc) when H == $; ->
   Hex = lists:reverse(Acc),
   {list_to_integer(Hex, 16),T}.


% http://www.unicode.org/reports/tr18/ "The values for these properties must 
% follow the Unicode definitions, and include the property and property value 
% aliases from the UCD. Matching of Binary, Enumerated, Catalog, and Name 
% values, must follow the Matching Rules from [UAX44] with one exception: 
% implementations are not required to ignore an initial prefix string of "is" 
% in property values."
% https://www.w3.org/TR/xmlschema-2/#regexs


%% Returns the range() of all unicode character codepoints for a given property.
-spec range(string()) -> range().
range("IsBasicLatin")                            -> [{range, 16#0000, 16#007F}];
range("IsLatin-1Supplement")                     -> [{range, 16#0080, 16#00FF}];
range("IsLatinExtended-A")                       -> [{range, 16#0100, 16#017F}];
range("IsLatinExtended-B")                       -> [{range, 16#0180, 16#024F}];
range("IsIPAExtensions")                         -> [{range, 16#0250, 16#02AF}];
range("IsSpacingModifierLetters")                -> [{range, 16#02B0, 16#02FF}];
range("IsCombiningDiacriticalMarks")             -> [{range, 16#0300, 16#036F}];
range("IsGreek")                                 -> [{range, 16#0370, 16#03FF}];
range("IsCyrillic")                              -> [{range, 16#0400, 16#04FF}];
range("IsArmenian")                              -> [{range, 16#0530, 16#058F}];
range("IsHebrew")                                -> [{range, 16#0590, 16#05FF}];
range("IsArabic")                                -> [{range, 16#0600, 16#06FF}];
range("IsSyriac")                                -> [{range, 16#0700, 16#074F}];
range("IsThaana")                                -> [{range, 16#0780, 16#07BF}];
range("IsDevanagari")                            -> [{range, 16#0900, 16#097F}];
range("IsBengali")                               -> [{range, 16#0980, 16#09FF}];
range("IsGurmukhi")                              -> [{range, 16#0A00, 16#0A7F}];
range("IsGujarati")                              -> [{range, 16#0A80, 16#0AFF}];
range("IsOriya")                                 -> [{range, 16#0B00, 16#0B7F}];
range("IsTamil")                                 -> [{range, 16#0B80, 16#0BFF}];
range("IsTelugu")                                -> [{range, 16#0C00, 16#0C7F}];
range("IsKannada")                               -> [{range, 16#0C80, 16#0CFF}];
range("IsMalayalam")                             -> [{range, 16#0D00, 16#0D7F}];
range("IsSinhala")                               -> [{range, 16#0D80, 16#0DFF}];
range("IsThai")                                  -> [{range, 16#0E00, 16#0E7F}];
range("IsLao")                                   -> [{range, 16#0E80, 16#0EFF}];
range("IsTibetan")                               -> [{range, 16#0F00, 16#0FFF}];
range("IsMyanmar")                               -> [{range, 16#1000, 16#109F}];
range("IsGeorgian")                              -> [{range, 16#10A0, 16#10FF}];
range("IsHangulJamo")                            -> [{range, 16#1100, 16#11FF}];
range("IsEthiopic")                              -> [{range, 16#1200, 16#137F}];
range("IsCherokee")                              -> [{range, 16#13A0, 16#13FF}];
range("IsUnifiedCanadianAboriginalSyllabics")    -> [{range, 16#1400, 16#167F}];
range("IsOgham")                                 -> [{range, 16#1680, 16#169F}];
range("IsRunic")                                 -> [{range, 16#16A0, 16#16FF}];
range("IsKhmer")                                 -> [{range, 16#1780, 16#17FF}];
range("IsMongolian")                             -> [{range, 16#1800, 16#18AF}];
range("IsLatinExtendedAdditional")               -> [{range, 16#1E00, 16#1EFF}];
range("IsGreekExtended")                         -> [{range, 16#1F00, 16#1FFF}];
range("IsGeneralPunctuation")                    -> [{range, 16#2000, 16#206F}];
range("IsSuperscriptsandSubscripts")             -> [{range, 16#2070, 16#209F}];
range("IsCurrencySymbols")                       -> [{range, 16#20A0, 16#20CF}];
range("IsCombiningMarksforSymbols")              -> [{range, 16#20D0, 16#20FF}];
range("IsLetterlikeSymbols")                     -> [{range, 16#2100, 16#214F}];
range("IsNumberForms")                           -> [{range, 16#2150, 16#218F}];
range("IsArrows")                                -> [{range, 16#2190, 16#21FF}];
range("IsMathematicalOperators")                 -> [{range, 16#2200, 16#22FF}];
range("IsMiscellaneousTechnical")                -> [{range, 16#2300, 16#23FF}];
range("IsControlPictures")                       -> [{range, 16#2400, 16#243F}];
range("IsOpticalCharacterRecognition")           -> [{range, 16#2440, 16#245F}];
range("IsEnclosedAlphanumerics")                 -> [{range, 16#2460, 16#24FF}];
range("IsBoxDrawing")                            -> [{range, 16#2500, 16#257F}];
range("IsBlockElements")                         -> [{range, 16#2580, 16#259F}];
range("IsGeometricShapes")                       -> [{range, 16#25A0, 16#25FF}];
range("IsMiscellaneousSymbols")                  -> [{range, 16#2600, 16#26FF}];
range("IsDingbats")                              -> [{range, 16#2700, 16#27BF}];
range("IsBraillePatterns")                       -> [{range, 16#2800, 16#28FF}];
range("IsCJKRadicalsSupplement")                 -> [{range, 16#2E80, 16#2EFF}];
range("IsKangxiRadicals")                        -> [{range, 16#2F00, 16#2FDF}];
range("IsIdeographicDescriptionCharacters")      -> [{range, 16#2FF0, 16#2FFF}];
range("IsCJKSymbolsandPunctuation")              -> [{range, 16#3000, 16#303F}];
range("IsHiragana")                              -> [{range, 16#3040, 16#309F}];
range("IsKatakana")                              -> [{range, 16#30A0, 16#30FF}];
range("IsBopomofo")                              -> [{range, 16#3100, 16#312F}];
range("IsHangulCompatibilityJamo")               -> [{range, 16#3130, 16#318F}];
range("IsKanbun")                                -> [{range, 16#3190, 16#319F}];
range("IsBopomofoExtended")                      -> [{range, 16#31A0, 16#31BF}];
range("IsEnclosedCJKLettersandMonths")           -> [{range, 16#3200, 16#32FF}];
range("IsCJKCompatibility")                      -> [{range, 16#3300, 16#33FF}];
range("IsCJKUnifiedIdeographsExtensionA")        -> [{range, 16#3400, 16#4DB5}];
range("IsCJKUnifiedIdeographs")                  -> [{range, 16#4E00, 16#9FFF}];
range("IsYiSyllables")                           -> [{range, 16#A000, 16#A48F}];
range("IsYiRadicals")                            -> [{range, 16#A490, 16#A4CF}];
range("IsHangulSyllables")                       -> [{range, 16#AC00, 16#D7A3}];
range("IsPrivateUse")                            -> [{range, 16#E000,   16#F8FF},
                                                     {range, 16#F0000,  16#FFFFD},
                                                     {range, 16#100000, 16#10FFFD}];
range("IsCJKCompatibilityIdeographs")            -> [{range, 16#F900, 16#FAFF}];
range("IsAlphabeticPresentationForms")           -> [{range, 16#FB00, 16#FB4F}];
range("IsArabicPresentationForms-A")             -> [{range, 16#FB50, 16#FDFF}];
range("IsCombiningHalfMarks")                    -> [{range, 16#FE20, 16#FE2F}];
range("IsCJKCompatibilityForms")                 -> [{range, 16#FE30, 16#FE4F}];
range("IsSmallFormVariants")                     -> [{range, 16#FE50, 16#FE6F}];
range("IsArabicPresentationForms-B")             -> [{range, 16#FE70, 16#FEFE}];
range("IsHalfwidthandFullwidthForms")            -> [{range, 16#FF00, 16#FFEF}];
range("IsSpecials")                              -> [{value, 16#FEFF},{range, 16#FFF0, 16#FFFD}];
% added
range("IsLowSurrogates")                         -> [{value, 16#0}]; % faked with 0, illegal characters
range("IsHighSurrogates")                        -> [{value, 16#0}]; % faked with 0, illegal characters
range("IsOldItalic")                             -> [{range, 16#10300, 16#1032F}];
range("IsGothic")                                -> [{range, 16#10330, 16#1034A}];
range("IsDeseret")                               -> [{range, 16#10400, 16#1044F}];
range("IsByzantineMusicalSymbols")               -> [{range, 16#1D000, 16#1D0FF}];
range("IsMusicalSymbols")                        -> [{range, 16#1D100, 16#1D1FF}];
range("IsMathematicalAlphanumericSymbols")       -> [{range, 16#1D400, 16#1D7FF}];
range("IsCJKUnifiedIdeographsExtensionB")        -> [{range, 16#20000, 16#2A6DF}];
range("IsCJKCompatibilityIdeographsSupplement")  -> [{range, 16#2F800, 16#2FA1F}];
range("IsTags")                                  -> [{value, 16#0}];

%%%
%% Start generated code
%%%
range("C") ->
    [{range, 0, 31}, {range, 127, 159}, {value, 173},
     {range, 1536, 1541}, {range, 1564, 1565}, {value, 1757},
     {value, 1807}, {value, 2274}, {range, 6158, 6159},
     {range, 8203, 8207}, {range, 8234, 8238},
     {range, 8288, 8303}, {range, 57344, 63743},
     {range, 65279, 65280}, {range, 65529, 65531},
     {value, 69821}, {range, 69837, 69839},
     {range, 113824, 118783}, {range, 119155, 119162},
     {range, 917505, 917759}, {range, 983040, 1114109}];
range("Cc") -> [{range, 0, 31}, {range, 127, 159}];
range("Cf") ->
    [{value, 173}, {range, 1536, 1541}, {range, 1564, 1565},
     {value, 1757}, {value, 1807}, {value, 2274},
     {range, 6158, 6159}, {range, 8203, 8207},
     {range, 8234, 8238}, {range, 8288, 8303},
     {range, 65279, 65280}, {range, 65529, 65531},
     {value, 69821}, {range, 69837, 69839},
     {range, 113824, 118783}, {range, 119155, 119162},
     {range, 917505, 917759}];
range("Cn") -> [{value, 0}];
range("Co") ->
    [{range, 57344, 63743}, {range, 983040, 1114109}];
range("Cs") -> [{value, 0}];
range("L") ->
    [{range, 65, 90}, {range, 97, 122}, {value, 170},
     {value, 181}, {value, 186}, {range, 192, 214},
     {range, 216, 246}, {range, 248, 705}, {range, 710, 721},
     {range, 736, 740}, {value, 748}, {value, 750},
     {range, 880, 884}, {range, 886, 893}, {range, 895, 899},
     {value, 902}, {range, 904, 1013}, {range, 1015, 1153},
     {range, 1162, 1369}, {range, 1376, 1416},
     {range, 1488, 1522}, {range, 1568, 1610},
     {range, 1646, 1647}, {range, 1649, 1747}, {value, 1749},
     {range, 1765, 1766}, {range, 1774, 1775},
     {range, 1786, 1788}, {value, 1791}, {value, 1808},
     {range, 1810, 1839}, {range, 1869, 1957},
     {range, 1969, 1983}, {range, 1994, 2026},
     {range, 2036, 2037}, {range, 2042, 2044},
     {range, 2048, 2069}, {value, 2074}, {value, 2084},
     {value, 2088}, {range, 2112, 2136}, {range, 2144, 2258},
     {range, 2308, 2361}, {value, 2365}, {value, 2384},
     {range, 2392, 2401}, {range, 2417, 2432},
     {range, 2437, 2491}, {value, 2493}, {range, 2510, 2518},
     {range, 2524, 2529}, {range, 2544, 2545}, {value, 2556},
     {range, 2565, 2619}, {range, 2649, 2661},
     {range, 2674, 2676}, {range, 2693, 2747}, {value, 2749},
     {range, 2768, 2785}, {value, 2809}, {range, 2821, 2875},
     {value, 2877}, {range, 2908, 2913}, {value, 2929},
     {range, 2947, 3005}, {range, 3024, 3030},
     {range, 3077, 3133}, {range, 3160, 3169}, {value, 3200},
     {range, 3205, 3259}, {value, 3261}, {range, 3294, 3297},
     {range, 3313, 3327}, {range, 3333, 3386}, {value, 3389},
     {value, 3406}, {range, 3412, 3414}, {range, 3423, 3425},
     {range, 3450, 3457}, {range, 3461, 3529},
     {range, 3585, 3632}, {range, 3634, 3635},
     {range, 3648, 3654}, {range, 3713, 3760},
     {range, 3762, 3763}, {range, 3773, 3783},
     {range, 3804, 3840}, {range, 3904, 3952},
     {range, 3976, 3980}, {range, 4096, 4138}, {value, 4159},
     {range, 4176, 4181}, {range, 4186, 4189}, {value, 4193},
     {range, 4197, 4198}, {range, 4206, 4208},
     {range, 4213, 4225}, {value, 4238}, {range, 4256, 4346},
     {range, 4348, 4956}, {range, 4992, 5007},
     {range, 5024, 5119}, {range, 5121, 5740},
     {range, 5743, 5759}, {range, 5761, 5786},
     {range, 5792, 5866}, {range, 5873, 5905},
     {range, 5920, 5937}, {range, 5952, 5969},
     {range, 5984, 6001}, {range, 6016, 6067}, {value, 6103},
     {value, 6108}, {range, 6176, 6276}, {range, 6279, 6312},
     {range, 6314, 6431}, {range, 6480, 6607},
     {range, 6656, 6678}, {range, 6688, 6740}, {value, 6823},
     {range, 6917, 6963}, {range, 6981, 6991},
     {range, 7043, 7072}, {range, 7086, 7087},
     {range, 7098, 7141}, {range, 7168, 7203},
     {range, 7245, 7247}, {range, 7258, 7293},
     {range, 7296, 7359}, {range, 7401, 7404},
     {range, 7406, 7409}, {range, 7413, 7414},
     {range, 7424, 7615}, {range, 7680, 8124}, {value, 8126},
     {range, 8130, 8140}, {range, 8144, 8156},
     {range, 8160, 8172}, {range, 8178, 8188},
     {range, 8305, 8307}, {value, 8319}, {range, 8336, 8351},
     {value, 8450}, {value, 8455}, {range, 8458, 8467},
     {value, 8469}, {range, 8473, 8477}, {value, 8484},
     {value, 8486}, {value, 8488}, {range, 8490, 8493},
     {range, 8495, 8505}, {range, 8508, 8511},
     {range, 8517, 8521}, {value, 8526}, {range, 8579, 8580},
     {range, 11264, 11492}, {range, 11499, 11502},
     {range, 11506, 11512}, {range, 11520, 11631},
     {range, 11648, 11743}, {value, 11823},
     {range, 12293, 12294}, {range, 12337, 12341},
     {range, 12347, 12348}, {range, 12353, 12440},
     {range, 12445, 12447}, {range, 12449, 12538},
     {range, 12540, 12687}, {range, 12704, 12735},
     {range, 12784, 12799}, {range, 13312, 19903},
     {range, 19968, 42127}, {range, 42192, 42237},
     {range, 42240, 42508}, {range, 42512, 42527},
     {range, 42538, 42606}, {range, 42623, 42653},
     {range, 42656, 42725}, {range, 42775, 42783},
     {range, 42786, 42888}, {range, 42891, 43009},
     {range, 43011, 43013}, {range, 43015, 43018},
     {range, 43020, 43042}, {range, 43072, 43123},
     {range, 43138, 43187}, {range, 43250, 43255},
     {value, 43259}, {range, 43261, 43262},
     {range, 43274, 43301}, {range, 43312, 43334},
     {range, 43360, 43391}, {range, 43396, 43442},
     {value, 43471}, {range, 43488, 43492},
     {range, 43494, 43503}, {range, 43514, 43560},
     {range, 43584, 43586}, {range, 43588, 43595},
     {range, 43616, 43638}, {value, 43642},
     {range, 43646, 43695}, {value, 43697},
     {range, 43701, 43702}, {range, 43705, 43709},
     {value, 43712}, {range, 43714, 43741},
     {range, 43744, 43754}, {range, 43762, 43764},
     {range, 43777, 43866}, {range, 43868, 44002},
     {range, 44032, 55291}, {range, 63744, 64285},
     {range, 64287, 64296}, {range, 64298, 64433},
     {range, 64467, 64829}, {range, 64848, 65019},
     {range, 65136, 65278}, {range, 65313, 65338},
     {range, 65345, 65370}, {range, 65382, 65503},
     {range, 65536, 65791}, {range, 66176, 66271},
     {range, 66304, 66335}, {range, 66349, 66368},
     {range, 66370, 66377}, {range, 66384, 66421},
     {range, 66432, 66462}, {range, 66464, 66511},
     {range, 66560, 66719}, {range, 66736, 66926},
     {range, 67072, 67670}, {range, 67680, 67702},
     {range, 67712, 67750}, {range, 67808, 67834},
     {range, 67840, 67861}, {range, 67872, 67902},
     {range, 67968, 68027}, {range, 68030, 68031},
     {value, 68096}, {range, 68112, 68151},
     {range, 68192, 68220}, {range, 68224, 68252},
     {range, 68288, 68295}, {range, 68297, 68324},
     {range, 68352, 68408}, {range, 68416, 68439},
     {range, 68448, 68471}, {range, 68480, 68504},
     {range, 68608, 68857}, {range, 68864, 68899},
     {range, 69376, 69404}, {range, 69415, 69445},
     {range, 69635, 69687}, {range, 69763, 69807},
     {range, 69840, 69871}, {range, 69891, 69926},
     {value, 69956}, {range, 69968, 70002},
     {range, 70006, 70015}, {range, 70019, 70066},
     {range, 70081, 70084}, {value, 70106}, {value, 70108},
     {range, 70144, 70187}, {range, 70272, 70312},
     {range, 70320, 70366}, {range, 70405, 70458},
     {value, 70461}, {range, 70480, 70486},
     {range, 70493, 70497}, {range, 70656, 70708},
     {range, 70727, 70730}, {range, 70784, 70831},
     {range, 70852, 70853}, {range, 70855, 70863},
     {range, 71040, 71086}, {range, 71128, 71131},
     {range, 71168, 71215}, {range, 71236, 71247},
     {range, 71296, 71338}, {range, 71424, 71452},
     {range, 71680, 71723}, {range, 71840, 71903},
     {range, 71935, 72192}, {range, 72203, 72242},
     {value, 72250}, {value, 72272}, {range, 72284, 72329},
     {value, 72349}, {range, 72384, 72750}, {value, 72768},
     {range, 72818, 72849}, {range, 72960, 73008},
     {value, 73030}, {range, 73056, 73097},
     {range, 73112, 73119}, {range, 73440, 73458},
     {range, 73728, 74751}, {range, 74880, 92767},
     {range, 92880, 92911}, {range, 92928, 92975},
     {range, 92992, 92995}, {range, 93027, 93823},
     {range, 93952, 94032}, {range, 94099, 113819},
     {range, 119808, 120512}, {range, 120514, 120538},
     {range, 120540, 120570}, {range, 120572, 120596},
     {range, 120598, 120628}, {range, 120630, 120654},
     {range, 120656, 120686}, {range, 120688, 120712},
     {range, 120714, 120744}, {range, 120746, 120770},
     {range, 120772, 120781}, {range, 124928, 125126},
     {range, 125184, 125251}, {range, 126464, 126703},
     {range, 131072, 917504}];
range("Ll") ->
    [{range, 97, 122}, {value, 181}, {range, 223, 246},
     {range, 248, 255}, {value, 257}, {value, 259},
     {value, 261}, {value, 263}, {value, 265}, {value, 267},
     {value, 269}, {value, 271}, {value, 273}, {value, 275},
     {value, 277}, {value, 279}, {value, 281}, {value, 283},
     {value, 285}, {value, 287}, {value, 289}, {value, 291},
     {value, 293}, {value, 295}, {value, 297}, {value, 299},
     {value, 301}, {value, 303}, {value, 305}, {value, 307},
     {value, 309}, {range, 311, 312}, {value, 314},
     {value, 316}, {value, 318}, {value, 320}, {value, 322},
     {value, 324}, {value, 326}, {range, 328, 329},
     {value, 331}, {value, 333}, {value, 335}, {value, 337},
     {value, 339}, {value, 341}, {value, 343}, {value, 345},
     {value, 347}, {value, 349}, {value, 351}, {value, 353},
     {value, 355}, {value, 357}, {value, 359}, {value, 361},
     {value, 363}, {value, 365}, {value, 367}, {value, 369},
     {value, 371}, {value, 373}, {value, 375}, {value, 378},
     {value, 380}, {range, 382, 384}, {value, 387},
     {value, 389}, {value, 392}, {range, 396, 397},
     {value, 402}, {value, 405}, {range, 409, 411},
     {value, 414}, {value, 417}, {value, 419}, {value, 421},
     {value, 424}, {range, 426, 427}, {value, 429},
     {value, 432}, {value, 436}, {value, 438},
     {range, 441, 442}, {range, 445, 447}, {value, 454},
     {value, 457}, {value, 460}, {value, 462}, {value, 464},
     {value, 466}, {value, 468}, {value, 470}, {value, 472},
     {value, 474}, {range, 476, 477}, {value, 479},
     {value, 481}, {value, 483}, {value, 485}, {value, 487},
     {value, 489}, {value, 491}, {value, 493},
     {range, 495, 496}, {value, 499}, {value, 501},
     {value, 505}, {value, 507}, {value, 509}, {value, 511},
     {value, 513}, {value, 515}, {value, 517}, {value, 519},
     {value, 521}, {value, 523}, {value, 525}, {value, 527},
     {value, 529}, {value, 531}, {value, 533}, {value, 535},
     {value, 537}, {value, 539}, {value, 541}, {value, 543},
     {value, 545}, {value, 547}, {value, 549}, {value, 551},
     {value, 553}, {value, 555}, {value, 557}, {value, 559},
     {value, 561}, {range, 563, 569}, {value, 572},
     {range, 575, 576}, {value, 578}, {value, 583},
     {value, 585}, {value, 587}, {value, 589},
     {range, 591, 659}, {range, 661, 687}, {value, 881},
     {value, 883}, {range, 887, 889}, {range, 891, 893},
     {value, 912}, {range, 940, 974}, {range, 976, 977},
     {range, 981, 983}, {value, 985}, {value, 987},
     {value, 989}, {value, 991}, {value, 993}, {value, 995},
     {value, 997}, {value, 999}, {value, 1001},
     {value, 1003}, {value, 1005}, {range, 1007, 1011},
     {value, 1013}, {value, 1016}, {range, 1019, 1020},
     {range, 1072, 1119}, {value, 1121}, {value, 1123},
     {value, 1125}, {value, 1127}, {value, 1129},
     {value, 1131}, {value, 1133}, {value, 1135},
     {value, 1137}, {value, 1139}, {value, 1141},
     {value, 1143}, {value, 1145}, {value, 1147},
     {value, 1149}, {value, 1151}, {value, 1153},
     {value, 1163}, {value, 1165}, {value, 1167},
     {value, 1169}, {value, 1171}, {value, 1173},
     {value, 1175}, {value, 1177}, {value, 1179},
     {value, 1181}, {value, 1183}, {value, 1185},
     {value, 1187}, {value, 1189}, {value, 1191},
     {value, 1193}, {value, 1195}, {value, 1197},
     {value, 1199}, {value, 1201}, {value, 1203},
     {value, 1205}, {value, 1207}, {value, 1209},
     {value, 1211}, {value, 1213}, {value, 1215},
     {value, 1218}, {value, 1220}, {value, 1222},
     {value, 1224}, {value, 1226}, {value, 1228},
     {range, 1230, 1231}, {value, 1233}, {value, 1235},
     {value, 1237}, {value, 1239}, {value, 1241},
     {value, 1243}, {value, 1245}, {value, 1247},
     {value, 1249}, {value, 1251}, {value, 1253},
     {value, 1255}, {value, 1257}, {value, 1259},
     {value, 1261}, {value, 1263}, {value, 1265},
     {value, 1267}, {value, 1269}, {value, 1271},
     {value, 1273}, {value, 1275}, {value, 1277},
     {value, 1279}, {value, 1281}, {value, 1283},
     {value, 1285}, {value, 1287}, {value, 1289},
     {value, 1291}, {value, 1293}, {value, 1295},
     {value, 1297}, {value, 1299}, {value, 1301},
     {value, 1303}, {value, 1305}, {value, 1307},
     {value, 1309}, {value, 1311}, {value, 1313},
     {value, 1315}, {value, 1317}, {value, 1319},
     {value, 1321}, {value, 1323}, {value, 1325},
     {range, 1327, 1328}, {range, 1376, 1416},
     {range, 4304, 4346}, {range, 4349, 4351},
     {range, 5112, 5119}, {range, 7296, 7311},
     {range, 7424, 7467}, {range, 7531, 7543},
     {range, 7545, 7578}, {value, 7681}, {value, 7683},
     {value, 7685}, {value, 7687}, {value, 7689},
     {value, 7691}, {value, 7693}, {value, 7695},
     {value, 7697}, {value, 7699}, {value, 7701},
     {value, 7703}, {value, 7705}, {value, 7707},
     {value, 7709}, {value, 7711}, {value, 7713},
     {value, 7715}, {value, 7717}, {value, 7719},
     {value, 7721}, {value, 7723}, {value, 7725},
     {value, 7727}, {value, 7729}, {value, 7731},
     {value, 7733}, {value, 7735}, {value, 7737},
     {value, 7739}, {value, 7741}, {value, 7743},
     {value, 7745}, {value, 7747}, {value, 7749},
     {value, 7751}, {value, 7753}, {value, 7755},
     {value, 7757}, {value, 7759}, {value, 7761},
     {value, 7763}, {value, 7765}, {value, 7767},
     {value, 7769}, {value, 7771}, {value, 7773},
     {value, 7775}, {value, 7777}, {value, 7779},
     {value, 7781}, {value, 7783}, {value, 7785},
     {value, 7787}, {value, 7789}, {value, 7791},
     {value, 7793}, {value, 7795}, {value, 7797},
     {value, 7799}, {value, 7801}, {value, 7803},
     {value, 7805}, {value, 7807}, {value, 7809},
     {value, 7811}, {value, 7813}, {value, 7815},
     {value, 7817}, {value, 7819}, {value, 7821},
     {value, 7823}, {value, 7825}, {value, 7827},
     {range, 7829, 7837}, {value, 7839}, {value, 7841},
     {value, 7843}, {value, 7845}, {value, 7847},
     {value, 7849}, {value, 7851}, {value, 7853},
     {value, 7855}, {value, 7857}, {value, 7859},
     {value, 7861}, {value, 7863}, {value, 7865},
     {value, 7867}, {value, 7869}, {value, 7871},
     {value, 7873}, {value, 7875}, {value, 7877},
     {value, 7879}, {value, 7881}, {value, 7883},
     {value, 7885}, {value, 7887}, {value, 7889},
     {value, 7891}, {value, 7893}, {value, 7895},
     {value, 7897}, {value, 7899}, {value, 7901},
     {value, 7903}, {value, 7905}, {value, 7907},
     {value, 7909}, {value, 7911}, {value, 7913},
     {value, 7915}, {value, 7917}, {value, 7919},
     {value, 7921}, {value, 7923}, {value, 7925},
     {value, 7927}, {value, 7929}, {value, 7931},
     {value, 7933}, {range, 7935, 7943}, {range, 7952, 7959},
     {range, 7968, 7975}, {range, 7984, 7991},
     {range, 8000, 8007}, {range, 8016, 8024},
     {range, 8032, 8039}, {range, 8048, 8071},
     {range, 8080, 8087}, {range, 8096, 8103},
     {range, 8112, 8119}, {value, 8126}, {range, 8130, 8135},
     {range, 8144, 8151}, {range, 8160, 8167},
     {range, 8178, 8183}, {value, 8458}, {range, 8462, 8463},
     {value, 8467}, {value, 8495}, {value, 8500},
     {value, 8505}, {range, 8508, 8509}, {range, 8518, 8521},
     {value, 8526}, {value, 8580}, {range, 11312, 11359},
     {value, 11361}, {range, 11365, 11366}, {value, 11368},
     {value, 11370}, {value, 11372}, {value, 11377},
     {range, 11379, 11380}, {range, 11382, 11387},
     {value, 11393}, {value, 11395}, {value, 11397},
     {value, 11399}, {value, 11401}, {value, 11403},
     {value, 11405}, {value, 11407}, {value, 11409},
     {value, 11411}, {value, 11413}, {value, 11415},
     {value, 11417}, {value, 11419}, {value, 11421},
     {value, 11423}, {value, 11425}, {value, 11427},
     {value, 11429}, {value, 11431}, {value, 11433},
     {value, 11435}, {value, 11437}, {value, 11439},
     {value, 11441}, {value, 11443}, {value, 11445},
     {value, 11447}, {value, 11449}, {value, 11451},
     {value, 11453}, {value, 11455}, {value, 11457},
     {value, 11459}, {value, 11461}, {value, 11463},
     {value, 11465}, {value, 11467}, {value, 11469},
     {value, 11471}, {value, 11473}, {value, 11475},
     {value, 11477}, {value, 11479}, {value, 11481},
     {value, 11483}, {value, 11485}, {value, 11487},
     {value, 11489}, {range, 11491, 11492}, {value, 11500},
     {value, 11502}, {range, 11507, 11512},
     {range, 11520, 11567}, {value, 42561}, {value, 42563},
     {value, 42565}, {value, 42567}, {value, 42569},
     {value, 42571}, {value, 42573}, {value, 42575},
     {value, 42577}, {value, 42579}, {value, 42581},
     {value, 42583}, {value, 42585}, {value, 42587},
     {value, 42589}, {value, 42591}, {value, 42593},
     {value, 42595}, {value, 42597}, {value, 42599},
     {value, 42601}, {value, 42603}, {value, 42605},
     {value, 42625}, {value, 42627}, {value, 42629},
     {value, 42631}, {value, 42633}, {value, 42635},
     {value, 42637}, {value, 42639}, {value, 42641},
     {value, 42643}, {value, 42645}, {value, 42647},
     {value, 42649}, {value, 42651}, {value, 42787},
     {value, 42789}, {value, 42791}, {value, 42793},
     {value, 42795}, {value, 42797}, {range, 42799, 42801},
     {value, 42803}, {value, 42805}, {value, 42807},
     {value, 42809}, {value, 42811}, {value, 42813},
     {value, 42815}, {value, 42817}, {value, 42819},
     {value, 42821}, {value, 42823}, {value, 42825},
     {value, 42827}, {value, 42829}, {value, 42831},
     {value, 42833}, {value, 42835}, {value, 42837},
     {value, 42839}, {value, 42841}, {value, 42843},
     {value, 42845}, {value, 42847}, {value, 42849},
     {value, 42851}, {value, 42853}, {value, 42855},
     {value, 42857}, {value, 42859}, {value, 42861},
     {value, 42863}, {range, 42865, 42872}, {value, 42874},
     {value, 42876}, {value, 42879}, {value, 42881},
     {value, 42883}, {value, 42885}, {value, 42887},
     {value, 42892}, {value, 42894}, {value, 42897},
     {range, 42899, 42901}, {value, 42903}, {value, 42905},
     {value, 42907}, {value, 42909}, {value, 42911},
     {value, 42913}, {value, 42915}, {value, 42917},
     {value, 42919}, {value, 42921}, {value, 42927},
     {value, 42933}, {value, 42935}, {range, 42937, 42998},
     {value, 43002}, {range, 43824, 43866},
     {range, 43872, 43967}, {range, 64256, 64284},
     {range, 65345, 65370}, {range, 66600, 66639},
     {range, 66776, 66815}, {range, 68800, 68857},
     {range, 71872, 71903}, {range, 93792, 93823},
     {range, 119834, 119859}, {range, 119886, 119911},
     {range, 119938, 119963}, {range, 119990, 120015},
     {range, 120042, 120067}, {range, 120094, 120119},
     {range, 120146, 120171}, {range, 120198, 120223},
     {range, 120250, 120275}, {range, 120302, 120327},
     {range, 120354, 120379}, {range, 120406, 120431},
     {range, 120458, 120487}, {range, 120514, 120538},
     {range, 120540, 120545}, {range, 120572, 120596},
     {range, 120598, 120603}, {range, 120630, 120654},
     {range, 120656, 120661}, {range, 120688, 120712},
     {range, 120714, 120719}, {range, 120746, 120770},
     {range, 120772, 120777}, {range, 120779, 120781},
     {range, 125218, 125251}];
range("Lm") ->
    [{range, 688, 705}, {range, 710, 721},
     {range, 736, 740}, {value, 748}, {value, 750},
     {value, 884}, {value, 890}, {value, 1369},
     {value, 1600}, {range, 1765, 1766}, {range, 2036, 2037},
     {range, 2042, 2044}, {value, 2074}, {value, 2084},
     {value, 2088}, {value, 2417}, {value, 3654},
     {range, 3782, 3783}, {value, 4348}, {value, 6103},
     {value, 6211}, {value, 6823}, {range, 7288, 7293},
     {range, 7468, 7530}, {value, 7544}, {range, 7579, 7615},
     {range, 8305, 8307}, {value, 8319}, {range, 8336, 8351},
     {range, 11388, 11389}, {value, 11631}, {value, 11823},
     {value, 12293}, {range, 12337, 12341}, {value, 12347},
     {range, 12445, 12446}, {range, 12540, 12542},
     {value, 40981}, {range, 42232, 42237}, {value, 42508},
     {value, 42623}, {range, 42652, 42653},
     {range, 42775, 42783}, {value, 42864}, {value, 42888},
     {range, 43000, 43001}, {value, 43471}, {value, 43494},
     {value, 43632}, {value, 43741}, {range, 43763, 43764},
     {range, 43868, 43871}, {value, 65392},
     {range, 65438, 65439}, {range, 92992, 92995},
     {range, 94099, 94207}];
range("Lo") ->
    [{value, 170}, {value, 186}, {value, 443},
     {range, 448, 451}, {value, 660}, {range, 1488, 1522},
     {range, 1568, 1599}, {range, 1601, 1610},
     {range, 1646, 1647}, {range, 1649, 1747}, {value, 1749},
     {range, 1774, 1775}, {range, 1786, 1788}, {value, 1791},
     {value, 1808}, {range, 1810, 1839}, {range, 1869, 1957},
     {range, 1969, 1983}, {range, 1994, 2026},
     {range, 2048, 2069}, {range, 2112, 2136},
     {range, 2144, 2258}, {range, 2308, 2361}, {value, 2365},
     {value, 2384}, {range, 2392, 2401}, {range, 2418, 2432},
     {range, 2437, 2491}, {value, 2493}, {range, 2510, 2518},
     {range, 2524, 2529}, {range, 2544, 2545}, {value, 2556},
     {range, 2565, 2619}, {range, 2649, 2661},
     {range, 2674, 2676}, {range, 2693, 2747}, {value, 2749},
     {range, 2768, 2785}, {value, 2809}, {range, 2821, 2875},
     {value, 2877}, {range, 2908, 2913}, {value, 2929},
     {range, 2947, 3005}, {range, 3024, 3030},
     {range, 3077, 3133}, {range, 3160, 3169}, {value, 3200},
     {range, 3205, 3259}, {value, 3261}, {range, 3294, 3297},
     {range, 3313, 3327}, {range, 3333, 3386}, {value, 3389},
     {value, 3406}, {range, 3412, 3414}, {range, 3423, 3425},
     {range, 3450, 3457}, {range, 3461, 3529},
     {range, 3585, 3632}, {range, 3634, 3635},
     {range, 3648, 3653}, {range, 3713, 3760},
     {range, 3762, 3763}, {range, 3773, 3781},
     {range, 3804, 3840}, {range, 3904, 3952},
     {range, 3976, 3980}, {range, 4096, 4138}, {value, 4159},
     {range, 4176, 4181}, {range, 4186, 4189}, {value, 4193},
     {range, 4197, 4198}, {range, 4206, 4208},
     {range, 4213, 4225}, {value, 4238}, {range, 4352, 4956},
     {range, 4992, 5007}, {range, 5121, 5740},
     {range, 5743, 5759}, {range, 5761, 5786},
     {range, 5792, 5866}, {range, 5873, 5905},
     {range, 5920, 5937}, {range, 5952, 5969},
     {range, 5984, 6001}, {range, 6016, 6067}, {value, 6108},
     {range, 6176, 6210}, {range, 6212, 6276},
     {range, 6279, 6312}, {range, 6314, 6431},
     {range, 6480, 6607}, {range, 6656, 6678},
     {range, 6688, 6740}, {range, 6917, 6963},
     {range, 6981, 6991}, {range, 7043, 7072},
     {range, 7086, 7087}, {range, 7098, 7141},
     {range, 7168, 7203}, {range, 7245, 7247},
     {range, 7258, 7287}, {range, 7401, 7404},
     {range, 7406, 7409}, {range, 7413, 7414},
     {range, 8501, 8504}, {range, 11568, 11630},
     {range, 11648, 11743}, {value, 12294}, {value, 12348},
     {range, 12353, 12440}, {value, 12447},
     {range, 12449, 12538}, {range, 12543, 12687},
     {range, 12704, 12735}, {range, 12784, 12799},
     {range, 13312, 19903}, {range, 19968, 40980},
     {range, 40982, 42127}, {range, 42192, 42231},
     {range, 42240, 42507}, {range, 42512, 42527},
     {range, 42538, 42559}, {value, 42606},
     {range, 42656, 42725}, {value, 42895}, {value, 42999},
     {range, 43003, 43009}, {range, 43011, 43013},
     {range, 43015, 43018}, {range, 43020, 43042},
     {range, 43072, 43123}, {range, 43138, 43187},
     {range, 43250, 43255}, {value, 43259},
     {range, 43261, 43262}, {range, 43274, 43301},
     {range, 43312, 43334}, {range, 43360, 43391},
     {range, 43396, 43442}, {range, 43488, 43492},
     {range, 43495, 43503}, {range, 43514, 43560},
     {range, 43584, 43586}, {range, 43588, 43595},
     {range, 43616, 43631}, {range, 43633, 43638},
     {value, 43642}, {range, 43646, 43695}, {value, 43697},
     {range, 43701, 43702}, {range, 43705, 43709},
     {value, 43712}, {range, 43714, 43740},
     {range, 43744, 43754}, {value, 43762},
     {range, 43777, 43823}, {range, 43968, 44002},
     {range, 44032, 55291}, {range, 63744, 64255},
     {value, 64285}, {range, 64287, 64296},
     {range, 64298, 64433}, {range, 64467, 64829},
     {range, 64848, 65019}, {range, 65136, 65278},
     {range, 65382, 65391}, {range, 65393, 65437},
     {range, 65440, 65503}, {range, 65536, 65791},
     {range, 66176, 66271}, {range, 66304, 66335},
     {range, 66349, 66368}, {range, 66370, 66377},
     {range, 66384, 66421}, {range, 66432, 66462},
     {range, 66464, 66511}, {range, 66640, 66719},
     {range, 66816, 66926}, {range, 67072, 67670},
     {range, 67680, 67702}, {range, 67712, 67750},
     {range, 67808, 67834}, {range, 67840, 67861},
     {range, 67872, 67902}, {range, 67968, 68027},
     {range, 68030, 68031}, {value, 68096},
     {range, 68112, 68151}, {range, 68192, 68220},
     {range, 68224, 68252}, {range, 68288, 68295},
     {range, 68297, 68324}, {range, 68352, 68408},
     {range, 68416, 68439}, {range, 68448, 68471},
     {range, 68480, 68504}, {range, 68608, 68735},
     {range, 68864, 68899}, {range, 69376, 69404},
     {range, 69415, 69445}, {range, 69635, 69687},
     {range, 69763, 69807}, {range, 69840, 69871},
     {range, 69891, 69926}, {value, 69956},
     {range, 69968, 70002}, {range, 70006, 70015},
     {range, 70019, 70066}, {range, 70081, 70084},
     {value, 70106}, {value, 70108}, {range, 70144, 70187},
     {range, 70272, 70312}, {range, 70320, 70366},
     {range, 70405, 70458}, {value, 70461},
     {range, 70480, 70486}, {range, 70493, 70497},
     {range, 70656, 70708}, {range, 70727, 70730},
     {range, 70784, 70831}, {range, 70852, 70853},
     {range, 70855, 70863}, {range, 71040, 71086},
     {range, 71128, 71131}, {range, 71168, 71215},
     {range, 71236, 71247}, {range, 71296, 71338},
     {range, 71424, 71452}, {range, 71680, 71723},
     {range, 71935, 72192}, {range, 72203, 72242},
     {value, 72250}, {value, 72272}, {range, 72284, 72329},
     {value, 72349}, {range, 72384, 72750}, {value, 72768},
     {range, 72818, 72849}, {range, 72960, 73008},
     {value, 73030}, {range, 73056, 73097},
     {range, 73112, 73119}, {range, 73440, 73458},
     {range, 73728, 74751}, {range, 74880, 92767},
     {range, 92880, 92911}, {range, 92928, 92975},
     {range, 93027, 93759}, {range, 93952, 94032},
     {range, 94208, 113819}, {range, 124928, 125126},
     {range, 126464, 126703}, {range, 131072, 917504}];
range("Lt") ->
    [{value, 453}, {value, 456}, {value, 459}, {value, 498},
     {range, 8072, 8079}, {range, 8088, 8095},
     {range, 8104, 8111}, {value, 8124}, {value, 8140},
     {value, 8188}];
range("Lu") ->
    [{range, 65, 90}, {range, 192, 214}, {range, 216, 222},
     {value, 256}, {value, 258}, {value, 260}, {value, 262},
     {value, 264}, {value, 266}, {value, 268}, {value, 270},
     {value, 272}, {value, 274}, {value, 276}, {value, 278},
     {value, 280}, {value, 282}, {value, 284}, {value, 286},
     {value, 288}, {value, 290}, {value, 292}, {value, 294},
     {value, 296}, {value, 298}, {value, 300}, {value, 302},
     {value, 304}, {value, 306}, {value, 308}, {value, 310},
     {value, 313}, {value, 315}, {value, 317}, {value, 319},
     {value, 321}, {value, 323}, {value, 325}, {value, 327},
     {value, 330}, {value, 332}, {value, 334}, {value, 336},
     {value, 338}, {value, 340}, {value, 342}, {value, 344},
     {value, 346}, {value, 348}, {value, 350}, {value, 352},
     {value, 354}, {value, 356}, {value, 358}, {value, 360},
     {value, 362}, {value, 364}, {value, 366}, {value, 368},
     {value, 370}, {value, 372}, {value, 374},
     {range, 376, 377}, {value, 379}, {value, 381},
     {range, 385, 386}, {value, 388}, {range, 390, 391},
     {range, 393, 395}, {range, 398, 401}, {range, 403, 404},
     {range, 406, 408}, {range, 412, 413}, {range, 415, 416},
     {value, 418}, {value, 420}, {range, 422, 423},
     {value, 425}, {value, 428}, {range, 430, 431},
     {range, 433, 435}, {value, 437}, {range, 439, 440},
     {value, 444}, {value, 452}, {value, 455}, {value, 458},
     {value, 461}, {value, 463}, {value, 465}, {value, 467},
     {value, 469}, {value, 471}, {value, 473}, {value, 475},
     {value, 478}, {value, 480}, {value, 482}, {value, 484},
     {value, 486}, {value, 488}, {value, 490}, {value, 492},
     {value, 494}, {value, 497}, {value, 500},
     {range, 502, 504}, {value, 506}, {value, 508},
     {value, 510}, {value, 512}, {value, 514}, {value, 516},
     {value, 518}, {value, 520}, {value, 522}, {value, 524},
     {value, 526}, {value, 528}, {value, 530}, {value, 532},
     {value, 534}, {value, 536}, {value, 538}, {value, 540},
     {value, 542}, {value, 544}, {value, 546}, {value, 548},
     {value, 550}, {value, 552}, {value, 554}, {value, 556},
     {value, 558}, {value, 560}, {value, 562},
     {range, 570, 571}, {range, 573, 574}, {value, 577},
     {range, 579, 582}, {value, 584}, {value, 586},
     {value, 588}, {value, 590}, {value, 880}, {value, 882},
     {value, 886}, {range, 895, 899}, {value, 902},
     {range, 904, 911}, {range, 913, 939}, {value, 975},
     {range, 978, 980}, {value, 984}, {value, 986},
     {value, 988}, {value, 990}, {value, 992}, {value, 994},
     {value, 996}, {value, 998}, {value, 1000},
     {value, 1002}, {value, 1004}, {value, 1006},
     {value, 1012}, {value, 1015}, {range, 1017, 1018},
     {range, 1021, 1071}, {value, 1120}, {value, 1122},
     {value, 1124}, {value, 1126}, {value, 1128},
     {value, 1130}, {value, 1132}, {value, 1134},
     {value, 1136}, {value, 1138}, {value, 1140},
     {value, 1142}, {value, 1144}, {value, 1146},
     {value, 1148}, {value, 1150}, {value, 1152},
     {value, 1162}, {value, 1164}, {value, 1166},
     {value, 1168}, {value, 1170}, {value, 1172},
     {value, 1174}, {value, 1176}, {value, 1178},
     {value, 1180}, {value, 1182}, {value, 1184},
     {value, 1186}, {value, 1188}, {value, 1190},
     {value, 1192}, {value, 1194}, {value, 1196},
     {value, 1198}, {value, 1200}, {value, 1202},
     {value, 1204}, {value, 1206}, {value, 1208},
     {value, 1210}, {value, 1212}, {value, 1214},
     {range, 1216, 1217}, {value, 1219}, {value, 1221},
     {value, 1223}, {value, 1225}, {value, 1227},
     {value, 1229}, {value, 1232}, {value, 1234},
     {value, 1236}, {value, 1238}, {value, 1240},
     {value, 1242}, {value, 1244}, {value, 1246},
     {value, 1248}, {value, 1250}, {value, 1252},
     {value, 1254}, {value, 1256}, {value, 1258},
     {value, 1260}, {value, 1262}, {value, 1264},
     {value, 1266}, {value, 1268}, {value, 1270},
     {value, 1272}, {value, 1274}, {value, 1276},
     {value, 1278}, {value, 1280}, {value, 1282},
     {value, 1284}, {value, 1286}, {value, 1288},
     {value, 1290}, {value, 1292}, {value, 1294},
     {value, 1296}, {value, 1298}, {value, 1300},
     {value, 1302}, {value, 1304}, {value, 1306},
     {value, 1308}, {value, 1310}, {value, 1312},
     {value, 1314}, {value, 1316}, {value, 1318},
     {value, 1320}, {value, 1322}, {value, 1324},
     {value, 1326}, {range, 1329, 1368}, {range, 4256, 4303},
     {range, 5024, 5111}, {range, 7312, 7359}, {value, 7680},
     {value, 7682}, {value, 7684}, {value, 7686},
     {value, 7688}, {value, 7690}, {value, 7692},
     {value, 7694}, {value, 7696}, {value, 7698},
     {value, 7700}, {value, 7702}, {value, 7704},
     {value, 7706}, {value, 7708}, {value, 7710},
     {value, 7712}, {value, 7714}, {value, 7716},
     {value, 7718}, {value, 7720}, {value, 7722},
     {value, 7724}, {value, 7726}, {value, 7728},
     {value, 7730}, {value, 7732}, {value, 7734},
     {value, 7736}, {value, 7738}, {value, 7740},
     {value, 7742}, {value, 7744}, {value, 7746},
     {value, 7748}, {value, 7750}, {value, 7752},
     {value, 7754}, {value, 7756}, {value, 7758},
     {value, 7760}, {value, 7762}, {value, 7764},
     {value, 7766}, {value, 7768}, {value, 7770},
     {value, 7772}, {value, 7774}, {value, 7776},
     {value, 7778}, {value, 7780}, {value, 7782},
     {value, 7784}, {value, 7786}, {value, 7788},
     {value, 7790}, {value, 7792}, {value, 7794},
     {value, 7796}, {value, 7798}, {value, 7800},
     {value, 7802}, {value, 7804}, {value, 7806},
     {value, 7808}, {value, 7810}, {value, 7812},
     {value, 7814}, {value, 7816}, {value, 7818},
     {value, 7820}, {value, 7822}, {value, 7824},
     {value, 7826}, {value, 7828}, {value, 7838},
     {value, 7840}, {value, 7842}, {value, 7844},
     {value, 7846}, {value, 7848}, {value, 7850},
     {value, 7852}, {value, 7854}, {value, 7856},
     {value, 7858}, {value, 7860}, {value, 7862},
     {value, 7864}, {value, 7866}, {value, 7868},
     {value, 7870}, {value, 7872}, {value, 7874},
     {value, 7876}, {value, 7878}, {value, 7880},
     {value, 7882}, {value, 7884}, {value, 7886},
     {value, 7888}, {value, 7890}, {value, 7892},
     {value, 7894}, {value, 7896}, {value, 7898},
     {value, 7900}, {value, 7902}, {value, 7904},
     {value, 7906}, {value, 7908}, {value, 7910},
     {value, 7912}, {value, 7914}, {value, 7916},
     {value, 7918}, {value, 7920}, {value, 7922},
     {value, 7924}, {value, 7926}, {value, 7928},
     {value, 7930}, {value, 7932}, {value, 7934},
     {range, 7944, 7951}, {range, 7960, 7967},
     {range, 7976, 7983}, {range, 7992, 7999},
     {range, 8008, 8015}, {range, 8025, 8031},
     {range, 8040, 8047}, {range, 8120, 8123},
     {range, 8136, 8139}, {range, 8152, 8156},
     {range, 8168, 8172}, {range, 8184, 8187}, {value, 8450},
     {value, 8455}, {range, 8459, 8461}, {range, 8464, 8466},
     {value, 8469}, {range, 8473, 8477}, {value, 8484},
     {value, 8486}, {value, 8488}, {range, 8490, 8493},
     {range, 8496, 8499}, {range, 8510, 8511}, {value, 8517},
     {value, 8579}, {range, 11264, 11311}, {value, 11360},
     {range, 11362, 11364}, {value, 11367}, {value, 11369},
     {value, 11371}, {range, 11373, 11376}, {value, 11378},
     {value, 11381}, {range, 11390, 11392}, {value, 11394},
     {value, 11396}, {value, 11398}, {value, 11400},
     {value, 11402}, {value, 11404}, {value, 11406},
     {value, 11408}, {value, 11410}, {value, 11412},
     {value, 11414}, {value, 11416}, {value, 11418},
     {value, 11420}, {value, 11422}, {value, 11424},
     {value, 11426}, {value, 11428}, {value, 11430},
     {value, 11432}, {value, 11434}, {value, 11436},
     {value, 11438}, {value, 11440}, {value, 11442},
     {value, 11444}, {value, 11446}, {value, 11448},
     {value, 11450}, {value, 11452}, {value, 11454},
     {value, 11456}, {value, 11458}, {value, 11460},
     {value, 11462}, {value, 11464}, {value, 11466},
     {value, 11468}, {value, 11470}, {value, 11472},
     {value, 11474}, {value, 11476}, {value, 11478},
     {value, 11480}, {value, 11482}, {value, 11484},
     {value, 11486}, {value, 11488}, {value, 11490},
     {value, 11499}, {value, 11501}, {value, 11506},
     {value, 42560}, {value, 42562}, {value, 42564},
     {value, 42566}, {value, 42568}, {value, 42570},
     {value, 42572}, {value, 42574}, {value, 42576},
     {value, 42578}, {value, 42580}, {value, 42582},
     {value, 42584}, {value, 42586}, {value, 42588},
     {value, 42590}, {value, 42592}, {value, 42594},
     {value, 42596}, {value, 42598}, {value, 42600},
     {value, 42602}, {value, 42604}, {value, 42624},
     {value, 42626}, {value, 42628}, {value, 42630},
     {value, 42632}, {value, 42634}, {value, 42636},
     {value, 42638}, {value, 42640}, {value, 42642},
     {value, 42644}, {value, 42646}, {value, 42648},
     {value, 42650}, {value, 42786}, {value, 42788},
     {value, 42790}, {value, 42792}, {value, 42794},
     {value, 42796}, {value, 42798}, {value, 42802},
     {value, 42804}, {value, 42806}, {value, 42808},
     {value, 42810}, {value, 42812}, {value, 42814},
     {value, 42816}, {value, 42818}, {value, 42820},
     {value, 42822}, {value, 42824}, {value, 42826},
     {value, 42828}, {value, 42830}, {value, 42832},
     {value, 42834}, {value, 42836}, {value, 42838},
     {value, 42840}, {value, 42842}, {value, 42844},
     {value, 42846}, {value, 42848}, {value, 42850},
     {value, 42852}, {value, 42854}, {value, 42856},
     {value, 42858}, {value, 42860}, {value, 42862},
     {value, 42873}, {value, 42875}, {range, 42877, 42878},
     {value, 42880}, {value, 42882}, {value, 42884},
     {value, 42886}, {value, 42891}, {value, 42893},
     {value, 42896}, {value, 42898}, {value, 42902},
     {value, 42904}, {value, 42906}, {value, 42908},
     {value, 42910}, {value, 42912}, {value, 42914},
     {value, 42916}, {value, 42918}, {value, 42920},
     {range, 42922, 42926}, {range, 42928, 42932},
     {value, 42934}, {value, 42936}, {range, 65313, 65338},
     {range, 66560, 66599}, {range, 66736, 66775},
     {range, 68736, 68799}, {range, 71840, 71871},
     {range, 93760, 93791}, {range, 119808, 119833},
     {range, 119860, 119885}, {range, 119912, 119937},
     {range, 119964, 119989}, {range, 120016, 120041},
     {range, 120068, 120093}, {range, 120120, 120145},
     {range, 120172, 120197}, {range, 120224, 120249},
     {range, 120276, 120301}, {range, 120328, 120353},
     {range, 120380, 120405}, {range, 120432, 120457},
     {range, 120488, 120512}, {range, 120546, 120570},
     {range, 120604, 120628}, {range, 120662, 120686},
     {range, 120720, 120744}, {value, 120778},
     {range, 125184, 125217}];
range("M") ->
    [{range, 768, 879}, {range, 1155, 1161},
     {range, 1425, 1469}, {value, 1471}, {range, 1473, 1474},
     {range, 1476, 1477}, {range, 1479, 1487},
     {range, 1552, 1562}, {range, 1611, 1631}, {value, 1648},
     {range, 1750, 1756}, {range, 1759, 1764},
     {range, 1767, 1768}, {range, 1770, 1773}, {value, 1809},
     {range, 1840, 1868}, {range, 1958, 1968},
     {range, 2027, 2035}, {value, 2045}, {range, 2070, 2073},
     {range, 2075, 2083}, {range, 2085, 2087},
     {range, 2089, 2095}, {range, 2137, 2141},
     {range, 2259, 2273}, {range, 2275, 2307},
     {range, 2362, 2364}, {range, 2366, 2383},
     {range, 2385, 2391}, {range, 2402, 2403},
     {range, 2433, 2436}, {value, 2492}, {range, 2494, 2509},
     {range, 2519, 2523}, {range, 2530, 2533},
     {range, 2558, 2564}, {range, 2620, 2648},
     {range, 2672, 2673}, {value, 2677}, {range, 2689, 2692},
     {value, 2748}, {range, 2750, 2767}, {range, 2786, 2789},
     {range, 2810, 2820}, {value, 2876}, {range, 2878, 2907},
     {range, 2914, 2917}, {value, 2946}, {range, 3006, 3023},
     {range, 3031, 3045}, {range, 3072, 3076},
     {range, 3134, 3159}, {range, 3170, 3173},
     {range, 3201, 3203}, {value, 3260}, {range, 3262, 3293},
     {range, 3298, 3301}, {range, 3328, 3332},
     {range, 3387, 3388}, {range, 3390, 3405}, {value, 3415},
     {range, 3426, 3429}, {range, 3458, 3460},
     {range, 3530, 3557}, {range, 3570, 3571}, {value, 3633},
     {range, 3636, 3646}, {range, 3655, 3662}, {value, 3761},
     {range, 3764, 3772}, {range, 3784, 3791},
     {range, 3864, 3865}, {value, 3893}, {value, 3895},
     {value, 3897}, {range, 3902, 3903}, {range, 3953, 3972},
     {range, 3974, 3975}, {range, 3981, 4029}, {value, 4038},
     {range, 4139, 4158}, {range, 4182, 4185},
     {range, 4190, 4192}, {range, 4194, 4196},
     {range, 4199, 4205}, {range, 4209, 4212},
     {range, 4226, 4237}, {value, 4239}, {range, 4250, 4253},
     {range, 4957, 4959}, {range, 5906, 5919},
     {range, 5938, 5940}, {range, 5970, 5983},
     {range, 6002, 6015}, {range, 6068, 6099},
     {range, 6109, 6111}, {range, 6155, 6157},
     {range, 6277, 6278}, {value, 6313}, {range, 6432, 6463},
     {range, 6679, 6685}, {range, 6741, 6783},
     {range, 6832, 6916}, {range, 6964, 6980},
     {range, 7019, 7027}, {range, 7040, 7042},
     {range, 7073, 7085}, {range, 7142, 7163},
     {range, 7204, 7226}, {range, 7376, 7378},
     {range, 7380, 7400}, {value, 7405}, {range, 7410, 7412},
     {range, 7415, 7423}, {range, 7616, 7679},
     {range, 8400, 8447}, {range, 11503, 11505},
     {value, 11647}, {range, 11744, 11775},
     {range, 12330, 12335}, {range, 12441, 12442},
     {range, 42607, 42610}, {range, 42612, 42621},
     {range, 42654, 42655}, {range, 42736, 42737},
     {value, 43010}, {value, 43014}, {value, 43019},
     {range, 43043, 43047}, {range, 43136, 43137},
     {range, 43188, 43213}, {range, 43232, 43249},
     {value, 43263}, {range, 43302, 43309},
     {range, 43335, 43358}, {range, 43392, 43395},
     {range, 43443, 43456}, {value, 43493},
     {range, 43561, 43583}, {value, 43587},
     {range, 43596, 43599}, {range, 43643, 43645},
     {value, 43696}, {range, 43698, 43700},
     {range, 43703, 43704}, {range, 43710, 43711},
     {value, 43713}, {range, 43755, 43759},
     {range, 43765, 43776}, {range, 44003, 44010},
     {range, 44012, 44015}, {value, 64286},
     {range, 65024, 65039}, {range, 65056, 65071},
     {range, 66045, 66175}, {value, 66272},
     {range, 66422, 66431}, {range, 68097, 68111},
     {range, 68152, 68159}, {range, 68325, 68330},
     {range, 68900, 68911}, {range, 69446, 69456},
     {range, 69632, 69634}, {range, 69688, 69702},
     {range, 69759, 69762}, {range, 69808, 69818},
     {range, 69888, 69890}, {range, 69927, 69941},
     {range, 69957, 69967}, {value, 70003},
     {range, 70016, 70018}, {range, 70067, 70080},
     {range, 70089, 70092}, {range, 70188, 70199},
     {range, 70206, 70271}, {range, 70367, 70383},
     {range, 70400, 70404}, {range, 70459, 70460},
     {range, 70462, 70479}, {range, 70487, 70492},
     {range, 70498, 70655}, {range, 70709, 70726},
     {range, 70750, 70783}, {range, 70832, 70851},
     {range, 71087, 71104}, {range, 71132, 71167},
     {range, 71216, 71232}, {range, 71339, 71359},
     {range, 71453, 71471}, {range, 71724, 71738},
     {range, 72193, 72202}, {range, 72243, 72249},
     {range, 72251, 72254}, {range, 72263, 72271},
     {range, 72273, 72283}, {range, 72330, 72345},
     {range, 72751, 72767}, {range, 72850, 72959},
     {range, 73009, 73029}, {range, 73031, 73039},
     {range, 73098, 73111}, {range, 73459, 73462},
     {range, 92912, 92916}, {range, 92976, 92982},
     {range, 94033, 94098}, {range, 113821, 113822},
     {range, 119141, 119145}, {range, 119149, 119154},
     {range, 119163, 119170}, {range, 119173, 119179},
     {range, 119210, 119213}, {range, 119362, 119364},
     {range, 121344, 121398}, {range, 121403, 121452},
     {value, 121461}, {value, 121476},
     {range, 121499, 124927}, {range, 125136, 125183},
     {range, 125252, 125263}, {range, 917760, 983039}];
range("Mc") ->
    [{value, 2307}, {value, 2363}, {range, 2366, 2368},
     {range, 2377, 2380}, {range, 2382, 2383},
     {range, 2434, 2436}, {range, 2494, 2496},
     {range, 2503, 2508}, {range, 2519, 2523},
     {range, 2563, 2564}, {range, 2622, 2624},
     {range, 2691, 2692}, {range, 2750, 2752},
     {range, 2761, 2764}, {range, 2818, 2820}, {value, 2878},
     {value, 2880}, {range, 2887, 2892}, {range, 2903, 2907},
     {range, 3006, 3007}, {range, 3009, 3020},
     {range, 3031, 3045}, {range, 3073, 3075},
     {range, 3137, 3141}, {range, 3202, 3203}, {value, 3262},
     {range, 3264, 3269}, {range, 3271, 3275},
     {range, 3285, 3293}, {range, 3330, 3332},
     {range, 3390, 3392}, {range, 3398, 3404}, {value, 3415},
     {range, 3458, 3460}, {range, 3535, 3537},
     {range, 3544, 3557}, {range, 3570, 3571},
     {range, 3902, 3903}, {value, 3967}, {range, 4139, 4140},
     {value, 4145}, {value, 4152}, {range, 4155, 4156},
     {range, 4182, 4183}, {range, 4194, 4196},
     {range, 4199, 4205}, {range, 4227, 4228},
     {range, 4231, 4236}, {value, 4239}, {range, 4250, 4252},
     {value, 6070}, {range, 6078, 6085}, {range, 6087, 6088},
     {range, 6435, 6438}, {range, 6441, 6449},
     {range, 6451, 6456}, {range, 6681, 6682}, {value, 6741},
     {value, 6743}, {value, 6753}, {range, 6755, 6756},
     {range, 6765, 6770}, {value, 6916}, {value, 6965},
     {value, 6971}, {range, 6973, 6977}, {range, 6979, 6980},
     {value, 7042}, {value, 7073}, {range, 7078, 7079},
     {value, 7082}, {value, 7143}, {range, 7146, 7148},
     {value, 7150}, {range, 7154, 7163}, {range, 7204, 7211},
     {range, 7220, 7221}, {value, 7393}, {range, 7410, 7411},
     {value, 7415}, {range, 12334, 12335},
     {range, 43043, 43044}, {value, 43047},
     {range, 43136, 43137}, {range, 43188, 43203},
     {range, 43346, 43358}, {value, 43395},
     {range, 43444, 43445}, {range, 43450, 43451},
     {range, 43453, 43456}, {range, 43567, 43568},
     {range, 43571, 43572}, {range, 43597, 43599},
     {value, 43643}, {value, 43645}, {value, 43755},
     {range, 43758, 43759}, {value, 43765},
     {range, 44003, 44004}, {range, 44006, 44007},
     {range, 44009, 44010}, {value, 44012}, {value, 69632},
     {value, 69634}, {value, 69762}, {range, 69808, 69810},
     {range, 69815, 69816}, {value, 69932},
     {range, 69957, 69967}, {value, 70018},
     {range, 70067, 70069}, {range, 70079, 70080},
     {range, 70188, 70190}, {range, 70194, 70195},
     {value, 70197}, {range, 70368, 70370},
     {range, 70402, 70404}, {range, 70462, 70463},
     {range, 70465, 70479}, {range, 70487, 70492},
     {range, 70498, 70501}, {range, 70709, 70711},
     {range, 70720, 70721}, {value, 70725},
     {range, 70832, 70834}, {value, 70841},
     {range, 70843, 70846}, {value, 70849},
     {range, 71087, 71089}, {range, 71096, 71099},
     {value, 71102}, {range, 71216, 71218},
     {range, 71227, 71228}, {value, 71230}, {value, 71340},
     {range, 71342, 71343}, {value, 71350},
     {range, 71456, 71457}, {value, 71462},
     {range, 71724, 71726}, {value, 71736}, {value, 72249},
     {range, 72279, 72280}, {value, 72343}, {value, 72751},
     {value, 72766}, {value, 72873}, {value, 72881},
     {value, 72884}, {range, 73098, 73103},
     {range, 73107, 73108}, {value, 73110},
     {range, 73461, 73462}, {range, 94033, 94094},
     {range, 119141, 119142}, {range, 119149, 119154}];
range("Me") ->
    [{range, 1160, 1161}, {range, 6846, 6911},
     {range, 8413, 8416}, {range, 8418, 8420},
     {range, 42608, 42610}];
range("Mn") ->
    [{range, 768, 879}, {range, 1155, 1159},
     {range, 1425, 1469}, {value, 1471}, {range, 1473, 1474},
     {range, 1476, 1477}, {range, 1479, 1487},
     {range, 1552, 1562}, {range, 1611, 1631}, {value, 1648},
     {range, 1750, 1756}, {range, 1759, 1764},
     {range, 1767, 1768}, {range, 1770, 1773}, {value, 1809},
     {range, 1840, 1868}, {range, 1958, 1968},
     {range, 2027, 2035}, {value, 2045}, {range, 2070, 2073},
     {range, 2075, 2083}, {range, 2085, 2087},
     {range, 2089, 2095}, {range, 2137, 2141},
     {range, 2259, 2273}, {range, 2275, 2306}, {value, 2362},
     {value, 2364}, {range, 2369, 2376}, {value, 2381},
     {range, 2385, 2391}, {range, 2402, 2403}, {value, 2433},
     {value, 2492}, {range, 2497, 2502}, {value, 2509},
     {range, 2530, 2533}, {range, 2558, 2562},
     {range, 2620, 2621}, {range, 2625, 2648},
     {range, 2672, 2673}, {value, 2677}, {range, 2689, 2690},
     {value, 2748}, {range, 2753, 2760}, {range, 2765, 2767},
     {range, 2786, 2789}, {range, 2810, 2817}, {value, 2876},
     {value, 2879}, {range, 2881, 2886}, {range, 2893, 2902},
     {range, 2914, 2917}, {value, 2946}, {value, 3008},
     {range, 3021, 3023}, {value, 3072}, {value, 3076},
     {range, 3134, 3136}, {range, 3142, 3159},
     {range, 3170, 3173}, {value, 3201}, {value, 3260},
     {value, 3263}, {value, 3270}, {range, 3276, 3284},
     {range, 3298, 3301}, {range, 3328, 3329},
     {range, 3387, 3388}, {range, 3393, 3397}, {value, 3405},
     {range, 3426, 3429}, {range, 3530, 3534},
     {range, 3538, 3543}, {value, 3633}, {range, 3636, 3646},
     {range, 3655, 3662}, {value, 3761}, {range, 3764, 3772},
     {range, 3784, 3791}, {range, 3864, 3865}, {value, 3893},
     {value, 3895}, {value, 3897}, {range, 3953, 3966},
     {range, 3968, 3972}, {range, 3974, 3975},
     {range, 3981, 4029}, {value, 4038}, {range, 4141, 4144},
     {range, 4146, 4151}, {range, 4153, 4154},
     {range, 4157, 4158}, {range, 4184, 4185},
     {range, 4190, 4192}, {range, 4209, 4212}, {value, 4226},
     {range, 4229, 4230}, {value, 4237}, {value, 4253},
     {range, 4957, 4959}, {range, 5906, 5919},
     {range, 5938, 5940}, {range, 5970, 5983},
     {range, 6002, 6015}, {range, 6068, 6069},
     {range, 6071, 6077}, {value, 6086}, {range, 6089, 6099},
     {range, 6109, 6111}, {range, 6155, 6157},
     {range, 6277, 6278}, {value, 6313}, {range, 6432, 6434},
     {range, 6439, 6440}, {value, 6450}, {range, 6457, 6463},
     {range, 6679, 6680}, {range, 6683, 6685}, {value, 6742},
     {range, 6744, 6752}, {value, 6754}, {range, 6757, 6764},
     {range, 6771, 6783}, {range, 6832, 6845},
     {range, 6912, 6915}, {value, 6964}, {range, 6966, 6970},
     {value, 6972}, {value, 6978}, {range, 7019, 7027},
     {range, 7040, 7041}, {range, 7074, 7077},
     {range, 7080, 7081}, {range, 7083, 7085}, {value, 7142},
     {range, 7144, 7145}, {value, 7149}, {range, 7151, 7153},
     {range, 7212, 7219}, {range, 7222, 7226},
     {range, 7376, 7378}, {range, 7380, 7392},
     {range, 7394, 7400}, {value, 7405}, {value, 7412},
     {range, 7416, 7423}, {range, 7616, 7679},
     {range, 8400, 8412}, {value, 8417}, {range, 8421, 8447},
     {range, 11503, 11505}, {value, 11647},
     {range, 11744, 11775}, {range, 12330, 12333},
     {range, 12441, 12442}, {value, 42607},
     {range, 42612, 42621}, {range, 42654, 42655},
     {range, 42736, 42737}, {value, 43010}, {value, 43014},
     {value, 43019}, {range, 43045, 43046},
     {range, 43204, 43213}, {range, 43232, 43249},
     {value, 43263}, {range, 43302, 43309},
     {range, 43335, 43345}, {range, 43392, 43394},
     {value, 43443}, {range, 43446, 43449}, {value, 43452},
     {value, 43493}, {range, 43561, 43566},
     {range, 43569, 43570}, {range, 43573, 43583},
     {value, 43587}, {value, 43596}, {value, 43644},
     {value, 43696}, {range, 43698, 43700},
     {range, 43703, 43704}, {range, 43710, 43711},
     {value, 43713}, {range, 43756, 43757},
     {range, 43766, 43776}, {value, 44005}, {value, 44008},
     {range, 44013, 44015}, {value, 64286},
     {range, 65024, 65039}, {range, 65056, 65071},
     {range, 66045, 66175}, {value, 66272},
     {range, 66422, 66431}, {range, 68097, 68111},
     {range, 68152, 68159}, {range, 68325, 68330},
     {range, 68900, 68911}, {range, 69446, 69456},
     {value, 69633}, {range, 69688, 69702},
     {range, 69759, 69761}, {range, 69811, 69814},
     {range, 69817, 69818}, {range, 69888, 69890},
     {range, 69927, 69931}, {range, 69933, 69941},
     {value, 70003}, {range, 70016, 70017},
     {range, 70070, 70078}, {range, 70089, 70092},
     {range, 70191, 70193}, {value, 70196},
     {range, 70198, 70199}, {range, 70206, 70271},
     {value, 70367}, {range, 70371, 70383},
     {range, 70400, 70401}, {range, 70459, 70460},
     {value, 70464}, {range, 70502, 70655},
     {range, 70712, 70719}, {range, 70722, 70724},
     {value, 70726}, {range, 70750, 70783},
     {range, 70835, 70840}, {value, 70842},
     {range, 70847, 70848}, {range, 70850, 70851},
     {range, 71090, 71095}, {range, 71100, 71101},
     {range, 71103, 71104}, {range, 71132, 71167},
     {range, 71219, 71226}, {value, 71229},
     {range, 71231, 71232}, {value, 71339}, {value, 71341},
     {range, 71344, 71349}, {range, 71351, 71359},
     {range, 71453, 71455}, {range, 71458, 71461},
     {range, 71463, 71471}, {range, 71727, 71735},
     {range, 71737, 71738}, {range, 72193, 72202},
     {range, 72243, 72248}, {range, 72251, 72254},
     {range, 72263, 72271}, {range, 72273, 72278},
     {range, 72281, 72283}, {range, 72330, 72342},
     {range, 72344, 72345}, {range, 72752, 72765},
     {value, 72767}, {range, 72850, 72872},
     {range, 72874, 72880}, {range, 72882, 72883},
     {range, 72885, 72959}, {range, 73009, 73029},
     {range, 73031, 73039}, {range, 73104, 73106},
     {value, 73109}, {value, 73111}, {range, 73459, 73460},
     {range, 92912, 92916}, {range, 92976, 92982},
     {range, 94095, 94098}, {range, 113821, 113822},
     {range, 119143, 119145}, {range, 119163, 119170},
     {range, 119173, 119179}, {range, 119210, 119213},
     {range, 119362, 119364}, {range, 121344, 121398},
     {range, 121403, 121452}, {value, 121461},
     {value, 121476}, {range, 121499, 124927},
     {range, 125136, 125183}, {range, 125252, 125263},
     {range, 917760, 983039}];
range("N") ->
    [{range, 48, 57}, {range, 178, 179}, {value, 185},
     {range, 188, 190}, {range, 1632, 1641},
     {range, 1776, 1785}, {range, 1984, 1993},
     {range, 2406, 2415}, {range, 2534, 2543},
     {range, 2548, 2553}, {range, 2662, 2671},
     {range, 2790, 2799}, {range, 2918, 2927},
     {range, 2930, 2945}, {range, 3046, 3058},
     {range, 3174, 3183}, {range, 3192, 3198},
     {range, 3302, 3311}, {range, 3416, 3422},
     {range, 3430, 3448}, {range, 3558, 3567},
     {range, 3664, 3673}, {range, 3792, 3801},
     {range, 3872, 3891}, {range, 4160, 4169},
     {range, 4240, 4249}, {range, 4969, 4991},
     {range, 5870, 5872}, {range, 6112, 6121},
     {range, 6128, 6143}, {range, 6160, 6169},
     {range, 6470, 6479}, {range, 6608, 6621},
     {range, 6784, 6793}, {range, 6800, 6809},
     {range, 6992, 7001}, {range, 7088, 7097},
     {range, 7232, 7241}, {range, 7248, 7257}, {value, 8304},
     {range, 8308, 8313}, {range, 8320, 8329},
     {range, 8528, 8578}, {range, 8581, 8585},
     {range, 9312, 9371}, {range, 9450, 9471},
     {range, 10102, 10131}, {value, 11517}, {value, 12295},
     {range, 12321, 12329}, {range, 12344, 12346},
     {range, 12690, 12693}, {range, 12832, 12841},
     {range, 12872, 12879}, {range, 12881, 12895},
     {range, 12928, 12937}, {range, 12977, 12991},
     {range, 42528, 42537}, {range, 42726, 42735},
     {range, 43056, 43061}, {range, 43216, 43225},
     {range, 43264, 43273}, {range, 43472, 43481},
     {range, 43504, 43513}, {range, 43600, 43609},
     {range, 44016, 44025}, {range, 65296, 65305},
     {range, 65799, 65846}, {range, 65856, 65912},
     {range, 65930, 65931}, {range, 66273, 66303},
     {range, 66336, 66348}, {value, 66369},
     {range, 66378, 66383}, {range, 66513, 66559},
     {range, 66720, 66729}, {range, 67672, 67679},
     {range, 67705, 67711}, {range, 67751, 67807},
     {range, 67835, 67839}, {range, 67862, 67870},
     {range, 68028, 68029}, {range, 68032, 68095},
     {range, 68160, 68175}, {range, 68221, 68222},
     {range, 68253, 68287}, {range, 68331, 68335},
     {range, 68440, 68447}, {range, 68472, 68479},
     {range, 68521, 68607}, {range, 68858, 68863},
     {range, 68912, 68921}, {range, 69216, 69375},
     {range, 69405, 69414}, {range, 69457, 69460},
     {range, 69714, 69743}, {range, 69872, 69881},
     {range, 69942, 69951}, {range, 70096, 70105},
     {range, 70113, 70143}, {range, 70384, 70393},
     {range, 70736, 70745}, {range, 70864, 70873},
     {range, 71248, 71257}, {range, 71360, 71369},
     {range, 71472, 71483}, {range, 71904, 71934},
     {range, 72784, 72815}, {range, 73040, 73049},
     {range, 73120, 73129}, {range, 74752, 74863},
     {range, 92768, 92777}, {range, 93008, 93017},
     {range, 93019, 93026}, {range, 93824, 93846},
     {range, 119520, 119551}, {range, 119648, 119807},
     {range, 120782, 120831}, {range, 125127, 125135},
     {range, 125264, 125273}, {range, 126065, 126123},
     {range, 126125, 126127}, {range, 126129, 126463},
     {range, 127232, 127247}];
range("Nd") ->
    [{range, 48, 57}, {range, 1632, 1641},
     {range, 1776, 1785}, {range, 1984, 1993},
     {range, 2406, 2415}, {range, 2534, 2543},
     {range, 2662, 2671}, {range, 2790, 2799},
     {range, 2918, 2927}, {range, 3046, 3055},
     {range, 3174, 3183}, {range, 3302, 3311},
     {range, 3430, 3439}, {range, 3558, 3567},
     {range, 3664, 3673}, {range, 3792, 3801},
     {range, 3872, 3881}, {range, 4160, 4169},
     {range, 4240, 4249}, {range, 6112, 6121},
     {range, 6160, 6169}, {range, 6470, 6479},
     {range, 6608, 6617}, {range, 6784, 6793},
     {range, 6800, 6809}, {range, 6992, 7001},
     {range, 7088, 7097}, {range, 7232, 7241},
     {range, 7248, 7257}, {range, 42528, 42537},
     {range, 43216, 43225}, {range, 43264, 43273},
     {range, 43472, 43481}, {range, 43504, 43513},
     {range, 43600, 43609}, {range, 44016, 44025},
     {range, 65296, 65305}, {range, 66720, 66729},
     {range, 68912, 68921}, {range, 69734, 69743},
     {range, 69872, 69881}, {range, 69942, 69951},
     {range, 70096, 70105}, {range, 70384, 70393},
     {range, 70736, 70745}, {range, 70864, 70873},
     {range, 71248, 71257}, {range, 71360, 71369},
     {range, 71472, 71481}, {range, 71904, 71913},
     {range, 72784, 72793}, {range, 73040, 73049},
     {range, 73120, 73129}, {range, 92768, 92777},
     {range, 93008, 93017}, {range, 120782, 120831},
     {range, 125264, 125273}];
range("Nl") ->
    [{range, 5870, 5872}, {range, 8544, 8578},
     {range, 8581, 8584}, {value, 12295},
     {range, 12321, 12329}, {range, 12344, 12346},
     {range, 42726, 42735}, {range, 65856, 65908},
     {value, 66369}, {range, 66378, 66383},
     {range, 66513, 66559}, {range, 74752, 74863}];
range("No") ->
    [{range, 178, 179}, {value, 185}, {range, 188, 190},
     {range, 2548, 2553}, {range, 2930, 2945},
     {range, 3056, 3058}, {range, 3192, 3198},
     {range, 3416, 3422}, {range, 3440, 3448},
     {range, 3882, 3891}, {range, 4969, 4991},
     {range, 6128, 6143}, {range, 6618, 6621}, {value, 8304},
     {range, 8308, 8313}, {range, 8320, 8329},
     {range, 8528, 8543}, {value, 8585}, {range, 9312, 9371},
     {range, 9450, 9471}, {range, 10102, 10131},
     {value, 11517}, {range, 12690, 12693},
     {range, 12832, 12841}, {range, 12872, 12879},
     {range, 12881, 12895}, {range, 12928, 12937},
     {range, 12977, 12991}, {range, 43056, 43061},
     {range, 65799, 65846}, {range, 65909, 65912},
     {range, 65930, 65931}, {range, 66273, 66303},
     {range, 66336, 66348}, {range, 67672, 67679},
     {range, 67705, 67711}, {range, 67751, 67807},
     {range, 67835, 67839}, {range, 67862, 67870},
     {range, 68028, 68029}, {range, 68032, 68095},
     {range, 68160, 68175}, {range, 68221, 68222},
     {range, 68253, 68287}, {range, 68331, 68335},
     {range, 68440, 68447}, {range, 68472, 68479},
     {range, 68521, 68607}, {range, 68858, 68863},
     {range, 69216, 69375}, {range, 69405, 69414},
     {range, 69457, 69460}, {range, 69714, 69733},
     {range, 70113, 70143}, {range, 71482, 71483},
     {range, 71914, 71934}, {range, 72794, 72815},
     {range, 93019, 93026}, {range, 93824, 93846},
     {range, 119520, 119551}, {range, 119648, 119807},
     {range, 125127, 125135}, {range, 126065, 126123},
     {range, 126125, 126127}, {range, 126129, 126463},
     {range, 127232, 127247}];
range("P") ->
    [{range, 33, 35}, {range, 37, 42}, {range, 44, 47},
     {range, 58, 59}, {range, 63, 64}, {range, 91, 93},
     {value, 95}, {value, 123}, {value, 125}, {value, 161},
     {value, 167}, {value, 171}, {range, 182, 183},
     {value, 187}, {value, 191}, {value, 894}, {value, 903},
     {range, 1370, 1375}, {range, 1417, 1420}, {value, 1470},
     {value, 1472}, {value, 1475}, {value, 1478},
     {range, 1523, 1535}, {range, 1545, 1546},
     {range, 1548, 1549}, {value, 1563}, {range, 1566, 1567},
     {range, 1642, 1645}, {value, 1748}, {range, 1792, 1806},
     {range, 2039, 2041}, {range, 2096, 2111},
     {range, 2142, 2143}, {range, 2404, 2405}, {value, 2416},
     {value, 2557}, {range, 2678, 2688}, {value, 2800},
     {value, 3204}, {range, 3572, 3584}, {value, 3663},
     {range, 3674, 3712}, {range, 3844, 3858}, {value, 3860},
     {range, 3898, 3901}, {value, 3973}, {range, 4048, 4052},
     {range, 4057, 4095}, {range, 4170, 4175}, {value, 4347},
     {range, 4960, 4968}, {value, 5120}, {range, 5741, 5742},
     {range, 5787, 5791}, {range, 5867, 5869},
     {range, 5941, 5951}, {range, 6100, 6102},
     {range, 6104, 6106}, {range, 6144, 6154},
     {range, 6468, 6469}, {range, 6686, 6687},
     {range, 6816, 6822}, {range, 6824, 6831},
     {range, 7002, 7008}, {range, 7164, 7167},
     {range, 7227, 7231}, {range, 7294, 7295},
     {range, 7360, 7375}, {value, 7379}, {range, 8208, 8231},
     {range, 8240, 8259}, {range, 8261, 8273},
     {range, 8275, 8286}, {range, 8317, 8318},
     {range, 8333, 8335}, {range, 8968, 8971},
     {range, 9001, 9002}, {range, 10088, 10101},
     {range, 10181, 10182}, {range, 10214, 10223},
     {range, 10627, 10648}, {range, 10712, 10715},
     {range, 10748, 10749}, {range, 11513, 11516},
     {range, 11518, 11519}, {range, 11632, 11646},
     {range, 11776, 11822}, {range, 11824, 11903},
     {range, 12289, 12291}, {range, 12296, 12305},
     {range, 12308, 12319}, {value, 12336}, {value, 12349},
     {value, 12448}, {value, 12539}, {range, 42238, 42239},
     {range, 42509, 42511}, {value, 42611}, {value, 42622},
     {range, 42738, 42751}, {range, 43124, 43135},
     {range, 43214, 43215}, {range, 43256, 43258},
     {value, 43260}, {range, 43310, 43311}, {value, 43359},
     {range, 43457, 43470}, {range, 43486, 43487},
     {range, 43612, 43615}, {range, 43742, 43743},
     {range, 43760, 43761}, {value, 44011},
     {range, 64830, 64847}, {range, 65040, 65055},
     {range, 65072, 65121}, {value, 65123}, {value, 65128},
     {range, 65130, 65135}, {range, 65281, 65283},
     {range, 65285, 65290}, {range, 65292, 65295},
     {range, 65306, 65307}, {range, 65311, 65312},
     {range, 65339, 65341}, {value, 65343}, {value, 65371},
     {value, 65373}, {range, 65375, 65381},
     {range, 65792, 65798}, {value, 66463}, {value, 66512},
     {range, 66927, 67071}, {value, 67671}, {value, 67871},
     {range, 67903, 67967}, {range, 68176, 68191},
     {value, 68223}, {range, 68336, 68351},
     {range, 68409, 68415}, {range, 68505, 68520},
     {range, 69461, 69631}, {range, 69703, 69713},
     {range, 69819, 69820}, {range, 69822, 69836},
     {range, 69952, 69955}, {range, 70004, 70005},
     {range, 70085, 70088}, {range, 70093, 70095},
     {value, 70107}, {range, 70109, 70112},
     {range, 70200, 70205}, {range, 70313, 70319},
     {range, 70731, 70735}, {range, 70747, 70749},
     {value, 70854}, {range, 71105, 71127},
     {range, 71233, 71235}, {range, 71264, 71295},
     {range, 71484, 71486}, {range, 71739, 71839},
     {range, 72255, 72262}, {range, 72346, 72348},
     {range, 72350, 72383}, {range, 72769, 72783},
     {range, 72816, 72817}, {range, 73463, 73727},
     {range, 74864, 74879}, {range, 92782, 92879},
     {range, 92917, 92927}, {range, 92983, 92987},
     {value, 92996}, {range, 93847, 93951}, {value, 113823},
     {range, 121479, 121498}, {range, 125278, 126064}];
range("Pc") ->
    [{value, 95}, {range, 8255, 8256}, {value, 8276},
     {range, 65075, 65076}, {range, 65101, 65103},
     {value, 65343}];
range("Pd") ->
    [{value, 45}, {range, 1418, 1420}, {value, 1470},
     {value, 5120}, {value, 6150}, {range, 8208, 8213},
     {value, 11799}, {value, 11802}, {range, 11834, 11835},
     {value, 11840}, {value, 12316}, {value, 12336},
     {value, 12448}, {range, 65073, 65074}, {value, 65112},
     {value, 65123}, {value, 65293}];
range("Pe") ->
    [{value, 41}, {value, 93}, {value, 125}, {value, 3899},
     {value, 3901}, {range, 5788, 5791}, {value, 8262},
     {value, 8318}, {range, 8334, 8335}, {value, 8969},
     {value, 8971}, {value, 9002}, {value, 10089},
     {value, 10091}, {value, 10093}, {value, 10095},
     {value, 10097}, {value, 10099}, {value, 10101},
     {value, 10182}, {value, 10215}, {value, 10217},
     {value, 10219}, {value, 10221}, {value, 10223},
     {value, 10628}, {value, 10630}, {value, 10632},
     {value, 10634}, {value, 10636}, {value, 10638},
     {value, 10640}, {value, 10642}, {value, 10644},
     {value, 10646}, {value, 10648}, {value, 10713},
     {value, 10715}, {value, 10749}, {value, 11811},
     {value, 11813}, {value, 11815}, {value, 11817},
     {value, 12297}, {value, 12299}, {value, 12301},
     {value, 12303}, {value, 12305}, {value, 12309},
     {value, 12311}, {value, 12313}, {value, 12315},
     {range, 12318, 12319}, {value, 64830}, {value, 65048},
     {value, 65078}, {value, 65080}, {value, 65082},
     {value, 65084}, {value, 65086}, {value, 65088},
     {value, 65090}, {value, 65092}, {value, 65096},
     {value, 65114}, {value, 65116}, {value, 65118},
     {value, 65289}, {value, 65341}, {value, 65373},
     {value, 65376}, {value, 65379}];
range("Pf") ->
    [{value, 187}, {value, 8217}, {value, 8221},
     {value, 8250}, {value, 11779}, {value, 11781},
     {value, 11786}, {value, 11789}, {value, 11805},
     {value, 11809}];
range("Pi") ->
    [{value, 171}, {value, 8216}, {range, 8219, 8220},
     {value, 8223}, {value, 8249}, {value, 11778},
     {value, 11780}, {value, 11785}, {value, 11788},
     {value, 11804}, {value, 11808}];
range("Po") ->
    [{range, 33, 35}, {range, 37, 39}, {value, 42},
     {value, 44}, {range, 46, 47}, {range, 58, 59},
     {range, 63, 64}, {value, 92}, {value, 161},
     {value, 167}, {range, 182, 183}, {value, 191},
     {value, 894}, {value, 903}, {range, 1370, 1375},
     {value, 1417}, {value, 1472}, {value, 1475},
     {value, 1478}, {range, 1523, 1535}, {range, 1545, 1546},
     {range, 1548, 1549}, {value, 1563}, {range, 1566, 1567},
     {range, 1642, 1645}, {value, 1748}, {range, 1792, 1806},
     {range, 2039, 2041}, {range, 2096, 2111},
     {range, 2142, 2143}, {range, 2404, 2405}, {value, 2416},
     {value, 2557}, {range, 2678, 2688}, {value, 2800},
     {value, 3204}, {range, 3572, 3584}, {value, 3663},
     {range, 3674, 3712}, {range, 3844, 3858}, {value, 3860},
     {value, 3973}, {range, 4048, 4052}, {range, 4057, 4095},
     {range, 4170, 4175}, {value, 4347}, {range, 4960, 4968},
     {range, 5741, 5742}, {range, 5867, 5869},
     {range, 5941, 5951}, {range, 6100, 6102},
     {range, 6104, 6106}, {range, 6144, 6149},
     {range, 6151, 6154}, {range, 6468, 6469},
     {range, 6686, 6687}, {range, 6816, 6822},
     {range, 6824, 6831}, {range, 7002, 7008},
     {range, 7164, 7167}, {range, 7227, 7231},
     {range, 7294, 7295}, {range, 7360, 7375}, {value, 7379},
     {range, 8214, 8215}, {range, 8224, 8231},
     {range, 8240, 8248}, {range, 8251, 8254},
     {range, 8257, 8259}, {range, 8263, 8273}, {value, 8275},
     {range, 8277, 8286}, {range, 11513, 11516},
     {range, 11518, 11519}, {range, 11632, 11646},
     {range, 11776, 11777}, {range, 11782, 11784},
     {value, 11787}, {range, 11790, 11798},
     {range, 11800, 11801}, {value, 11803},
     {range, 11806, 11807}, {range, 11818, 11822},
     {range, 11824, 11833}, {range, 11836, 11839},
     {value, 11841}, {range, 11843, 11903},
     {range, 12289, 12291}, {value, 12349}, {value, 12539},
     {range, 42238, 42239}, {range, 42509, 42511},
     {value, 42611}, {value, 42622}, {range, 42738, 42751},
     {range, 43124, 43135}, {range, 43214, 43215},
     {range, 43256, 43258}, {value, 43260},
     {range, 43310, 43311}, {value, 43359},
     {range, 43457, 43470}, {range, 43486, 43487},
     {range, 43612, 43615}, {range, 43742, 43743},
     {range, 43760, 43761}, {value, 44011},
     {range, 65040, 65046}, {range, 65049, 65055},
     {value, 65072}, {range, 65093, 65094},
     {range, 65097, 65100}, {range, 65104, 65111},
     {range, 65119, 65121}, {value, 65128},
     {range, 65130, 65135}, {range, 65281, 65283},
     {range, 65285, 65287}, {value, 65290}, {value, 65292},
     {range, 65294, 65295}, {range, 65306, 65307},
     {range, 65311, 65312}, {value, 65340}, {value, 65377},
     {range, 65380, 65381}, {range, 65792, 65798},
     {value, 66463}, {value, 66512}, {range, 66927, 67071},
     {value, 67671}, {value, 67871}, {range, 67903, 67967},
     {range, 68176, 68191}, {value, 68223},
     {range, 68336, 68351}, {range, 68409, 68415},
     {range, 68505, 68520}, {range, 69461, 69631},
     {range, 69703, 69713}, {range, 69819, 69820},
     {range, 69822, 69836}, {range, 69952, 69955},
     {range, 70004, 70005}, {range, 70085, 70088},
     {range, 70093, 70095}, {value, 70107},
     {range, 70109, 70112}, {range, 70200, 70205},
     {range, 70313, 70319}, {range, 70731, 70735},
     {range, 70747, 70749}, {value, 70854},
     {range, 71105, 71127}, {range, 71233, 71235},
     {range, 71264, 71295}, {range, 71484, 71486},
     {range, 71739, 71839}, {range, 72255, 72262},
     {range, 72346, 72348}, {range, 72350, 72383},
     {range, 72769, 72783}, {range, 72816, 72817},
     {range, 73463, 73727}, {range, 74864, 74879},
     {range, 92782, 92879}, {range, 92917, 92927},
     {range, 92983, 92987}, {value, 92996},
     {range, 93847, 93951}, {value, 113823},
     {range, 121479, 121498}, {range, 125278, 126064}];
range("Ps") ->
    [{value, 40}, {value, 91}, {value, 123}, {value, 3898},
     {value, 3900}, {value, 5787}, {value, 8218},
     {value, 8222}, {value, 8261}, {value, 8317},
     {value, 8333}, {value, 8968}, {value, 8970},
     {value, 9001}, {value, 10088}, {value, 10090},
     {value, 10092}, {value, 10094}, {value, 10096},
     {value, 10098}, {value, 10100}, {value, 10181},
     {value, 10214}, {value, 10216}, {value, 10218},
     {value, 10220}, {value, 10222}, {value, 10627},
     {value, 10629}, {value, 10631}, {value, 10633},
     {value, 10635}, {value, 10637}, {value, 10639},
     {value, 10641}, {value, 10643}, {value, 10645},
     {value, 10647}, {value, 10712}, {value, 10714},
     {value, 10748}, {value, 11810}, {value, 11812},
     {value, 11814}, {value, 11816}, {value, 11842},
     {value, 12296}, {value, 12298}, {value, 12300},
     {value, 12302}, {value, 12304}, {value, 12308},
     {value, 12310}, {value, 12312}, {value, 12314},
     {value, 12317}, {range, 64831, 64847}, {value, 65047},
     {value, 65077}, {value, 65079}, {value, 65081},
     {value, 65083}, {value, 65085}, {value, 65087},
     {value, 65089}, {value, 65091}, {value, 65095},
     {value, 65113}, {value, 65115}, {value, 65117},
     {value, 65288}, {value, 65339}, {value, 65371},
     {value, 65375}, {value, 65378}];
range("S") ->
    [{value, 36}, {value, 43}, {range, 60, 62}, {value, 94},
     {value, 96}, {value, 124}, {value, 126},
     {range, 162, 166}, {range, 168, 169}, {value, 172},
     {range, 174, 177}, {value, 180}, {value, 184},
     {value, 215}, {value, 247}, {range, 706, 709},
     {range, 722, 735}, {range, 741, 747}, {value, 749},
     {range, 751, 767}, {value, 885}, {range, 900, 901},
     {value, 1014}, {value, 1154}, {range, 1421, 1424},
     {range, 1542, 1544}, {value, 1547}, {range, 1550, 1551},
     {value, 1758}, {value, 1769}, {range, 1789, 1790},
     {value, 2038}, {range, 2046, 2047}, {range, 2546, 2547},
     {range, 2554, 2555}, {range, 2801, 2808}, {value, 2928},
     {range, 3059, 3071}, {value, 3199}, {range, 3407, 3411},
     {value, 3449}, {value, 3647}, {range, 3841, 3843},
     {value, 3859}, {range, 3861, 3863}, {range, 3866, 3871},
     {value, 3892}, {value, 3894}, {value, 3896},
     {range, 4030, 4037}, {range, 4039, 4047},
     {range, 4053, 4056}, {range, 4254, 4255},
     {range, 5008, 5023}, {value, 6107}, {range, 6464, 6467},
     {range, 6622, 6655}, {range, 7009, 7018},
     {range, 7028, 7039}, {value, 8125}, {range, 8127, 8129},
     {range, 8141, 8143}, {range, 8157, 8159},
     {range, 8173, 8177}, {range, 8189, 8191}, {value, 8260},
     {value, 8274}, {range, 8314, 8316}, {range, 8330, 8332},
     {range, 8352, 8399}, {range, 8448, 8449},
     {range, 8451, 8454}, {range, 8456, 8457}, {value, 8468},
     {range, 8470, 8472}, {range, 8478, 8483}, {value, 8485},
     {value, 8487}, {value, 8489}, {value, 8494},
     {range, 8506, 8507}, {range, 8512, 8516},
     {range, 8522, 8525}, {value, 8527}, {range, 8586, 8967},
     {range, 8972, 9000}, {range, 9003, 9311},
     {range, 9372, 9449}, {range, 9472, 10087},
     {range, 10132, 10180}, {range, 10183, 10213},
     {range, 10224, 10626}, {range, 10649, 10711},
     {range, 10716, 10747}, {range, 10750, 11263},
     {range, 11493, 11498}, {range, 11904, 12287},
     {value, 12292}, {range, 12306, 12307}, {value, 12320},
     {range, 12342, 12343}, {range, 12350, 12352},
     {range, 12443, 12444}, {range, 12688, 12689},
     {range, 12694, 12703}, {range, 12736, 12783},
     {range, 12800, 12831}, {range, 12842, 12871},
     {value, 12880}, {range, 12896, 12927},
     {range, 12938, 12976}, {range, 12992, 13311},
     {range, 19904, 19967}, {range, 42128, 42191},
     {range, 42752, 42774}, {range, 42784, 42785},
     {range, 42889, 42890}, {range, 43048, 43055},
     {range, 43062, 43071}, {range, 43639, 43641},
     {value, 43867}, {value, 64297}, {range, 64434, 64466},
     {range, 65020, 65023}, {value, 65122},
     {range, 65124, 65127}, {value, 65129}, {value, 65284},
     {value, 65291}, {range, 65308, 65310}, {value, 65342},
     {value, 65344}, {value, 65372}, {value, 65374},
     {range, 65504, 65528}, {range, 65532, 65535},
     {range, 65847, 65855}, {range, 65913, 65929},
     {range, 65932, 66044}, {range, 67703, 67704},
     {value, 68296}, {range, 71487, 71679},
     {range, 92988, 92991}, {range, 92997, 93007},
     {value, 113820}, {range, 118784, 119140},
     {range, 119146, 119148}, {range, 119171, 119172},
     {range, 119180, 119209}, {range, 119214, 119361},
     {range, 119365, 119519}, {range, 119552, 119647},
     {value, 120513}, {value, 120539}, {value, 120571},
     {value, 120597}, {value, 120629}, {value, 120655},
     {value, 120687}, {value, 120713}, {value, 120745},
     {value, 120771}, {range, 120832, 121343},
     {range, 121399, 121402}, {range, 121453, 121460},
     {range, 121462, 121475}, {range, 121477, 121478},
     {value, 126124}, {value, 126128},
     {range, 126704, 127231}, {range, 127248, 131071}];
range("Sc") ->
    [{value, 36}, {range, 162, 165}, {range, 1423, 1424},
     {value, 1547}, {range, 2046, 2047}, {range, 2546, 2547},
     {value, 2555}, {range, 2801, 2808}, {value, 3065},
     {value, 3647}, {value, 6107}, {range, 8352, 8399},
     {value, 43064}, {value, 65020}, {value, 65129},
     {value, 65284}, {range, 65504, 65505},
     {range, 65509, 65511}, {value, 126128}];
range("Sk") ->
    [{value, 94}, {value, 96}, {value, 168}, {value, 175},
     {value, 180}, {value, 184}, {range, 706, 709},
     {range, 722, 735}, {range, 741, 747}, {value, 749},
     {range, 751, 767}, {value, 885}, {range, 900, 901},
     {value, 8125}, {range, 8127, 8129}, {range, 8141, 8143},
     {range, 8157, 8159}, {range, 8173, 8177},
     {range, 8189, 8191}, {range, 12443, 12444},
     {range, 42752, 42774}, {range, 42784, 42785},
     {range, 42889, 42890}, {value, 43867},
     {range, 64434, 64466}, {value, 65342}, {value, 65344},
     {value, 65507}, {range, 127995, 127999}];
range("Sm") ->
    [{value, 43}, {range, 60, 62}, {value, 124},
     {value, 126}, {value, 172}, {value, 177}, {value, 215},
     {value, 247}, {value, 1014}, {range, 1542, 1544},
     {value, 8260}, {value, 8274}, {range, 8314, 8316},
     {range, 8330, 8332}, {value, 8472}, {range, 8512, 8516},
     {value, 8523}, {range, 8592, 8596}, {range, 8602, 8603},
     {value, 8608}, {value, 8611}, {value, 8614},
     {value, 8622}, {range, 8654, 8655}, {value, 8658},
     {value, 8660}, {range, 8692, 8959}, {range, 8992, 8993},
     {value, 9084}, {range, 9115, 9139}, {range, 9180, 9185},
     {value, 9655}, {value, 9665}, {range, 9720, 9727},
     {value, 9839}, {range, 10176, 10180},
     {range, 10183, 10213}, {range, 10224, 10239},
     {range, 10496, 10626}, {range, 10649, 10711},
     {range, 10716, 10747}, {range, 10750, 11007},
     {range, 11056, 11076}, {range, 11079, 11084},
     {value, 64297}, {value, 65122}, {range, 65124, 65127},
     {value, 65291}, {range, 65308, 65310}, {value, 65372},
     {value, 65374}, {value, 65506}, {range, 65513, 65516},
     {value, 120513}, {value, 120539}, {value, 120571},
     {value, 120597}, {value, 120629}, {value, 120655},
     {value, 120687}, {value, 120713}, {value, 120745},
     {value, 120771}, {range, 126704, 126975}];
range("So") ->
    [{value, 166}, {value, 169}, {value, 174}, {value, 176},
     {value, 1154}, {range, 1421, 1422}, {range, 1550, 1551},
     {value, 1758}, {value, 1769}, {range, 1789, 1790},
     {value, 2038}, {value, 2554}, {value, 2928},
     {range, 3059, 3064}, {range, 3066, 3071}, {value, 3199},
     {range, 3407, 3411}, {value, 3449}, {range, 3841, 3843},
     {value, 3859}, {range, 3861, 3863}, {range, 3866, 3871},
     {value, 3892}, {value, 3894}, {value, 3896},
     {range, 4030, 4037}, {range, 4039, 4047},
     {range, 4053, 4056}, {range, 4254, 4255},
     {range, 5008, 5023}, {range, 6464, 6467},
     {range, 6622, 6655}, {range, 7009, 7018},
     {range, 7028, 7039}, {range, 8448, 8449},
     {range, 8451, 8454}, {range, 8456, 8457}, {value, 8468},
     {range, 8470, 8471}, {range, 8478, 8483}, {value, 8485},
     {value, 8487}, {value, 8489}, {value, 8494},
     {range, 8506, 8507}, {value, 8522}, {range, 8524, 8525},
     {value, 8527}, {range, 8586, 8591}, {range, 8597, 8601},
     {range, 8604, 8607}, {range, 8609, 8610},
     {range, 8612, 8613}, {range, 8615, 8621},
     {range, 8623, 8653}, {range, 8656, 8657}, {value, 8659},
     {range, 8661, 8691}, {range, 8960, 8967},
     {range, 8972, 8991}, {range, 8994, 9000},
     {range, 9003, 9083}, {range, 9085, 9114},
     {range, 9140, 9179}, {range, 9186, 9311},
     {range, 9372, 9449}, {range, 9472, 9654},
     {range, 9656, 9664}, {range, 9666, 9719},
     {range, 9728, 9838}, {range, 9840, 10087},
     {range, 10132, 10175}, {range, 10240, 10495},
     {range, 11008, 11055}, {range, 11077, 11078},
     {range, 11085, 11263}, {range, 11493, 11498},
     {range, 11904, 12287}, {value, 12292},
     {range, 12306, 12307}, {value, 12320},
     {range, 12342, 12343}, {range, 12350, 12352},
     {range, 12688, 12689}, {range, 12694, 12703},
     {range, 12736, 12783}, {range, 12800, 12831},
     {range, 12842, 12871}, {value, 12880},
     {range, 12896, 12927}, {range, 12938, 12976},
     {range, 12992, 13311}, {range, 19904, 19967},
     {range, 42128, 42191}, {range, 43048, 43055},
     {range, 43062, 43063}, {range, 43065, 43071},
     {range, 43639, 43641}, {range, 65021, 65023},
     {value, 65508}, {value, 65512}, {range, 65517, 65528},
     {range, 65532, 65535}, {range, 65847, 65855},
     {range, 65913, 65929}, {range, 65932, 66044},
     {range, 67703, 67704}, {value, 68296},
     {range, 71487, 71679}, {range, 92988, 92991},
     {range, 92997, 93007}, {value, 113820},
     {range, 118784, 119140}, {range, 119146, 119148},
     {range, 119171, 119172}, {range, 119180, 119209},
     {range, 119214, 119361}, {range, 119365, 119519},
     {range, 119552, 119647}, {range, 120832, 121343},
     {range, 121399, 121402}, {range, 121453, 121460},
     {range, 121462, 121475}, {range, 121477, 121478},
     {value, 126124}, {range, 126976, 127231},
     {range, 127248, 127994}, {range, 128000, 131071}];
range("Z") ->
    [{value, 32}, {value, 160}, {value, 5760},
     {range, 8192, 8202}, {range, 8232, 8233}, {value, 8239},
     {value, 8287}, {value, 12288}];
range("Zl") -> [{value, 8232}];
range("Zp") -> [{value, 8233}];
range("Zs") ->
    [{value, 32}, {value, 160}, {value, 5760},
     {range, 8192, 8202}, {value, 8239}, {value, 8287},
     {value, 12288}];

%%%
%% End generated code
%%%

% [#x20\t\n\r]
range(R) when R == "\\s";
              R == "\\S" ->
   [{value, 16#09},
    {value, 16#0A},
    {value, 16#0D},
    {value, 16#20}];

% the set of initial name characters, those matched by Letter | '_' | ':'
% AKA NameStartChar
range(R) when R == "\\i";
              R == "\\I" ->
   % initial name characters
   [{value, $:},
    {range, $A, $Z},
    {value, $_},
    {range, $a, $z},
    {range, 16#C0, 16#D6},
    {range, 16#D8, 16#F6},
    {range, 16#F8, 16#2FF},
    {range, 16#370, 16#37D},
    {range, 16#37F, 16#1FFF},
    {range, 16#200C, 16#200D},
    {range, 16#2070, 16#218F},
    {range, 16#2C00, 16#2FEF},
    {range, 16#3001, 16#D7FF},
    {range, 16#F900, 16#FDCF},
    {range, 16#FDF0, 16#FFFD},
    {range, 16#10000, 16#EFFFF}];

% the set of name characters, those matched by NameChar
%% [4]  NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] 
%%                        | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | 
%%                        [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] 
%%                        | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] 
%%                        | [#x10000-#xEFFFF]
%% [4a] NameChar      ::= NameStartChar | "-" | "." | [0-9] | #xB7 | 
%%                        [#x0300-#x036F] | [#x203F-#x2040]
range(R) when R == "\\c";
              R == "\\C" ->
   % name char
   [{range, $-, $.},
    {range, $0, $:},
    {range, $A, $Z},
    {value, $_},
    {range, $a, $z},
    {value, 16#B7},
    {range, 16#C0, 16#D6},
    {range, 16#D8, 16#F6},
    {range, 16#F8, 16#37D},
    {range, 16#37F, 16#1FFF},
    {range, 16#200C, 16#200D},
    {range, 16#203F, 16#2040},
    {range, 16#2070, 16#218F},
    {range, 16#2C00, 16#2FEF},
    {range, 16#3001, 16#D7FF},
    {range, 16#F900, 16#FDCF},
    {range, 16#FDF0, 16#FFFD},
    {range, 16#10000, 16#EFFFF}];
range(R) when R == "\\d";
              R == "\\D" ->
   range("Nd");
% [#x0000-#x10FFFF]-[\p{P}\p{Z}\p{C}]
% (all characters except the set of "punctuation", 
% "separator" and "other" characters)
% subtract([{range, 16#0, 16#10FFFF]}, union(range("P"), union(range("Z"), range("C")))).
range(R) when R == "\\w";
              R == "\\W" ->
   [{value,36},
    {value,43},
    {range,48,57},
    {range,60,62},
    {range,65,90},
    {value,94},
    {range,96,122},
    {value,124},
    {value,126},
    {range,162,166},
    {range,168,170},
    {value,172},
    {range,174,181},
    {range,184,186},
    {range,188,190},
    {range,192,893},
    {range,895,902},
    {range,904,1369},
    {range,1376,1416},
    {range,1421,1469},
    {value,1471},
    {range,1473,1474},
    {range,1476,1477},
    {range,1479,1522},
    {range,1542,1544},
    {value,1547},
    {range,1550,1562},
    {range,1568,1641},
    {range,1646,1747},
    {range,1749,1756},
    {range,1758,1791},
    {range,1808,2038},
    {range,2042,2095},
    {range,2112,2141},
    {range,2144,2273},
    {range,2275,2403},
    {range,2406,2415},
    {range,2417,2556},
    {range,2558,2677},
    {range,2689,2799},
    {range,2801,3203},
    {range,3205,3571},
    {range,3585,3662},
    {range,3664,3673},
    {range,3713,3843},
    {value,3859},
    {range,3861,3897},
    {range,3902,3972},
    {range,3974,4047},
    {range,4053,4056},
    {range,4096,4169},
    {range,4176,4346},
    {range,4348,4959},
    {range,4969,5119},
    {range,5121,5740},
    {range,5743,5759},
    {range,5761,5786},
    {range,5792,5866},
    {range,5870,5940},
    {range,5952,6099},
    {value,6103},
    {range,6107,6143},
    {range,6155,6157},
    {range,6160,6467},
    {range,6470,6685},
    {range,6688,6815},
    {value,6823},
    {range,6832,7001},
    {range,7009,7163},
    {range,7168,7226},
    {range,7232,7293},
    {range,7296,7359},
    {range,7376,7378},
    {range,7380,8191},
    {value,8260},
    {value,8274},
    {range,8304,8316},
    {range,8319,8332},
    {range,8336,8967},
    {range,8972,9000},
    {range,9003,10087},
    {range,10102,10180},
    {range,10183,10213},
    {range,10224,10626},
    {range,10649,10711},
    {range,10716,10747},
    {range,10750,11512},
    {value,11517},
    {range,11520,11631},
    {range,11647,11775},
    {value,11823},
    {range,11904,12287},
    {range,12292,12295},
    {range,12306,12307},
    {range,12320,12335},
    {range,12337,12348},
    {range,12350,12447},
    {range,12449,12538},
    {range,12540,42237},
    {range,42240,42508},
    {range,42512,42610},
    {range,42612,42621},
    {range,42623,42737},
    {range,42752,43123},
    {range,43136,43213},
    {range,43216,43255},
    {value,43259},
    {range,43261,43309},
    {range,43312,43358},
    {range,43360,43456},
    {range,43471,43485},
    {range,43488,43611},
    {range,43616,43741},
    {range,43744,43759},
    {range,43762,44010},
    {range,44012,55295},
    {range,63744,64829},
    {range,64848,65039},
    {range,65056,65071},
    {value,65122},
    {range,65124,65127},
    {value,65129},
    {range,65136,65278},
    {value,65284},
    {value,65291},
    {range,65296,65305},
    {range,65308,65310},
    {range,65313,65338},
    {value,65342},
    {range,65344,65370},
    {value,65372},
    {value,65374},
    {range,65382,65528},
    {range,65532,65791},
    {range,65799,66462},
    {range,66464,66511},
    {range,66513,66926},
    {range,67072,67670},
    {range,67672,67870},
    {range,67872,67902},
    {range,67968,68175},
    {range,68192,68222},
    {range,68224,68335},
    {range,68352,68408},
    {range,68416,68504},
    {range,68521,69460},
    {range,69632,69702},
    {range,69714,69818},
    {range,69840,69951},
    {range,69956,70003},
    {range,70006,70084},
    {range,70089,70092},
    {range,70096,70106},
    {value,70108},
    {range,70113,70199},
    {range,70206,70312},
    {range,70320,70730},
    {range,70736,70746},
    {range,70750,70853},
    {range,70855,71104},
    {range,71128,71232},
    {range,71236,71263},
    {range,71296,71483},
    {range,71487,71738},
    {range,71840,72254},
    {range,72263,72345},
    {value,72349},
    {range,72384,72768},
    {range,72784,72815},
    {range,72818,73462},
    {range,73728,74863},
    {range,74880,92781},
    {range,92880,92916},
    {range,92928,92982},
    {range,92988,92995},
    {range,92997,93846},
    {range,93952,113822},
    {range,118784,119154},
    {range,119163,121478},
    {range,121499,125277},
    {range,126065,917504},
    {range,917760,983039},
    {range,1114110,1114111}];
range(Unknown)  -> {Unknown}.