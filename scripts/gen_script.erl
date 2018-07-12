%% @author Zack
%% @doc @todo Add description to gen_script.


-module(gen_script).

%% ====================================================================
%% API functions
%% ====================================================================
-export([run/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================

% uses data from https://unicode.org/Public/UNIDATA/UnicodeData.txt
run() ->
   {ok,Fd} = file:open("UnicodeData.txt",
                       [read,raw,binary]),
   AllLines = one_line(Fd,[]),
   Split = split_lines(AllLines,[]),
   Filled = fill_missing_points(Split, []),
   Pids = start_all(),
   {ok,Out} = file:open("Output.txt", [write]),
   io:fwrite(Out,"~s~n", [erl_prettypr:format(do_send(Filled, Pids))]),
   file:close(Fd),
   file:close(Out).
   

one_line(Fd,Acc) ->
   case file:read_line(Fd) of
      eof ->
         Acc;
      {ok,Line} ->
         one_line(Fd,[Line|Acc])
   end.

split_lines([],Acc) -> Acc;
split_lines([Line|T], Acc) ->
   [Code,Rest1] = string:split(Line, <<";">>, leading),
   [_Desc,Rest2] = string:split(Rest1, <<";">>, leading),
   [SubCat,_] = string:split(Rest2, <<";">>, leading),
   split_lines(T, [[binary_to_integer(Code,16),SubCat]|Acc]).

% fill in unused codepoints to simplify ranges, but not decimal digits
fill_missing_points([H|T], []) -> 
   [H|fill_missing_points(T, H)];
fill_missing_points([[P,S]|T], [_,_]) when P >= 16#D800,
                                           P =< 16#DFFF -> % invalid
   fill_missing_points(T, [P,S]);
fill_missing_points([[P,S]|T], [LP,_]) when LP + 1 == P -> % not missing
   [[P,S]|fill_missing_points(T, [P,S])];
fill_missing_points([[_,_] = H|T], [LP,<<"Nd">>]) -> % is missing, but don't fill
   fill_missing_points([H|T], [LP +1,<<"Nd">>]);
fill_missing_points([[_,_] = H|T], [LP,LS]) -> % is missing
   [[LP +1,LS]|fill_missing_points([H|T], [LP +1,LS])];
fill_missing_points([], _) ->
   [].


range_collect() ->
   receive
      Int when is_integer(Int) ->
         range_collect(Int,Int,[]);
      {done, From, Cat} ->
         From ! {Cat, []}
   end.

range_collect(Min,Max,Acc) ->
   receive
      Int when is_integer(Int), Int == Max + 1 ->
         range_collect(Min,Int,Acc);
      Int when is_integer(Int), Min == Max ->
         range_collect(Int,Int,[{value,Min}|Acc]);
      Int when is_integer(Int) ->
         range_collect(Int,Int,[{range,Min,Max}|Acc]);
      {done, From, Cat} when Min == Max ->
         NewAcc = lists:reverse([{value,Min}|Acc]),
         From ! {Cat, NewAcc};
      {done, From, Cat} ->
         NewAcc = lists:reverse([{range,Min,Max}|Acc]),
         From ! {Cat, NewAcc}
   after 10000 ->
       Acc
   end.

rec_all([], Acc) -> 
   erl_syntax:revert(to_abstract(Acc));
rec_all([{K,_}|T], Acc) -> 
   io:format("Waiting on: ~p~n",[K]),
   receive
      {K,Vs} ->
         io:format("Got: ~p~n",[K]),
         rec_all(T, [{binary_to_list(K),Vs}|Acc])
   after 100000 ->
       Acc
   end.

   
do_send([], PidMap) ->
   Self = self(),
   List = maps:to_list(PidMap),
   [V ! {done,Self,K} || {K,V} <- List],
   rec_all(List,[]);
do_send([[Int,<<Cat:1/bytes,_/binary>> = SCat]|Rest], PidMap) ->
   (maps:get(Cat, PidMap)) ! Int,
   (maps:get(SCat, PidMap)) ! Int,
   do_send(Rest, PidMap).

do_spawn() ->
   erlang:spawn(fun() -> range_collect() end).

start_all() ->
   #{ <<"Cc">> => do_spawn(),
      <<"Cf">> => do_spawn(),
      <<"Cn">> => do_spawn(),
      <<"Co">> => do_spawn(),
      <<"Cs">> => do_spawn(),
       <<"C">> => do_spawn(),
      
      <<"Ll">> => do_spawn(),
      <<"Lm">> => do_spawn(),
      <<"Lo">> => do_spawn(),
      <<"Lt">> => do_spawn(),
      <<"Lu">> => do_spawn(),
       <<"L">> => do_spawn(),
      
      <<"Mc">> => do_spawn(),
      <<"Me">> => do_spawn(),
      <<"Mn">> => do_spawn(),
       <<"M">> => do_spawn(),
      
      <<"Nd">> => do_spawn(),
      <<"Nl">> => do_spawn(),
      <<"No">> => do_spawn(),
       <<"N">> => do_spawn(),
      
      <<"Pc">> => do_spawn(),
      <<"Pd">> => do_spawn(),
      <<"Pe">> => do_spawn(),
      <<"Pf">> => do_spawn(),
      <<"Pi">> => do_spawn(),
      <<"Po">> => do_spawn(),
      <<"Ps">> => do_spawn(),
       <<"P">> => do_spawn(),
      
      <<"Sc">> => do_spawn(),
      <<"Sk">> => do_spawn(),
      <<"Sm">> => do_spawn(),
      <<"So">> => do_spawn(),
       <<"S">> => do_spawn(),
      
      <<"Zl">> => do_spawn(),
      <<"Zp">> => do_spawn(),
      <<"Zs">> => do_spawn(),
       <<"Z">> => do_spawn()
    }.

to_abstract(List) ->
   FunName = erl_syntax:atom(range),
   FunClauses = [clause(Li) || Li <- lists:sort(List)],
   erl_syntax:function(FunName, FunClauses).

clause({Pattern,[]}) ->
   P = erl_syntax:string(Pattern),
   B = erl_syntax:abstract([{value,0}]),
   erl_syntax:clause([P],[],[B]);
clause({Pattern,Vals}) ->
   P = erl_syntax:string(Pattern),
   B = erl_syntax:abstract(Vals),
   erl_syntax:clause([P],[],[B]).
