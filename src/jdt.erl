%%%-------------------------------------------------------------------
%%% @author Antonio SJ Musumeci <bile@landofbile.com>
%%% @copyright (C) 2013, Antonio SJ Musumeci
%%% @doc
%%%
%%% @end
%%% Created : 5 Jan 2013 by Antonio SJ Musumeci <bile@landofbile.com>
%%%-------------------------------------------------------------------
-module(jdt).

-compile(inline).
-compile({inline_size,100}).

-export([
         new/0,
         set/3,set/2,
         setn/2,setn/1,
         merge/2,
         unset/2,
         unsetn/2,
         get/3,get/2,
         find/2,
         mask/2,mask/3,
         fmask/2,fmask/3,
         fold/3
        ]).

-export([
         from_proplist/2,from_proplist/1,
         to_proplist/1
        ]).

-type key() :: binary().
-type value() :: binary() | number() | object() | boolean() | null | [value()].
-type kv() :: {key(),value()}.
-type object() :: {[kv()]}.
-type path() :: [key() | non_neg_integer()].
-type fpath() :: [key() | non_neg_integer() | null].

-export_type([key/0,value/0,kv/0,object/0,path/0,fpath/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec new() -> object().
new() ->
    {[]}.

-spec set(key() | path(),value(),object()) -> object().
set([Key|_]=Path,Value,{Proplist}=JDT)
  when is_binary(Key),is_list(Proplist) ->
    '_set'(Path,Value,JDT);
set(Key,Value,{Proplist}=JDT)
  when is_binary(Key),is_list(Proplist) ->
    '_set'([Key],Value,JDT);
set([],_Value,{Proplist}=JDT)
  when is_list(Proplist) ->
    JDT;
set(_,_,_) ->
    erlang:error(badarg).

-spec set(key() | path(),value()) -> object().
set(Key,Value) ->
    set(Key,Value,new()).

-spec setn([path()],object()) -> object().
setn([{Key,Value}|KVPairs],{Proplist}=JDT)
  when is_list(Proplist) ->
    NewJDT = set(Key,Value,JDT),
    setn(KVPairs,NewJDT);
setn([],JDT) ->
    JDT;
setn([_|KVPairs],JDT) ->
    setn(KVPairs,JDT).

-spec setn([path()]) -> object().
setn(KVPairs) ->
    JDT = jdt:new(),
    setn(KVPairs,JDT).

-spec merge(object(),object()) -> object().
merge(From,To) ->
    '_merge'(From,To).

-spec unset(path(),object()) -> object().
unset([Key|_]=Path,{Proplist}=JDT)
  when is_binary(Key),is_list(Proplist) ->
    '_unset'(Path,JDT).

-spec unsetn([path()],object()) -> object().
unsetn([Key|Keys],{Proplist}=JDT)
  when is_list(Key),is_list(Proplist) ->
    NewJDT = unset(Key,JDT),
    unsetn(Keys,NewJDT);
unsetn([],JDT) ->
    JDT;
unsetn([_|KVPairs],JDT) ->
    unsetn(KVPairs,JDT).

-spec get(key() | path(),object()) -> value().
get([Key|_]=Path,{Proplist}=JDT)
  when is_binary(Key),is_list(Proplist) ->
    '_get'(Path,JDT);
get(Key,{Proplist}=JDT)
  when is_binary(Key),is_list(Proplist) ->
    '_get'([Key],JDT);
get([],{Proplist}=JDT)
  when is_list(Proplist) ->
    JDT;
get(_,_) ->
    erlang:error(badarg).

-spec get(path(),object(),value()) -> value().
get([Key|_]=Path,{Proplist}=JDT,Default)
  when is_binary(Key),is_list(Proplist) ->
    case '_get'(Path,JDT) of
        undefined ->
            Default;
        RV ->
            RV
    end;
get([],{Proplist}=_JDT,Default)
  when is_list(Proplist) ->
    Default;
get(_,_,_) ->
    erlang:error(badarg).

-spec find(fpath(),object()) -> [value()].
find(Path,JDT) ->
    '_find'(Path,JDT,[]).

-spec mask([path()],object(),object()) -> object().
mask([Path|_]=Paths,Source,Target)
  when is_list(Path) ->
    '_mask'(Paths,Source,Target);
mask(Path,Source,Target)
  when is_list(Path) ->
    '_mask'([Path],Source,Target);
mask(_,_,_) ->
    erlang:error(badarg).

-spec mask([path()],object()) -> object().
mask(Path,Source) ->
    mask(Path,Source,new()).

-spec fmask(fpath(),object(),object()) -> object().
fmask(Path,Source,Target)
  when is_list(Path) ->
    '_fmask'(Path,[],Source,Target);
fmask(_,_,_) ->
    erlang:error(badarg).

-spec fmask(fpath(),object()) -> object().
fmask(Path,Source) ->
    fmask(Path,Source,new()).

-spec fold(fun((_,_,_) -> any()),any(),object()) -> any().
fold(Fun,Acc,{Proplist})
  when is_function(Fun,3),is_list(Proplist) ->
    '_fold'(Fun,Acc,Proplist);
fold(_,_,_) ->
    erlang:error(badarg).

-spec from_proplist([atom() | {atom() | binary(),_}],object()) -> object().
from_proplist(Proplist,{JDTPL}=JDT)
  when is_list(Proplist),is_list(JDTPL) ->
    '_from_proplist'(Proplist,JDT);
from_proplist(_,_) ->
    erlang:error(badarg).

-spec from_proplist([atom() | {atom() | binary(),_}]) -> object().
from_proplist(Proplist)
  when is_list(Proplist) ->
    '_from_proplist'(Proplist,new());
from_proplist(_) ->
    erlang:error(badarg).

-spec to_proplist(object()) -> any().
to_proplist({Proplist}) ->
    Proplist;
to_proplist(_) ->
    erlang:error(badarg).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec setnth(non_neg_integer(),_,maybe_improper_list(),[binary() | non_neg_integer()]) -> nonempty_maybe_improper_list().
setnth(N,Value,[H|T],Path)
  when N > 0 ->
    [H|setnth(N-1,Value,T,Path)];
setnth(N,Value,[],Path)
  when N > 0 ->
    [null|setnth(N-1,Value,[],Path)];
setnth(0,Value,[H|T],Path) ->
    ['_set'(Path,Value,H)|T];
setnth(0,Value,[],Path) ->
    ['_set'(Path,Value,[])].

-spec unsetnth(_,maybe_improper_list(),_) -> any().
unsetnth(N,[H|T],Path)
  when N > 0 ->
    [H|unsetnth(N-1,T,Path)];
unsetnth(N,[],_Path)
  when N > 0 ->
    [];
unsetnth(0,[_|T],[]) ->
    T;
unsetnth(0,[{Proplist}|T],Path)
  when is_list(Proplist) ->
    ['_unset'(Path,{Proplist})|T];
unsetnth(0,[],_Path) ->
    [].

-spec '_set'([binary() | non_neg_integer()],_,_) -> any().
'_set'([Key|Path],Value,{Proplist})
  when is_binary(Key),is_list(Proplist) ->
    NewKV =
        case lists:keyfind(Key,1,Proplist) of
            {_,KeyValue} ->
                '_set'(Path,Value,KeyValue);
            false ->
                '_set'(Path,Value,{[]})
        end,
    {lists:keystore(Key,1,Proplist,{Key,NewKV})};
'_set'([Key|Path],Value,Data)
  when is_binary(Key) ->
    {[{Key,'_set'(Path,Value,Data)}]};
'_set'([Key|Path],Value,List)
  when is_integer(Key),is_list(List) ->
    setnth(Key,Value,List,Path);
'_set'([Key|Path],Value,_)
  when is_integer(Key) ->
    setnth(Key,Value,[],Path);
'_set'([],Value,_) ->
    Value.

-spec '_unset'(nonempty_maybe_improper_list(),maybe_improper_list() | {maybe_improper_list()}) -> any().
'_unset'([Key],{Proplist})
  when is_binary(Key),is_list(Proplist) ->
    {lists:keydelete(Key,1,Proplist)};
'_unset'([Key],List) ->
    unsetnth(Key,List,[]);
'_unset'([Key|Path],{Proplist})
  when is_binary(Key) ->
    case lists:keyfind(Key,1,Proplist) of
        {_,KeyValue} ->
            NewValue = '_unset'(Path,KeyValue),
            {lists:keystore(Key,1,Proplist,{Key,NewValue})};
        false ->
            {Proplist}
    end;
'_unset'([Key|Path],List)
  when is_integer(Key),is_list(List) ->
    unsetnth(Key,List,Path).

-spec '_merge'(_,_) -> any().
'_merge'({[{Key,Value}|FromList]},{ToList}) ->
    NewKV =
        case lists:keyfind(Key,1,ToList) of
            {_,KeyValue} ->
                '_merge'(Value,KeyValue);
            false ->
                Value
        end,
    NewToList = lists:keystore(Key,1,ToList,{Key,NewKV}),
    '_merge'({FromList},{NewToList});
'_merge'([FValue|FromList],[TValue|ToList]) ->
    ['_merge'(FValue,TValue)|'_merge'(FromList,ToList)];
'_merge'([],ToList)
  when is_list(ToList) ->
    ToList;
'_merge'(FromList,[])
  when is_list(FromList) ->
    FromList;
'_merge'({[]},To) ->
    To;
'_merge'(From,_To) ->
    From.

-spec getnth(integer(),_) -> any().
getnth(N,[_|T])
  when N > 0 ->
    getnth(N-1,T);
getnth(0,[H|_]) ->
    H;
getnth(_N,_L) ->
    undefined.

-spec '_get'(_,_) -> any().
'_get'([Key|Path],{Proplist})
  when is_binary(Key),is_list(Proplist) ->
    case lists:keyfind(Key,1,Proplist) of
        {_,Value} ->
            '_get'(Path,Value);
        false ->
            undefined
    end;
'_get'([Key|Path],List)
  when is_integer(Key),is_list(List) ->
    '_get'(Path,getnth(Key,List));
'_get'([],Value) ->
    Value;
'_get'(_,_) ->
    undefined.

-spec '_find'(_,_,[any()]) -> [any()].
'_find'([null|Path],{[{_Key,Value}|List]},Acc) ->
    NewAcc = '_find'(Path,Value,Acc),
    '_find'([null|Path],{List},NewAcc);
'_find'([Key|Path],{Proplist},Acc)
  when is_binary(Key),is_list(Proplist) ->
    case lists:keyfind(Key,1,Proplist) of
        {_,Value} ->
            '_find'(Path,Value,Acc);
        false ->
            Acc
    end;
'_find'([Key|Path],List,Acc)
  when is_integer(Key),is_list(List) ->
    '_find'(Path,getnth(Key,List),Acc);
'_find'([],Value,Acc) ->
    [Value|Acc];
'_find'(_,_,Acc) ->
    Acc.

-spec '_from_proplist'([atom() | {atom() | binary() | [any()],_}],_) -> any().
'_from_proplist'([{Key,[{_VK,_VV}|_]=ValuePL}|Proplist],JDT) ->
    BinKey = to_binary(Key),
    ValueJDT = '_from_proplist'(ValuePL,new()),
    NewJDT = '_set'([BinKey],ValueJDT,JDT),
    '_from_proplist'(Proplist,NewJDT);
'_from_proplist'([{Key,Value}|Proplist],JDT) ->
    BinKey = to_binary(Key),
    NewJDT = '_set'([BinKey],Value,JDT),
    '_from_proplist'(Proplist,NewJDT);
'_from_proplist'([Key|Proplist],JDT)
  when is_atom(Key) ->
    BinKey = to_binary(Key),
    NewJDT = '_set'([BinKey],true,JDT),
    '_from_proplist'(Proplist,NewJDT);
'_from_proplist'([],JDT) ->
    JDT.

-spec '_mask'([any()],_,_) -> any().
'_mask'([Path|Paths],Source,Target)
  when is_list(Path) ->
    case '_get'(Path,Source) of
        undefined ->
            '_mask'(Paths,Source,Target);
        Value ->
            NewTarget = '_set'(Path,Value,Target),
            '_mask'(Paths,Source,NewTarget)
    end;
'_mask'([_|Paths],Source,Target) ->
    '_mask'(Paths,Source,Target);
'_mask'([],_Source,Target) ->
    Target.

-spec '_fmask'(fpath(),list(),_,_) -> object().
'_fmask'([Key|Path],RevPath,{Source},Target)
  when is_binary(Key),is_list(Source) ->
    case lists:keyfind(Key,1,Source) of
        {_,Value} ->
            '_fmask'(Path,[Key|RevPath],Value,Target);
        false ->
            Target
    end;
'_fmask'([Key|Path],RevPath,Source,Target)
  when is_integer(Key),is_list(Source) ->
    case getnth(Key,Source) of
        undefined ->
            Target;
        Value ->
            '_fmask'(Path,[Key|RevPath],Value,Target)
    end;
'_fmask'([null|Path],RevPath,{Source},Target)
  when is_list(Source) ->
    Fun = fun({Key,_},Acc) ->
                  '_fmask'([Key|Path],RevPath,{Source},Acc)
          end,
    lists:foldl(Fun,Target,Source);
'_fmask'([null|Path],RevPath,List,Target)
  when is_list(List) ->
    Fun = fun(Value,{TAcc,Count}) ->
                  {'_fmask'(Path,[Count|RevPath],Value,TAcc),Count+1}
          end,
    {NewTarget,_} = lists:foldl(Fun,{Target,0},List),
    NewTarget;
'_fmask'([],RevPath,Source,{Target}) ->
    Path = lists:reverse(RevPath),
    '_set'(Path,Source,{Target});
'_fmask'(_,_,_,Target) ->
    Target.

-spec '_fold'(fun((_,_,_) -> any()),_,[any()]) -> any().
'_fold'(UserFun,InitAcc,Proplist)
  when is_list(Proplist) ->
    FoldFun = fun({Key,Value},Acc) ->
                      UserFun(Key,Value,Acc)
              end,
    lists:foldl(FoldFun,InitAcc,Proplist).

-spec to_binary(atom() | binary() | [any()]) -> binary().
to_binary(Atom)
  when is_atom(Atom) ->
    erlang:atom_to_binary(Atom,utf8);
to_binary(Binary)
  when is_binary(Binary) ->
    Binary;
to_binary(List)
  when is_list(List) ->
    erlang:iolist_to_binary(convert_atoms(List)).

-spec convert_atoms([any()]) -> [any()].
convert_atoms([H|T])
  when is_atom(H) ->
    [to_binary(H)|convert_atoms(T)];
convert_atoms([H|T])
  when is_list(H) ->
    [convert_atoms(H)|convert_atoms(T)];
convert_atoms([H|T]) ->
    [H|convert_atoms(T)];
convert_atoms([]) ->
    [].
