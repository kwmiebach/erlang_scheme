%%
%% File: mel_object.erl
%% Simple Java style operations and "vocabulary" for Erlang.
%%
-module(mel_object).
-export([new_object/0, define_var/3, get_var/3, define_method/3, call_method/3]).
-export([return_object/1]).

new_object() ->
    dict:new().

define_var(Object, Key, Value) ->
    dict:store(Key, Value, Object).

get_var(Object, Key, Default) ->
    case dict:find(Key, Object) of
        {ok, Value} -> Value;
        error       -> Default
    end.

define_method(Object, Name, Func) ->
    define_var(Object, Name, Func).

call_method(Object, Name, Args) ->
    DefFunc = fun(Obj, X) -> [] end,
    Func = get_var(Object, Name, DefFunc),
    WrapperFunc = fun(Obj, A) ->
                          io:format("trace: object call =>~p~n", [Name]),
                          Func(Obj, A)
                  end,
    WrapperFunc(Object, Args).

return_object(Object) -> Object.

%% End of File
