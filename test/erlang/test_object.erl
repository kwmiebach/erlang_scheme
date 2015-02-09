%% Purpose: test case for object functions.
%% File: test_object.erl

-module(test_object).
-include_lib("eunit/include/eunit.hrl").
-include("mel_records.hrl").

-import(mel_object, [new_object/0, define_var/3, get_var/3, define_method/3, call_method/3]).
-import(mel_object, [return_object/1]).
-export([test_object_main/0]).

object_test_()  ->
[
 ?_assert(2 == 2)
].

proc_add() ->
    Func = fun(Object, Args) ->
                   io:format("method call <add>, args=>~p~n", [Args]),
                   A = lists:nth(1, [1,2]),
                   B = lists:nth(2, [1,2]),
                   A + B
           end,
    Func.

proc_add_new() ->
    Func = fun(Object, Args) ->
                   io:format("method call <add>, args=>~p~n", [Args]),
                   A = get_var(Object, "arg1", 0),
                   B = get_var(Object, "arg2", 0),
                   A + B
           end,
    Func.

test_object_main() ->
    io:format("objec test"),
    Object = new_object(),
    Obj2 = define_method(Object, "+", proc_add()),
    Res = call_method(Obj2, "+", [1, 2]),
    io:format("Result=>~p~n", [Res]),
                                                % Test the new ADD
    O3 = define_method(Object, "new_add", proc_add_new()),
    O4 = define_var(define_var(O3, "arg1", 4), "arg2", 5),
    Res2 = call_method(O4, "new_add", []),
    io:format("Result=>~p~n", [Res2]), 
    return_object(O3).
   
%% End of File


