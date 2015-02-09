%%------------------------------------------------
%% Berlin Brown
%% Copyright Botnode.com(Berlin Brown) 2008
%% File: mel_input_reader.erl
%% Overview: Mel, "My Embedded Language" is a simple scheme implementation
%%           in Erlang, based on JScheme.
%% Date: 7/15/2008
%%
%% References:
%% http://www.erlang.se/doc/programming_rules.shtml
%% http://www.erlang.org/doc/reference_manual/part_frame.html
%%
%%The different functions allowed in match_spec work like this:
%%
%% is_atom, is_constant, is_float, is_integer, is_list, 
%% is_number, is_pid, is_port, is_reference, is_tuple, 
%% is_binary, is_function
%%
%% Conversion BIF (built in functions):
%% list_to_integer(Value), list_to_float(Value),
%% integer_to_list(Value), float_to_list(Value)
%%------------------------------------------------
-module(mel_scheme_util).
-include("mel_records.hrl").
-export([cons/2, first/1, rest/1, second/1]).
-export([sleep/1]).
-export([new_environment/0, lookup/3, define/3]).
-export([define/3, define_procedure/5, define_procedure/4]).
-export([get_type/1, ycomb/1]).

-import(mel_procedures, [new_procedure/4]).

%%------------------------------------------------
%% Environment Utilies
%%------------------------------------------------
new_environment() ->
    dict:new().

lookup(Env, Key, Default) ->
    case dict:find(Key, Env) of
        {ok, Value} -> Value;
        error       -> Default
    end.

define(Env, Key, Value) ->
    dict:store(Key, Value, Env).

define_procedure(Env, Name, Id, MinArgs, MaxArgs) ->
    define(Env, Name, new_procedure(Id, Name, MinArgs, MaxArgs)).

define_procedure(Env, Name, Id, MinArgs) ->
    define(Env, Name, new_procedure(Id, Name, MinArgs, MinArgs)).

%%------------------------------------------------
%% Misc Utilities
%%------------------------------------------------

%% Description: Erlang Y Combinator.
%%              Used in mel_scheme module.
ycomb(M) ->
    G = fun (F) ->
				M(fun(A) -> 
						  (F(F))(A) 
				  end)
		end,
    G(G).

is_string([C | Cs]) when is_integer(C), C >= 0, C =< 16#10ffff ->
    is_string(Cs);
is_string([_ | _]) -> false;
is_string([]) -> true;
is_string(_) -> false.

get_type(Val, Status) when is_integer(Val) -> integer;
get_type(Val, Status) when is_list(Val) -> pair;
get_type(Val, Status) when is_float(Val) -> float;                            
get_type(Val, Status) when is_atom(Val) -> atom;
get_type(Val, Status) when is_function(Val) -> function;
get_type(Val, Status) when is_record(Val, proc_info) -> procedure;
get_type(_,   _) -> undefined.
get_type(Val) ->
    case is_string(Val) of
        true -> string;
        false -> get_type(Val, ok)
    end.

sleep(T) ->
    receive
        after T -> ok
        end.

%% cons(x, y) is the same as new Pair(x, y).
%% 
%% Cons presents and interesting function that is fundamental to lisp.
%% Here are some examples of cons usage (tested in common lisp).
%% <code>
%% (cons 1 2):
%% Pair pair = SchemeUtil.cons("1", "2");
%% assertEquals("" + pair, "(1 . 2)");
%% 
%% (cons 1 nil):
%% Pair pair = SchemeUtil.cons("1", null);
%% assertEquals("" + pair, "(1)");
%% 
%% (cons 1 (cons 2 nil)):
%% 
%% Pair pair = SchemeUtil.cons("1", SchemeUtil.cons("2", null));
%% assertEquals("" + pair, "(1 2)");
%%  
%% </code>
%%
cons(Fst, Rest) -> [Fst|Rest].

%% Like Common Lisp first; car of a Pair, or null for anything else.
first([Fst|Rest]) -> Fst;
first(_) -> [].

%% Like Common Lisp rest; car of a Pair, or null for anything else.
%% "A cons cell is composed of two pointers; the car operation 
%% extracts the first pointer, and the cdr operation extracts the second."
%% 
%% "Thus, the expression (car (cons x y)) evaluates to x, 
%% and (cdr (cons x y)) evaluates to y."
rest([Fst|Rest]) -> Rest;
rest(_) -> [].

second(X) ->
    first(rest(X)).
  
%% End of File
