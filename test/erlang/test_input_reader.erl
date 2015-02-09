%% File: test_input_reader.erl
%% Eunit oriented test cases.
%% Date: 7/24/2008
-module(test_input_reader).
-include_lib("eunit/include/eunit.hrl").

-include("mel_records.hrl").
-import(mel_input_reader, [is_whitespace/1, next_token/1]).
-import(mel_input_reader, [new_reader/1, stream_read/1, while_whitespace/1]).
-import(mel_input_reader, [new_stack/0, push_stack/2, pop_stack/1, peek_stack/1]).
-import(mel_input_reader, [read/1]).
-import(mel_scheme, [eval/1, eval/2, eval_all_expr/1]).

-export([simple_reader/0, test_while_ws/0, test_stack_main/0]).
-export([test_next_main/0, test_read_main/0, test_compute_main/0]).

whitespace_test_() -> 
[
 ?_assert(2 == 2),
 ?_assert(is_whitespace(1)  == false),
 ?_assert(is_whitespace(9)  == true),
 ?_assert(is_whitespace(10) == true),
 ?_assert(is_whitespace(11) == true),
 ?_assert(is_whitespace(12) == true),
 ?_assert(is_whitespace(32) == true)
].

simple_reader_test_() ->
[
 ?_assert(test_while_ws("") == -1),
 ?_assert(test_while_ws("     ") == -1),
 ?_assert(test_while_ws(" ") == -1),
 ?_assert(test_while_ws("  aa   ") == 97),
 ?_assert(test_while_ws("a") == 97)
].

simple_reader() ->
	R = new_reader("()"),
	{ A, B, C} = go_read(R),
	{ X, Y, Z} = go_read(C),
	Y.
go_read(Reader) ->
	{ Done, C, R } = stream_read(Reader),
	Strm = R#reader_info.reader_strm,
	io:format("trace: ==>~p~n", [R]),
	{ Done, C, R}.

test_while_ws() ->
	R = new_reader("   "),
	{ R2 } = while_whitespace(R),
    C = R2#reader_info.last_ch,
	io:format("trace: whitespace end at =>[~p]~n", [C]),
	C.
test_while_ws(Input) ->
	R = new_reader(Input),
	{ R2 } = while_whitespace(R),
    C = R2#reader_info.last_ch,
	io:format("trace: whitespace end at =>[~p]~n", [C]),
	C.

test_stack_main() ->
	A = new_stack(),
	B = push_stack(A, 97),	
	io:format("t==>[~p]~n", [B]),
	{ ok, H, R } = pop_stack(B),	
	io:format("t==>[~p]~n", [R]),
	D = pop_stack(R),	
	io:format("t==>[~p]~n", [D]).

test_next_main() ->
	R = new_reader("  (+ 1 2 3)  "),
	D = next_token(R),
	io:format("Test Next Token=>~p~n", [D]),
    D.              

test_read_main() ->
	R = new_reader("   (+ 1 2 3 4 5 6 )   "),
	{ Pair, RUpt } = read(R),
	io:format("Test Read=>~p~n", [Pair]),
    Pair.

test_eval_main() ->
	R = new_reader("        (   / (+ 4 4) (+ 2 2 ))  "),    
    { R2 } = while_whitespace(R),
    io:format("Test While White Space =>~p~n", [R2]),
	{ Pair, RUpt } = read(R),
	io:format("Test Read=>~p~n", [Pair]),
    Pair2 = eval(Pair),
    io:format("Pair => [~p]", [Pair2]),
    Pair.

test_compute_main() ->
 	R = new_reader("   (cons 1 (cons 2 3))  (+ 1 2)"), 
	{ R2 } = while_whitespace(R),
	io:format("Test While White Space =>~p~n", [R2]),
 	{ Pair, RUpt } = read(R),
 	io:format("Test Read=>~p~n", [Pair]),
	Pair2 = eval(Pair),
	io:format("Pair => [~p]~n", [Pair2]),
 	X = new_reader("   "), 
 	{ YPair, RYUpt } = read(X),
 	io:format("~nTest EOF=>~p, ~p~n", [YPair, RYUpt]),  
	EvalRes = eval_all_expr("    (+ 1 2) (+ 4 5) (* 3 (+ 4 9)) (cons 1 2) "),
	io:format("~nTest Eval All=>~p~n", [EvalRes]),	
    [].
% End of File
