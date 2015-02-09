%% 
%% test_all.erl
%%
-module(test_all).
-export([run_tests/0, test_main/0]).

run_tests () ->
	test_whitespace:test(),
	test_input_reader:test().

test_main() ->
	io:format("Test Main~n"),
	test_input_reader:test_compute_main().
	
%% End of the File
