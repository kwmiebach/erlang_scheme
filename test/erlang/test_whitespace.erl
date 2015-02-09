%% File: test_whitespace.erl
%% EUnit oriented test cases for testing the
%% whitespace checks.
-module(test_whitespace).
-include_lib("eunit/include/eunit.hrl").
-import(mel_input_reader, [is_whitespace/1]).

whitespace_test_() -> 
[
 ?_assert(2 == 2),
 ?_assert(is_whitespace(1)  == false),
 ?_assert(is_whitespace(9)  == true),
 ?_assert(is_whitespace(10) == true),
 ?_assert(is_whitespace(11) == true),
 ?_assert(is_whitespace(12) == true),
 ?_assert(is_whitespace(13) == true)
].

% End of File
