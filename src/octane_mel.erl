%%------------------------------------------------
%% Berlin Brown - <berlin dot brown at gmail.com>
%% Date: 7/23/2008
%% Overview: Mel, "My Embedded Language" is a simple scheme implementation
%%           in Erlang, based on JScheme.
%% See: http://octane-lang.googlecode.com/svn/trunk/misc/tutorial/jscheme_new/
%% Purpose: Main Entry point for MEL.
%% Test Environment: Eshell V5.6 (Win32)
%% File: octane_mel.erl.
%%
%% * This software is licensed under the BSD license.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 
%%   % Redistributions of source code must retain the above copyright
%%     notice, this list of conditions and the following disclaimer.
%%     
%%   % Redistributions in binary form must reproduce the above copyright
%%     notice, this list of conditions and the following disclaimer in the
%%     documentation and/or other materials provided with the distribution.
%%     
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
%% TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
%% PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
%% EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%------------------------------------------------

-module(octane_mel).
-import(mel_input_reader, [new_reader/1, stream_read/1, while_whitespace/1]).
-import(mel_input_reader, [read/1]).
-import(mel_scheme, [eval/1, eval/2, eval_all_expr/1]).
-export([mel_main/0]).

eval_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    io:format("Binary Data [ ~p ]~n", [Bin]),
    EvalRes = eval_all_expr(binary_to_list(Bin)),
    io:format("Pair => [~p]", [EvalRes]),
    [].

mel_main() ->
    io:format("Running~n"),
    eval_file("test.scm").

%% End of File
