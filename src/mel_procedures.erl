%%------------------------------------------------
%% Berlin Brown
%% Copyright Botnode.com(Berlin Brown) 2008
%% File: mel_procedures.erl
%% Overview: Mel, "My Embedded Language" is a simple scheme implementation
%%           in Erlang, based on JScheme.
%% Date: 7/15/2008
%%
%% References:
%% http://www.erlang.se/doc/programming_rules.shtml
%% http://www.erlang.org/doc/reference_manual/part_frame.html
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
-module(mel_procedures).
-include("mel_records.hrl").
-import(mel_scheme_util, [cons/2, sleep/1]).
-import(mel_scheme_util, [get_type/1, first/1, second/1,rest/1]).
-import(mel_scheme_util, [define/3, define_procedure/5, define_procedure/4]).
-export([num_compute/3, proc_apply/2, install_procedures/1, new_procedure/4]).

-define(PLUS,   27).
-define(MINUS,  21).
-define(TIMES,  34).
-define(DIVIDE, 14).
-define(THIRD,  113).
-define(CONS,   13).
-define(CAR,    10).
-define(CDR,    30).

new_procedure(Id, Name, MinArgs, MaxArgs) ->
	Proc = #proc_info{
      name = Name,
	  min_args = MinArgs,
      max_args = MaxArgs,
      id = Id      
	 },
	Proc.

num_compute(Args, Op, Result) ->
    case Args of
        [] ->
			case Op of
				$\- -> Result - 0.0;
				$\/ -> Result / 1.0;
				_ -> result
            end; % End Case Op
		_ ->
            num_compute_while(Args, Op, Result)
    end.
%%----------------------------------------------------------------------
%% Function: num_compute_while/3
%% Args:    List of Args, Operation, Prev Result
%% Purpose: Perform calculation on the given args with the given Op
%%          and continue until no more args are left.
%% Returns: The Pair/Result after the procedure has been applied
%%          Result from the procedure (E.g. float value)
%%----------------------------------------------------------------------
num_compute_while(Args, Op, Result) ->
                                                % Perform the operation against the linked list
    case (get_type(Args) == pair) of
        false -> 
                                                % Exit with the result
            Result;
        true ->
            X = first(Args),
                                                % Traverse      
            MoreArgs = rest(Args),
            NewRes = case Op of
                         '+' -> X + Result;
                         '-' -> X - Result;
                         '*' -> X * Result;
                         '/' -> X / Result;
                         _ ->
                             io:format("Internal Error: unrecognized op: [~p]~n",[Op]),
                             -1
                                                % End of Op Case
                     end,
                                                % Continue with loop
                                                % and return some result
            num_compute_while(MoreArgs, Op, NewRes)
    end. % End Pair Type Check
			
%%----------------------------------------------------------------------
%% Function: proc_apply/2
%% Args:    Procedure Record (proc_info), List of arguments
%% Returns: The Pair/Result after the procedure has been applied
%%          Result from the procedure (E.g. float value)
%%----------------------------------------------------------------------
proc_apply(Proc, Args) ->
    io:format("~ntrace: proc_apply => [~p]~n", [Proc]),
    NArgs = length(Args),
    X = first(Args),
    Y = second(Args),
    MinArgs = Proc#proc_info.min_args,
    MaxArgs = Proc#proc_info.max_args,
    Id = Proc#proc_info.id,
    Name = Proc#proc_info.name,
    case Id of
        ?PLUS ->
            num_compute(Args, '+', 0.0);
        ?MINUS ->
            num_compute(rest(Args), '-', X);
        ?TIMES ->
            num_compute(Args, '*', 1.0);
        ?DIVIDE ->
            num_compute(rest(Args), '/', X);
        ?CONS ->
            cons(X, Y);
        ?CAR ->
            first(X);
        ?CDR ->
            rest(x)
    end.

install_procedures(Env) ->
    N = 9999,
                                                % Lazy approach for keeping 
												% state of Env
    E2 = define_procedure(Env, "cons",      ?CONS,      2),	    
    E3 = define_procedure(E2, "cdr",     	?CDR,       1),    	    	     	
    E4 = define_procedure(E3, "*",       	?TIMES,     0, N),     
    E5 = define_procedure(E4, "+",       	?PLUS,      0, N),
    E6 = define_procedure(E5, "-",       	?MINUS,     1, N),
    E7 = define_procedure(E6, "/",       	?DIVIDE,    1, N),
    E7.

%% End of File
