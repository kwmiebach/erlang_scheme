%%------------------------------------------------
%% Berlin Brown
%% File: mel_input_reader.erl
%% Date: 7/15/2008
%% Overview: Mel, "My Embedded Language" is a simple scheme implementation
%%           in Erlang, based on JScheme.
%% References:
%% http://www.erlang.se/doc/programming_rules.shtml
%% http://www.erlang.org/doc/reference_manual/part_frame.html
%%
%% License:
%%
%% Copyright (c) 2008, Botnode.com (Berlin Brown)
%%; All rights reserved.

%% Redistribution and use in source and binary forms, with or without 
%% modification, are permitted provided that the following conditions are met:

%%    * Redistributions of source code must retain the above copyright 
%%      notice, this list of conditions and the following disclaimer.

%%    * Redistributions in binary form must reproduce the above copyright 
%%      notice, this list of conditions and the following disclaimer 
%%      in the documentation and/or other materials provided with the distribution.

%%    * Neither the name of the Botnode.com(Berlin Brown) nor the names of its
%%      contributors may be used to endorse or promote products 
%%      derived from this software without specific prior written permission.

%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
%% FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT 
%% SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED 
%% AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
%% OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
%% SUCH DAMAGE.
%%------------------------------------------------
-module(mel_scheme).
-include("mel_records.hrl").
-import(mel_scheme_util, [sleep/1, get_type/1, ycomb/1]).
-import(mel_scheme_util, [cons/2, first/1, rest/1]).
-import(mel_scheme_util, [new_environment/0, lookup/3, define/3]).
-import(mel_procedures, [proc_apply/2, new_procedure/3, install_procedures/1]).
-import(mel_input_reader, [new_reader/1, read/1]).
-export([eval/1, eval/2, eval_all_expr/1]).

eval(Object) ->
    eval(Object, install_procedures(new_environment())).   

eval(Object, Env) ->    
    Type = get_type(Object),
    io:format("trace: eval[t0] - eval type=> [~p]/{~p}~n", [Type, Object]),
    if
        Type == string ->
                                                % Return the object value from the Environment
            lookup(Env, Object, []);
        not (Type == pair) ->
                                                % Return Constant
            Object;
        true ->
                                                % Handle a Pair
            Fn = first(Object),
            Args = rest(Object),
            io:format("trace: eval[t1] - instance of procedure call [~p]~n", [Fn]),
                                                % Eval should perform a lookup and return
                                                % The procedure record
            ProcRec = eval(Fn, Env),
                                                % Return value after computation
            proc_apply(ProcRec, eval_list(Args, Env))
    end.
    
eval_list([], Env) -> [];
eval_list(List, Env) when not is_list(List) ->
    io:format("Illegal arg list: ~p~n", [List]),
    [];
eval_list(List, Env) ->
    First = eval(first(List), Env),
    Rest = eval_list(rest(List), Env),
    cons(First, Rest).

%%
%% Purpose: Evaluate all expressions in this file until
%%          a end of file is reached.
eval_all_expr(Data) ->
	RNew = new_reader(Data),
	EvalAll = fun(F) ->					  
					  fun(RUpt) ->
							  {Pair, RTmpUpt} = read(RUpt),
							  case Pair of
												% EOF, exit with true
								  "#!EOF" -> true;
								  []      -> true;
								  _ ->
												% Else, evaluate
									  ResObj = eval(Pair),
									  io:format("last-eval =>~p~n", [ResObj]),
									  F(RTmpUpt)
							  end
					  end % End of First Fun
			  end,
	FirstEvalChk = fun(PairRs, RUptData) ->
												% First check function, check for EOF
												% Otherwise, 
												% call the ycombinator on
												% EvalAll
						   NewReadrTupl = {PairRs, RUptData},
						   case NewReadrTupl of
							   { "#!EOF", R } -> true;
							   { [],      R } -> true;
							   {  _,      R } -> (ycomb(EvalAll))(R)
						   end
				   end,
	%% Invoke the first evaluation expression check
	{ PrNew, RUptNew } = read(RNew),
	PrRes = eval(PrNew),
	FirstEvalChk(PrRes, RUptNew).
					  
%% End of File

