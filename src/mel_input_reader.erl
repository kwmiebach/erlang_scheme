%%------------------------------------------------
%% Berlin Brown
%% Copyright Botnode.com(Berlin Brown) 2008
%% File: mel_input_reader.erl
%%
%% Overview: Mel, "My Embedded Language" is a simple scheme implementation
%%           in Erlang, based on JScheme.
%% Purpose: The "My Embedded Language" Input Reader is a stream
%% reader for scheme.  This module contains code for reading scheme code
%% character by character.
%% (Note: in our case, a stream is a list/string.  We extract one
%% character at a time from the list until the end)
%%
%% Date: 7/15/2008
%%
%% References:
%% http://www.erlang.se/doc/programming_rules.shtml
%% http://www.erlang.org/doc/reference_manual/part_frame.html
%% http://www.erlang.org/doc/man/lists.html
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
-module(mel_input_reader).
-include("mel_records.hrl").
-export([is_whitespace/1, next_token/1]).
-export([new_reader/1, stream_read/1, while_whitespace/1]).
-export([new_stack/0, push_stack/2, pop_stack/1, peek_stack/1]).
-export([read/1]).

-import(mel_scheme_util, [cons/2, sleep/1]).

%%----------------------------------------------------------------------
%% Function: read/1
%% Purpose: Get various information from a process.
%% Args:    Reader (reader_info)
%% Returns: The potential Pair object and the updated Record
%%          { Pair Data, Updated Record }
%%----------------------------------------------------------------------
read(Reader) ->
    { Tok, R } = next_token(Reader),
	if 
		Tok == "(" -> read_tail(R);
		Tok == ")" ->
            io:format("WARN: Extra ')' ignored."),
            read(R);
		true -> { Tok, R }
	end.

%%----------------------------------------------------------------------
%% Function: read_tail/1
%% Args:    Reader (reader_info)
%% Returns: The potential Pair object and the updated Record
%%          { Pair Data, Updated Reader Record }
%%----------------------------------------------------------------------
read_tail(#reader_info{token_stack=TokStk}=Reader) ->
    { Tok, R } = next_token(Reader),
    if 
        Tok == "#!EOF" ->
            io:format("ERROR: readTail() - EOF during read.~n"),
            { [], R };
        Tok == ")" ->
            { [], R };
        true ->
            T2Stk = push_stack(TokStk, Tok),
            R2x = R#reader_info{token_stack=T2Stk},
                                                % R[2]'x' is the current Reader info
                                                % Build a pair out of the token from read
                                                % and read_tail.
            { PairRead, R3x} = read(R2x),
            { PairTail, R4x } = read_tail(R3x),
            { cons(PairRead, PairTail), R4x }
    end.

%%----------------------------------------------------------------------
%% Function: convert_tok_float/2
%% Purpose: Crude approach for converting token to float.  If 
%%          the LAST char is numeric, attempt to convert float.
%% Args:    Reader (reader_info)
%% Returns: 
%%     { ok,    Value } (Success Status Return and the Value)
%%     { error, Original Token Buffer } (Fail Status Return and empty Value, ignore)
%%----------------------------------------------------------------------	 
convert_tok_float(Reader, Ch) ->
    Buf = Reader#reader_info.token_buffer,
    case ((Ch == $.) or (Ch == $+) or (Ch == $-) or ((Ch >= $0) and (Ch =< $9))) of
        false ->
            { error, Buf };
        true ->
            try
                                                % Attempt to convert from list to float
                                                % and then list to integer
                try
                    { ok, list_to_float(Buf) }
                catch error:_ ->
                                                % Bad hack for ensuring float
                        Fake = Buf ++ ".0",
                        { ok, list_to_float(Fake) }
                end
            catch error:_ ->
                                                % Could not convert token buffer, 
                                                % return original Reader
                    io:format("trace: ERR convert =>~p~n", [Buf]),
                    { error, Buf }
            end
    end.

%%----------------------------------------------------------------------
%% Function: next_token_build/1
%% Purpose: Return the Token Atom
%% Args:    Reader (reader_info)
%% Returns: 
%%     {Top Token, Record Info with updated buffer}
%%----------------------------------------------------------------------	 
next_token_build(Reader) ->
	ChStk = Reader#reader_info.char_stack,
                                                % Task 1: Pop the token 
                                                % and character stacks
	{ Ch, RUpt } = 
		case (not is_empty(ChStk)) of
			true ->
                                                % Pop char off char stack
				{ _, Top, Rest } = pop_stack(ChStk),	
                                                % Return top of stack, 
                                                % and current reader,
                                                % with the updated stack
				{ Top, Reader#reader_info{char_stack=Rest} };
            false ->
                                                % The state of the reader has changed, 
                                                % mutated char reader stream.
                { Rx } = stream_read(Reader),
                { peek_char_reader(Rx), Rx }
		end,
                                                % Task 2: Check for and 
                                                % ignore whitespace
	{ Ch2, R2Upt } = case is_whitespace(Ch) of
                         true -> 
                             { R2x } = while_whitespace(RUpt),
                             { peek_char_reader(R2x), R2x };
                         false -> { Ch, RUpt }
                     end,
                                                % Build exit points
	if
        Ch2 == -1   -> { "#!EOF", R2Upt };
        Ch2 == $(  -> { "(",     R2Upt };
        Ch2 == $)  -> { ")",     R2Upt };
        true ->
                                                % Reset the atom buffer
                                                % Set the buffer for the NEW reader
            BufRst = new_stack(),
            RResetBufUpt = R2Upt#reader_info{token_buffer=BufRst},
                                                % Buffer is updated by build token
            { R3Uptx } = build_generic_token(RResetBufUpt, Ch2),
            case convert_tok_float(R3Uptx, Ch2) of
                { ok, FloatVal } -> 
                                                % Return <float, Reader>
                    { FloatVal, R3Uptx };
                { _, _ } ->
                                                % Error case for convert attempt, 
                                                % act like normal
                                                % Return <string, Reader>
                    { peek_buf_reader(R3Uptx), R3Uptx }
            end
    end.

%%----------------------------------------------------------------------
%% Function: next_token/1
%% Purpose: Return the Token Atom
%% Args:    Reader (reader_info)
%% Returns: Next token along with the updated Record object
%%     {Top Token, Record Info with updated buffer}
%%----------------------------------------------------------------------
next_token(Reader) ->
	TokStack = Reader#reader_info.token_stack,
	case is_empty(TokStack) of
        true -> next_token_build(Reader);
        false ->
            { ok, TopTok, Rest } = pop_stack(TokStack),
                                                % Update Record info token stack            
            { TopTok, Reader#reader_info{token_stack=Rest} }
    end.

%%----------------------------------------------------------------------
%% Function: build_generic_token/2
%% Purpose: Build an atom string based on the token buffer stack.  
%%          In Java:
%% <code>
%% 	   do {
%%			buff.append((char) ch);
%%			ch = inputReader.read();
%%		} while (!Character.isWhitespace((char) ch) ...
%% </code>
%% Args:    Reader (reader_info)
%% Returns: Build Atom/Symbol token; mutate the character and buffer stack and
%%          update the Reader data and return.
%%----------------------------------------------------------------------    
build_generic_token(#reader_info{token_buffer=Buf,char_stack=ChStack}=Reader, Ch) ->
                                                % Mutate buffer stack
    BufUpt = push_stack(Buf, Ch),
    { RUpt } = stream_read(Reader), ChUpt = peek_char_reader(RUpt),
    case not_lang_token(ChUpt) of
        true ->
            R3 = RUpt#reader_info{last_ch=ChUpt, 
                                  token_buffer=BufUpt},
            build_generic_token(R3, ChUpt);
        false ->
                                                % Note: Push a 'language' token 
                                                % onto the character stack
            ChStkUpt = push_stack(ChStack, ChUpt),
            { RUpt#reader_info{last_ch=ChUpt, char_stack=ChStkUpt,
                               token_buffer=lists:reverse(BufUpt)} }
    end.

%%----------------------------------------------------------------------    
%% Simple utility function to check if the character
%% is white space or a language token. Used with
%% build generic token.
%%----------------------------------------------------------------------    
not_lang_token(Ch) ->
    ((not is_whitespace(Ch))
     and (not ( Ch == -1 ))   
     and (not ( Ch == $( )) and (not ( Ch == $) ))).

%% Utility function, return the last char
peek_char_reader(Reader) ->
    Reader#reader_info.last_ch.
peek_buf_reader(Reader) ->                                                
    Reader#reader_info.token_buffer.
  
%%----------------------------------------------------------------------
%% Stack Operations
%% Function: push_stack/2, pop_stack/1, peek_stack/1
%%
%% Purpose: Push and pop values onto a list based stack.
%% Returns: A list of {Key, Value} 
%%     or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
new_stack() -> [].

is_empty([]) -> true;	
is_empty(Stack) -> false.

push_stack([], Data) -> [Data];
push_stack(Stack, Data) -> [Data|Stack].

pop_stack([]) -> { empty, false, [] };
pop_stack([Head|Rest]) -> { ok, Head, Rest }.

peek_stack([]) -> { empty, false, [] };
peek_stack([Head|Rest]=Stack) -> { ok, Head, Stack }.

%%------------------------------------------------
%% Stream Operations
%%------------------------------------------------
new_reader(Data) ->
	Reader = #reader_info{
	  reader_strm = Data
	 },
	Reader.
%%----------------------------------------------------------------------
%% Function: stream_read/2
%% Purpose:  Read the next character/value in the stream.  In this case, 
%%           a stream is just a "list" of values and we pop the next
%%           value in the list until the list is empty.
%% Note:   Reader only returns a 'Reader Info' record
%% Args:   Reader is an instance of the record "Reader" record.
%% Returns: The status of the read, the Top/Head character
%%          updated in the Reader record.
%%          { Reader, status = ok } (Reader read successfully)
%%          { Reader, status = done } (Reader reached EOF)
%%----------------------------------------------------------------------
stream_read(Reader) ->
	Str = Reader#reader_info.reader_strm,
    { Done, H, T } = stream_read(Str, ok),
                                                % Where tail can be an empty list of []                      
                                                % Update the record info with; 
                                                % set the status, last char
	{ Reader#reader_info{status=Done, last_ch=H,
                         reader_strm = T} }.
stream_read([], _) -> { done, -1, [] };
stream_read(-1, _) -> { done, -1, [] };
stream_read(Str, _) ->
    [ Head | Tail ] = Str,
    { ok, Head, Tail }. 

%%----------------------------------------------------------------------
%% Function: while_ws/2
%% Purpose: Read the next character/value in the stream.  Skip
%%          over whitespace.  Once a valid character is found, exit
%%          and return that character.
%% Args:   Reader is an instance of the record "Reader" record.
%% Returns: The status of the read, the Top/Head character
%%           and the Reader record.
%%          { Reader } (Reader read successfully)
%%----------------------------------------------------------------------
while_ws(Reader) ->
    % Updated the state of the Reader with the next char
	{ Info } = stream_read(Reader),
    Ch = Info#reader_info.last_ch,
	case Info#reader_info.status of
        done -> 
                                                % End of File/Stream:
                                                % Set the Reader status = done
            { Info#reader_info{status=done, last_ch=-1} }; 
        ok -> 
            case is_whitespace(Ch) of
                true ->
                                                % Continue to the next char
                    while_ws(Info);
                false ->
                                                % Not whitespace so exit with this char
                                                % Return with Reader, set Ch
                    { Info#reader_info{status=done, last_ch=Ch} }
                end
    end.

%%----------------------------------------------------------------------
%% Function: while_whitespace/2
%% Purpose: Wrapper function for while_ws.  Return Reader with the
%%          updated last_ch character.
%% Returns: { Updated Reader }
%%----------------------------------------------------------------------
while_whitespace(Reader) ->
	{ R } = while_ws(Reader),
    Ch = R#reader_info.last_ch,
	case is_whitespace(Ch) of
		true -> { R#reader_info{status=done, last_ch=-1} };
		false -> { R#reader_info{status=done, last_ch=Ch} }
    end.

%%------------------------------------------------
%% Determines if the specified character is white space.
%% (not the best implementation)
%%------------------------------------------------
is_whitespace(C) ->
	if ((C == -1) or (C == 9)  or (C == 10) or (C == 11) or
        (C == 12) or (C == 13) or (C == 28) or
        (C == 29) or (C == 30) or (C == 31) or
        (C == 32)) -> true;
       true -> false
	end.

is_str_int(String) ->
	case (catch list_to_integer(String)) of
		{'EXIT', _} -> false;
		Integer -> true
	end.
  
%% End of File
