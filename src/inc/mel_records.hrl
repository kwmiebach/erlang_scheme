%%------------------------------------------------
%% Berlin Brown
%% Copyright Botnode.com(Berlin Brown) 2008
%% File: mel_records.hrl
%% Date: 7/15/2008
%%
%%------------------------------------------------

%% Where status = init|ok|done
-record(reader_info, {status=init, last_ch=-1,
                      reader_strm="", token_buffer=[], 
					  char_stack=[], token_stack=[]}).

-record(proc_info, {name="Anon.Proc", min_args=0,
					max_args=9999, id=-1, apply_proc}).

%% End of File.
