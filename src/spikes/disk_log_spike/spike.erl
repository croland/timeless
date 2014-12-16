-module(spike).
-export([open/0, get_all/1]).

-include_lib("eunit/include/eunit.hrl").

run() ->
	Log = open().

read() ->
	ok.

read_log(Chunk, Log) ->
	ok.

open() -> disk_log:open([{name, os:cmd("mktemp")}]).
  
open_test() -> 
  {ok, Log} = open(),
  [Mode | _] = [M || {mode, M} <- disk_log:info(Log)], 
	?assert(Mode =:= read_write),
  disk_log:close(Log).

read_test() ->
  Log = open(),
  disk_log:close(Log).
  

get_all(Log) ->
  get_all_terms(Log, start, []).

get_all_terms(Log, Cont, Res) ->
  case disk_log:chunk(Log, Cont) of
    {error, _R} -> read_fail({bad_chunk, Log, Cont});
    {Cont2, Terms} -> get_all_terms(Log, Cont2, Res ++ Terms);
    eof -> Res
  end.

read_fail(R) ->
  exit({?MODULE, get(line), R}).
