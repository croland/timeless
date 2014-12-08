-module(spike).

-include_lib("eunit/include/eunit.hrl").

run() ->
	Log = open().

read() ->
	ok.

read_log(Chunk, Log) ->
	ok.

open() -> 
	{ok, Log} = disk_log:open([{name, os:cmd("mktemp")}]),
	Log.

open_test_() -> 
	Log = open(),
	?assert().
