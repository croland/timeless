-module(spike).
-record(tick, {date, open, high, low, close, vol, adjclose}).

-include_lib("eunit/include/eunit.hrl").

open() -> {ok, Log} = disk_log:open([{name, os:cmd("mktemp")}]), Log.

get_all(Log) -> get_all_terms(Log, start, []).
read_fail(R) -> exit({?MODULE, get(line), R}).
get_all_terms(Log, Cont, Res) ->
  case disk_log:chunk(Log, Cont) of
    {error, _R} -> read_fail({bad_chunk, Log, Cont});
    {Cont2, Terms} -> get_all_terms(Log, Cont2, Res ++ Terms);
    eof -> Res
  end.

parse_line(Line) ->
  [Date, Open, High, Low, Close, Volume, AdjClose] = re:split(Line, "[,]", [{return, list}]),
  Tick = #tick{date=iolist_to_binary(Date), open=list_to_float(Open), high=list_to_float(High), low=list_to_float(Low), close=list_to_float(Close), vol=list_to_integer(Volume), adjclose=list_to_float(AdjClose)}, 
  Tick.


% ----- Tests
open_a_new_log_test() -> 
  Log = open(),
  [Mode | _] = [M || {mode, M} <- disk_log:info(Log)], 
	?assert(Mode =:= read_write),
  disk_log:close(Log).

read_event_from_log_test() ->
  Log = open(),
  disk_log:log(Log, {some_event, "hello"}),
  Events = get_all(Log),
  Length = length(Events),
  ?assert(Length =:= 1),
  disk_log:close(Log).

parse_daily_price_event_from_input_and_log_event_test() ->
  Line = "2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
  DailyPriceEvent = parse_line(Line),
  Log = open(),
  disk_log:log(Log, DailyPriceEvent), 
  Events = get_all(Log), 
  Event = lists:last(Events), 
  Open = Event#tick.open,
  ?assert(Open =:= 27.31),
  disk_log:close(Log).

create_a_datapipeline_and_resolve_event_into_aggregate_from_log() ->
  ok.

