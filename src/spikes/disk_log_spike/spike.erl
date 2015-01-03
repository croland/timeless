-module(spike).
-export([process_event/0]).

-include_lib("eunit/include/eunit.hrl").

-record(subscriber, {pid, event}).
-record(event, {type, body}).
-record(tick, {date, open, high, low, close, vol, adjclose}).


% Log
open() -> {ok, Log} = disk_log:open([{name, os:cmd("mktemp")}]), Log.
get_all(Log) -> get_all_terms(Log, start, []).
read_fail(R) -> exit({?MODULE, get(line), R}).
get_all_terms(Log, Cont, Res) ->
  case disk_log:chunk(Log, Cont) of
    {error, _R} -> read_fail({bad_chunk, Log, Cont});
    {Cont2, Terms} -> get_all_terms(Log, Cont2, Res ++ Terms);
    eof -> Res
  end.


% File processing for Yahoo historical data file
% This is already part of a spike in ferl 
parse_line_to_tick(Line) ->
	[Date, Open, High, Low, Close, Volume, AdjClose] = re:split(Line, "[,]", [{return, list}]),
  Tick = #tick{
    date=iolist_to_binary(Date), 
    open=list_to_float(Open), 
    high=list_to_float(High), 
    low=list_to_float(Low), 
    close=list_to_float(Close), 
    vol=list_to_integer(Volume), 
    adjclose=list_to_float(AdjClose)
  },
  Tick.

open_and_parse_file(Filename) ->
  {ok, Binary} = file:read_file(Filename),
  Lines = string:tokens(erlang:binary_to_list(Binary), "\r\n"),
	lists:map(fun(L) -> parse_line_to_tick(L) end, Lines).

log_tick_events(Log, Ticks) -> lists:foreach(fun(Tick) -> disk_log:log(Log, tick_to_event(Tick)) end, Ticks), ok.
tick_to_event(Tick) -> #event{type=tick_event, body=Tick}.
ticks_to_events(Ticks) -> lists:map(fun(Tick) -> tick_to_event(Tick) end, Ticks).

% Routing
% Will need to move this to a router module to manage subscriptions and routing messages
route_event_to_subscribers(Subscribers, Event) ->
  [S#subscriber.pid ! Event || S <- Subscribers, Event#event.type =:= S#subscriber.event], 
  ok.

route_events_to_subscribers(Subscribers, Events) ->
  lists:foreach(fun(Event) -> route_event_to_subscribers(Subscribers, Event) end, Events), 
  ok.


% Test
process_event() ->
  receive
    {event, Event} ->
      Event;
    _ ->
      ok
  end.

% create a process that an event is routed to
spawn_test_process() -> spawn(spike, process_event, []).

route_events_after_being_parsed_from_file_test() ->
  Ticks = open_and_parse_file("./goog-daily.csv"),
  Events = ticks_to_events(Ticks), 
  Subscriber = spawn_test_process(),
  Subscribers = [#subscriber{pid=Subscriber, event=tick_event}],
  route_events_to_subscribers(Subscribers, Events),
  ok.

register_a_router_to_route_events_when_pulled_from_log() ->
  ok.

parse_and_load_daily_prices_into_log_test() ->
  Log = open(),
  Ticks = open_and_parse_file("./goog-daily.csv"),
  log_tick_events(Log, Ticks),
  Events = get_all(Log), 
  Event = lists:last(Events), 
  Tick = Event#event.body,
  ?assert(Event#event.type =:= tick_event),
  ?assert(Tick#tick.adjclose =:= 558.46),
  disk_log:close(Log).

open_and_parse_daily_prices_test() ->
  Ticks = open_and_parse_file("./goog-daily.csv"),
  Tick = lists:last(Ticks), 
  ?assert(Tick#tick.adjclose =:= 558.46).

parse_daily_line_test() ->
  Line = "2015-01-02,529.01,531.27,524.10,524.81,1443600,524.81",
  Tick = parse_line_to_tick(Line),
  ?assert(Tick#tick.open =:= 529.01),
  ?assert(Tick#tick.adjclose =:= 524.81).

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
  DailyPriceEvent = parse_line_to_tick(Line),
  Log = open(),
  disk_log:log(Log, DailyPriceEvent), 
  Events = get_all(Log), 
  Event = lists:last(Events), 
  Open = Event#tick.open,
  ?assert(Open =:= 27.31),
  disk_log:close(Log).

register_a_datapipeline_router_to_listen_for_events() ->
  ok.

subscribe_a_datapipeline_process_to_a_router() ->
  ok.

create_a_datapipeline_and_resolve_event_into_aggregate_from_log() ->
  ok.

