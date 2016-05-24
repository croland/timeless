#!/usr/bin/env escript


main(_) -> 
  {ok, Context}   = erlzmq:context(),
  {ok, Responder} = erlzmq:socket(Context, rep),
  ok = erlzmq:bind(Responder, "tcp://*:5555"),

  loop(Responder),

  erlzmq:close(Responder),
  erlzmq:exit(Context).

loop(Responder) ->
  {ok, Msg} = erlzmq:recv(Responder),
  io:format("Received ~s~n", [Msg]),

  timer:sleep(1000),

  erlzmq:send(Responder, "Meep"),
  loop(Responder).

