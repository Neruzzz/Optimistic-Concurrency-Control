-module(opty).
-export([start/6, stop/2, startDistributed/6, startServer/1]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

startDistributed(Clients, Entries, Reads, Writes, Time, Server) ->
	spawn(Server, opty, startServer, [Entries]),
	timer:sleep(1000),
	io:format("Starting clients ~n"),
	start(Clients, Entries, Reads, Writes, Time, Server).

start(Clients, Entries, Reads, Writes, Time, Server) ->
    L = startClients(Clients, [], Entries, Reads, Writes, Server),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n", 
         [Clients, Entries, Reads, Writes, Time]),
    timer:sleep(Time*1000),
    stop(L, Server).

startServer(Entries)->
	register(s, server:start(Entries)).

stop(L, Server) ->
    io:format("Stopping...~n"),
    stopClients(L),
	waitClients(L),
    {s, Server} ! stop,
    io:format("Stopped~n").

startClients(0, L, _, _, _, _) -> L;
startClients(Clients, L, Entries, Reads, Writes, Server) ->
    Pid = client:start(Clients, Entries, Reads, Writes, {s, Server}),
    startClients(Clients-1, [Pid|L], Entries, Reads, Writes, Server).

stopClients([]) ->
    ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},
    stopClients(L).

waitClients([]) ->
    ok;
waitClients(L) ->
    receive
        {done, Pid} ->
            waitClients(lists:delete(Pid, L))
    end.
