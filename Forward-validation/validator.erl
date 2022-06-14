-module(validator).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init()->
    validator().

validator() ->
    receive
        {validate, Ref, Reads, Writes, Client} -> %%Write = pending write operations (store), Reads (List of reads performed), Client = ID process of client to reply
            Tag = make_ref(),
            send_write_checks(Writes, Tag, Client), 
            case check_writes(length(Writes), Tag) of
                ok ->
                    update(Writes),
					deactivate(Reads, Client),
                    Client ! {Ref, ok};
                abort ->
					deactivate(Reads, Client),
					Client ! {Ref, abort}
            end,
            validator();
        stop ->
            ok;
        _Old ->
            validator()
    end.
    
update(Writes) ->
    lists:foreach(fun({_, Entry, Value}) -> 
				  Entry ! {write, Value}
                  end, 
                  Writes).
				  
send_write_checks(Writes, Tag, Client) ->
	Self = self(),
	lists:foreach(fun({_, Entry, _}) -> 
				Entry ! {check, Tag, Self, Client}
				end,
				Writes).
				
check_writes(0, _) ->
    ok;
check_writes(N, Tag) ->
    receive
        {Tag, ok} ->
            check_writes(N-1, Tag);
        {Tag, abort} ->
            abort
    end.
	
deactivate(Reads, Client) ->
	Self = self(),
    lists:foreach(fun({Entry}) -> 
				  Entry ! {deactivate, Self, Client}
                  end, 
                  Reads),
	check_deactivate(length(Reads)).

check_deactivate(N) ->
	if
		N == 0 ->
			ok;
		true ->
			receive
				ok ->
					check_deactivate(N-1)
			end
	end.