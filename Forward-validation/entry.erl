-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, []).

entry(Value, Active) ->
    receive
        {read, Ref, From, Client} ->
            From ! {Ref, self(), Value},
			case lists:member(Client, Active) of
				true ->
					entry(Value, Active);
				false ->
					entry(Value, [Client|Active])
			end;
        {write, New} ->
            entry(New , Active);
        {check, Ref, From, Client} ->
			NewList = lists:delete(Client, Active),
            if 
                 length(NewList) == 0 ->  
                    From ! {Ref, ok};
                true ->
                    From ! {Ref, abort}
            end,
            entry(Value, Active);
		{deactivate, From, Client} ->
			From ! ok,
			NewList = lists:delete(Client, Active),
			entry(Value, NewList);
			
        stop ->
            ok
    end.
