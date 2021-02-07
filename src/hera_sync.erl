-module(hera_sync).

-export([start_monitor/1, subscribe/2]).

-record(state, {
    p_ref :: reference(),
    queue :: queue:queue({pid(), reference()})
}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_monitor(MeasurePid) ->
    ParentPid = self(),
    {ok, spawn_monitor(fun() -> init(ParentPid, MeasurePid) end)}.


subscribe(SyncPid, MeasurePid) ->
    SyncPid ! {subscribe, MeasurePid}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(ParentPid, MeasurePid) ->
    Ref = monitor(process, ParentPid),
    Q = sub(MeasurePid, queue:new()),
    loop(#state{p_ref=Ref, queue=Q}).


loop(#state{p_ref=PRef, queue=Q}) ->
    receive
        {subscribe, NewMeasure} ->
            Q2 = sub(NewMeasure, Q),
            loop(#state{p_ref=PRef, queue=Q2})
    after 0 ->
        case queue:is_empty(Q) of
        true ->
            io:format("empty~n"),
            {stop, normal};
        false ->
            {{value, {Pid, Ref}}, Q2} = queue:out(Q),
            Pid ! {authorized, self()},
            receive
                {ok, Pid} ->
                    io:format("done~n"),
                    Q3 = queue:in({Pid, Ref}, Q2),
                    loop(#state{p_ref=PRef, queue=Q3});
                {'DOWN', Ref, _, _, _} ->
                   loop(#state{p_ref=PRef, queue=Q2});
                {'DOWN', PRef, _, _, _} ->
                    {stop, normal}
            end
        end
    end.


sub(MeasurePid, Queue) ->
    Ref = monitor(process, MeasurePid),
    Item = {MeasurePid, Ref},
    queue:in(Item, Queue).


test() ->
    {ok, Pid} = hera_sub:subscribe(test),
    Ref = monitor(process, Pid),
    receive
        {authorized, Pid} ->
            io:format("Measuring....~n"),
            timer:sleep(5000),
            Pid ! {ok, self()};
        {'DOWN', Ref, _, _, _} ->
            io:format("Sync down~n")
    end.
