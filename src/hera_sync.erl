-module(hera_sync).

-export([start_monitor/1, subscribe/2]).

-record(state, {
    p_ref :: reference(),
    queue :: queue:queue({pid(), reference()})
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% starts and monitor a new hera_sync with MeasurePid as first subscriber
-spec start_monitor(MeasurePid) -> {ok, {pid(), reference()}} when
    MeasurePid :: pid().

start_monitor(MeasurePid) ->
    ParentPid = self(),
    {ok, spawn_monitor(fun() -> init(ParentPid, MeasurePid) end)}.


%% cast a subscribe message to the hera_sync identified by SyncPid
-spec subscribe(SyncPid, MeasurePid) -> ok when
    SyncPid :: pid(),
    MeasurePid :: pid().

subscribe(SyncPid, MeasurePid) ->
    SyncPid ! {subscribe, MeasurePid},
    ok.

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
            {stop, normal};
        false ->
            {{value, {Pid, Ref}}, Q2} = queue:out(Q),
            Pid ! {authorized, self()},
            receive
                {ok, Pid} ->
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
