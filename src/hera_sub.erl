-module(hera_sub).

-behaviour(gen_server).

-export([start_link/0, subscribe/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    names = #{} :: #{atom() => pid()},
    refs = #{} :: #{reference() => atom()}
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    global:re_register_name(?MODULE, Pid),
    {ok, Pid}.


%% subscribe the caller to the hera_sync in charge of
%% measurments identified by Name
-spec subscribe(Name) -> {ok, pid()} when
    Name :: atom().

subscribe(Name) ->
    case global:whereis_name(?MODULE) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, {subscribe, Name});
        undefined ->
            timer:sleep(1000),
            subscribe(Name)
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
    {ok, #state{}}.


handle_call({subscribe, Name}, {SubPid, _}, State=#state{names=N}) when
    is_map_key(Name, N) ->
    Pid = maps:get(Name, N),
    hera_sync:subscribe(Pid, SubPid),
    {reply, {ok, Pid}, State};

handle_call({subscribe, Name}, {SubPid, _}, State) ->
    #state{names=N, refs=R} = State,
    {ok, {Pid, Ref}} = hera_sync:start_monitor(SubPid),
    NewState = State#state{names=N#{Name=>Pid}, refs=R#{Ref=>Name}},
    {reply, {ok, Pid}, NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({'DOWN', Ref, _, _, _}, State=#state{names=N, refs=R}) ->
    {Name, R2} = maps:take(Ref, R),
    NewState = State#state{names=maps:remove(Name, N), refs=R2},
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.