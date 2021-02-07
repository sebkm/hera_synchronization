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
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).


subscribe(Name) ->
    % maybe wait is undef
    gen_server:call({global, ?MODULE}, {subscribe, Name}).

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
    NewState = #state{names=N#{Name=>Pid}, refs=R#{Ref=>Name}},
    {reply, {ok, Pid}, NewState}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'DOWN', Ref, _, _, _}, #state{names=N, refs=R}) ->
    {Name, R2} = maps:take(Ref, R),
    State = #state{names=maps:remove(Name, N), refs=R2},
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.