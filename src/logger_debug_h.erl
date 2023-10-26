-module(logger_debug_h).

%% API
-export([start/1]).

%% logger callbacks
-export([
    log/2,
    adding_handler/1,
    removing_handler/1,
    changing_config/3,
    filter_config/1
]).
-ignore_xref([
    start/1,
    log/2,
    adding_handler/1,
    removing_handler/1,
    changing_config/3,
    filter_config/1
]).

%% Adds a new handler, waits for it to be added.
%% There is a bug in the logger, when add_handler/remove_handler
%% are not consistent. So, we do all the synchronisation here instead.
start(#{id := Id}) ->
    Pid = self(),
    Name = reg_cleaner_name(Id),
    case whereis(Name) of
        undefined ->
            ok;
        OldPid ->
            OldPid ! stop,
            wait_for_down(OldPid)
    end,
    {Cleaner, Mon} = spawn_monitor(fun() ->
        erlang:monitor(process, Pid),
        erlang:register(Name, self()),
        wait_for_handler_removed(Id),
        ok = logger:add_handler(
            Id,
            ?MODULE,
            #{config => #{forward_to_pid => Pid}}
        ),
        wait_for(fun() -> lists:member(Id, logger:get_handler_ids()) end, true),
        Pid ! {done_starting, self()},
        receive
            {'DOWN', _Ref, process, Pid, _Reason} ->
                ok;
            stop ->
                ok
        end,
        wait_for_handler_removed(Id)
    end),
    receive
        {'DOWN', Mon, process, Cleaner, Reason} ->
            error({failed_to_add_handler, Reason});
        {done_starting, Cleaner} ->
            ok
    end.

reg_cleaner_name(Id) when is_atom(Id) ->
    list_to_atom(atom_to_list(Id) ++ "_cleaner").

wait_for_down(Pid) ->
    Mon = erlang:monitor(process, Pid),
    receive
        {'DOWN', Mon, process, Pid, _Reason} ->
            ok
    after 5000 ->
        error({timeout, wait_for_down})
    end.

wait_for(F, Cond) ->
    wait_for(F, Cond, 50).

wait_for(_F, Cond, 0) ->
    error({timeout, wait_for, Cond});
wait_for(F, Cond, Retries) ->
    case F() of
        Cond ->
            ok;
        _ ->
            timer:sleep(100),
            wait_for(F, Cond, Retries - 1)
    end.

wait_for_handler_removed(Id) ->
    F = fun() ->
        _ = logger:remove_handler(Id),
        lists:member(Id, logger:get_handler_ids())
    end,
    extra_info(
        fun() -> wait_for(F, false) end,
        fun() -> {logger:remove_handler(Id), logger:get_handler_ids()} end
    ).

extra_info(Try, Extra) ->
    try
        Try()
    catch
        Class:Reason:Stacktrace ->
            erlang:raise(Class, {Reason, Extra()}, Stacktrace)
    end.

%%%===================================================================
%%% logger callbacks
%%%===================================================================

%%%-----------------------------------------------------------------
%%% Handler being added
-spec adding_handler(Config) -> {ok, Config} when
    Config :: logger:handler_config().

adding_handler(Config) ->
    {ok, Config}.

%%%-----------------------------------------------------------------
%%% Updating handler config
-spec changing_config(SetOrUpdate, OldConfig, NewConfig) ->
    {ok, Config}
when
    SetOrUpdate :: set | update,
    OldConfig :: logger:handler_config(),
    NewConfig :: logger:handler_config(),
    Config :: logger:handler_config().

changing_config(_SetOrUpdate, _OldConfig, NewConfig) ->
    {ok, NewConfig}.

%%%-----------------------------------------------------------------
%%% Handler being removed
-spec removing_handler(Config) -> ok when
    Config :: logger:handler_config().

removing_handler(_Config) ->
    ok.

%%%-----------------------------------------------------------------
%%% Log a string or report
-spec log(LogEvent, Config) -> ok when
    LogEvent :: logger:log_event(),
    Config :: logger:handler_config().

log(LogEvent, #{id := Id, config := #{forward_to_pid := Pid}}) ->
    Pid ! {log, Id, LogEvent},
    ok.

%%%-----------------------------------------------------------------
%%% Remove internal fields from configuration
-spec filter_config(Config) -> Config when
    Config :: logger:handler_config().

filter_config(Config) ->
    Config.
