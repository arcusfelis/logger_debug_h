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

start(#{id := Id}) ->
    Pid = self(),
    CleanerPid = spawn(fun() ->
        erlang:monitor(process, Pid),
        receive
            {'DOWN', _Ref, process, Pid, _Reason} ->
                logger:remove_handler(Id)
        end
    end),
    erlang:register(reg_cleaner_name(Id), CleanerPid),
    logger:add_handler(
        Id,
        ?MODULE,
        #{config => #{forward_to_pid => Pid}}
    ).

reg_cleaner_name(Id) when is_atom(Id) ->
    list_to_atom(atom_to_list(Id) ++ "_cleaner").

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
