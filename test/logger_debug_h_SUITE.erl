-module(logger_debug_h_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").

-compile([export_all, nowarn_export_all]).

all() ->
    [
        {group, main}
    ].

groups() ->
    [
        {main, [], cases()}
    ].

cases() ->
    [
        test_log_forwarded,
        adding_handler_twice
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

test_log_forwarded(_Config) ->
    logger_debug_h:start(#{id => test_log_forwarded}),
    ?LOG_ERROR(#{what => testtest}),
    Me = self(),
    receive
        {log, test_log_forwarded, Log} ->
            #{
                level := error,
                meta :=
                    #{
                        mfa := {logger_debug_h_SUITE, test_log_forwarded, 1},
                        pid := Me
                    },
                msg := {report, #{what := testtest}}
            } = Log
    end.

adding_handler_twice(_Config) ->
    ok = logger_debug_h:start(#{id => ?FUNCTION_NAME}),
    ok = logger_debug_h:start(#{id => ?FUNCTION_NAME}),
    ok.
