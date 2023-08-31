# Debug erlang Logger in your tests

Example of usage:

```erlang
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
```
