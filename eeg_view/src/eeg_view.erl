%% ------------------------------------------------------------------
%% @author Jon Trembley <jon@deathray.tv>
%% @doc    eeg_view startup code
%% ------------------------------------------------------------------
-module(eeg_view).
-author('jon <jon@deathray.tv>').

%% ------------------------------------------------------------------
%% API 
%% ------------------------------------------------------------------
-export([start/0, start_link/0, stop/0]).


ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    LogHandlers = [{webmachine_access_log_handler, ["priv/log"]},
        {webmachine_error_log_handler, ["priv/log"]}],
    application:set_env(webmachine, log_handlers, LogHandlers),
    webmachine_util:ensure_all_started(webmachine),                                                                                                                       
    eeg_view_sup:start_link().

%% @spec start() -> ok
%% @doc Start the eeg_view server.
start() ->
    LogHandlers = [{webmachine_access_log_handler, ["priv/log"]},
        {webmachine_error_log_handler, ["priv/log"]}],
    application:set_env(webmachine, log_handlers, LogHandlers),
    webmachine_util:ensure_all_started(webmachine),
    application:start(eeg_view).

%% @spec stop() -> ok
%% @doc Stop the eeg_view server.
stop() ->
    Res = application:stop(eeg_view),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    Res.
