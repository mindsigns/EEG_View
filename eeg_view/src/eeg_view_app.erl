%% ------------------------------------------------------------------
%% @author Jon Trembley <jon@deathray.tv>
%% @doc    Callbacks for the eeg_view application.
%% ------------------------------------------------------------------
-module(eeg_view_app).
-author('jon <jon@deathray.tv>').

%% ------------------------------------------------------------------
%% API 
%% ------------------------------------------------------------------
-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for eeg_view.
start(_Type, _StartArgs) ->
    eeg_view_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for eeg_view.
stop(_State) ->
    ok.
