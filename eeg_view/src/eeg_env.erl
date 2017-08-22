%% ------------------------------------------------------------------
%% @author Jon Trembley <jon@deathray.tv>
%% @doc    Provide App wide configuration
%% ------------------------------------------------------------------
-module(eeg_env).
-author('jon <jon@deathray.tv>').

%% ------------------------------------------------------------------
%% API 
%% ------------------------------------------------------------------
-export([get_env/1, get_env/2, set_env/2]).

-define(APPLICATION, eeg_view).

%% ------------------------------------------------------------------
%% Functions
%% ------------------------------------------------------------------
get_env(Key) ->
    {ok, Value} = application:get_env(?APPLICATION, Key),
    Value.

get_env(Key, Default) ->
    case application:get_env(?APPLICATION, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

set_env(Key, Value) ->
    application:set_env(?APPLICATION, Key, Value).
