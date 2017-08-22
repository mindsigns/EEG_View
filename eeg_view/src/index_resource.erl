%% ------------------------------------------------------------------
%% @author Jon Trembley <jon@deathray.tv>
%% @doc    Webmachine resource for Index page.
%% ------------------------------------------------------------------
-module(index_resource).
-author('jon <jon@deathray.tv>').

%% ------------------------------------------------------------------
%% API 
%% ------------------------------------------------------------------
-export([init/1, to_html/2]).

%% ------------------------------------------------------------------
%% Header includes
%% ------------------------------------------------------------------
-include_lib("webmachine/include/webmachine.hrl").


init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    Res = lists:reverse( db_queries:sessions()),
    Sessions = lists:map(fun utils:format_session/1, Res),
    {ok, Content} = session_dtl:render([{sessions, Sessions}]),
    {Content, ReqData, State}. 
