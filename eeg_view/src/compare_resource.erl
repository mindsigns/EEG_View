%% ------------------------------------------------------------------
%% @author Jon Trembley <jon@deathray.tv>
%% @doc    Webmachine resource for comparing sessions
%% ------------------------------------------------------------------
-module(compare_resource).
-author('jon <jon@deathray.tv>').

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-export([init/1, to_html/2, allowed_methods/2]).

%% ------------------------------------------------------------------
%% Header includes
%% ------------------------------------------------------------------
-include_lib("webmachine/include/webmachine.hrl").

%% ------------------------------------------------------------------
init([]) -> {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['GET', 'HEAD'], ReqData, State}.

to_html(ReqData, State) -> 

    Sess = db_queries:compare(),
    Sessions = lists:map(fun utils:format_compare/1, Sess),

    {ok, Content} = compare_dtl:render([{sessions, Sessions}]),
    {Content, ReqData, State}.
