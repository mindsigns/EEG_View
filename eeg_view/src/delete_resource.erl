%% ------------------------------------------------------------------
%% @author Jon Trembley <jon@deathray.tv>
%% @doc    Webmachine resource to delete sessions
%% ------------------------------------------------------------------
-module(delete_resource).
-author('jon <jon@deathray.tv>').

%% ------------------------------------------------------------------
%% API 
%% ------------------------------------------------------------------
-export([init/1, to_html/2, allowed_methods/2, process_post/2]).

%% ------------------------------------------------------------------
%% Header includes
%% ------------------------------------------------------------------
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['GET', 'HEAD', 'POST'], ReqData, State}.

process_post(ReqData, State) ->
    N = wrq:disp_path(ReqData),
    Id = utils:str_to_int(N),

    db_queries:delete(Id),

    {{halt, 303},
     wrq:set_resp_header("Location", "/", ReqData),
     State}.

to_html(ReqData, State) -> 
    N = wrq:disp_path(ReqData),
    Id = utils:str_to_int(N),

    Qentries = db_queries:sessions(Id),
    Session = lists:map(fun utils:format_session/1, Qentries),

    {ok, Content} = delete_dtl:render([{session, Session}]),
    {Content, ReqData, State}.
