%% ------------------------------------------------------------------
%% @author Jon Trembley <jon@deathray.tv>
%% @doc    Webmachine resource to edit sessions
%% ------------------------------------------------------------------
-module(edit_resource).
-author('jon <jon@deathray.tv>').

%% ------------------------------------------------------------------
%% API Function Exports
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
    [{"name", Name}, {"starttime", Starttime},{"stoptime", Stoptime}, {"notes", Notes}, {"session_id", Session_Id}] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    db_queries:update_session(Session_Id, Starttime, Stoptime, Notes, Name),

    {{halt, 303},
     wrq:set_resp_header("Location", "/", ReqData),
     State}.


to_html(ReqData, State) -> 
    N = wrq:disp_path(ReqData),
    Id = utils:str_to_int(N),

    Qentries = db_queries:sessions(Id),
    Entries = lists:map(fun utils:format_session/1, Qentries),

    {ok, Content} = edit_dtl:render([{entries, Entries}]),
    {Content, ReqData, State}.

    
