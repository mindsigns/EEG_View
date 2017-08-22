%% ------------------------------------------------------------------
%% @author Jon Trembley <jon@deathray.tv>
%% @doc    Webmachine resource to export sessions
%% ------------------------------------------------------------------
-module(export_resource).
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
    [{"filename", FileName}, {"session_id", Id}] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),

  csv:write_file(FileName, Id),

  {{halt, 303},
    wrq:set_resp_header("Location", "/", ReqData),
  State}.

to_html(ReqData, State) -> 
    N = wrq:disp_path(ReqData),
    Id = utils:str_to_int(N),

    Qentries = db_queries:sessions(Id),
    Entries = lists:map(fun utils:format_session/1, Qentries),

    {ok, Content} = export_dtl:render([{entries, Entries}]),
    {Content, ReqData, State}.

    
