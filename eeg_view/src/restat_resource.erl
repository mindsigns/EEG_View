%% ------------------------------------------------------------------
%% @author Jon Trembley <jon@deathray.tv>
%% @doc    Webmachine resource to create session stats
%% ------------------------------------------------------------------
-module(restat_resource).
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
    {['GET', 'HEAD', 'POST'], ReqData, State}.

to_html(ReqData, State) -> 
    N = wrq:disp_path(ReqData),
    Id = utils:str_to_int(N),

    utils:make_stats(Id),
 
    {{halt, 303},
     wrq:set_resp_header("Location", "/", ReqData),
     State}.
