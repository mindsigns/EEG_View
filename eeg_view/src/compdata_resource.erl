%% ------------------------------------------------------------------
%% @author Jon Trembley <jon@deathray.tv>
%% @doc    Webmachine resource for sending comparitive data for bar graphs
%% ------------------------------------------------------------------
-module(compdata_resource).
-author('jon <jon@deathray.tv>').

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-export([init/1, to_html/2]).

%% ------------------------------------------------------------------
%% Header includes
%% ------------------------------------------------------------------
-include_lib("webmachine/include/webmachine.hrl").

%% ------------------------------------------------------------------
init([]) -> {ok, undefined}.

to_html(ReqData, Context) ->

    Sess = db_queries:compare(),

    Pid = counter:start(),

    Header = "Session ,MeditationAvg, AttentionAvg, Delta Avg, Theta Avg, H.Alpha Avg, L.Alpha Avg, H.Beta Avg, L.Beta Avg\n",

    Data = lists:map(
                    fun(X) ->
                        utils:format_compchart(X,Pid)
                 end, Sess),

    counter:stop(Pid),

    {ok, Content} = data_dtl:render([{header, Header}, {data, Data}]),
    {Content, ReqData, Context}.
