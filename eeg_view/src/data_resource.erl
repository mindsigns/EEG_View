%% ------------------------------------------------------------------
%% @author Jon Trembley <jon@deathray.tv>
%% @doc    WebMachine resource to produce data for charting.
%% ------------------------------------------------------------------
-module(data_resource).
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
    N = wrq:disp_path(ReqData),
    Id = utils:str_to_int(N),
    Qentries = db_queries:sessions(Id),
    [{_Id, Starttime, Stoptime, _Name, _Desc}] = Qentries,

    Res = db_queries:export(Starttime, Stoptime),

    Pid = counter:start(),

    Header = "Tick,Meditation, Attention, Delta, Theta, HighAlpha, LowAlpha, HighBeta, LowBeta, MidGamma, LowGamma\n",

    Data = lists:map(
                    fun(X) ->
                        utils:format_chart(X,Pid)
                 end, Res),
    counter:stop(Pid),
    {ok, Content} = data_dtl:render([{header, Header}, {data, Data}]),
    {Content, ReqData, Context}.
