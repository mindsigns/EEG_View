%% ------------------------------------------------------------------
%% @author Jon Trembley <jon@deathray.tv>
%% @doc    Webmachine resource for displaying the EEG data.
%% ------------------------------------------------------------------
-module(show_resource).
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


%% @doc Render the charts with existing datapoints
to_html(ReqData, State) ->
    N = wrq:disp_path(ReqData),
    Id = utils:str_to_int(N),

    Qentries = db_queries:sessions(Id),
    [{Id, Starttime, Stoptime, Desc, Name }] = Qentries,
    StartTime = utils:format_date(Starttime),
    StopTime  = utils:format_date(Stoptime),
    TimeStamp = StartTime ++ " - " ++ StopTime,
    Stats =  utils:format_stats(db_queries:stats(Id)),
    {ok, Content} = showchart_dtl:render([{name, Name}, {stats, Stats}, {id, Id}, {notes, Desc}, {timestamp, TimeStamp}]),
    {Content, ReqData, State}.
