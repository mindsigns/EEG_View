%% ------------------------------------------------------------------
%% @author Jon Trembley <jon@deathray.tv>
%% @doc    Utility functions
%% ------------------------------------------------------------------
-module(utils).
-author('jon <jon@deathray.tv>').

%% ------------------------------------------------------------------
%% API 
%% ------------------------------------------------------------------
-export([
         format_session/1,
         format_chart/2,
         format_compchart/2,
         format_date/1,
         format_compare/1,
         str_to_int/1,
         stats/3,
         make_stats/1,
         format_stats/1
        ]).

%% ------------------------------------------------------------------
%% Header includes
%% ------------------------------------------------------------------
-include("eeg.hrl").

%% @doc Format a list of Sessions
format_session(Session) ->
    {Id, StartTime, StopTime, Notes, Name} = Session,
    [{"id", Id},
    {"starttime", StartTime},
    {"stoptime", StopTime},
    {"date", format_date(StartTime)},
    {"notes", Notes},
    {"name", Name}].

%% @doc Formats Entry for chart plotting
format_chart(Entry,Pid) ->
    {_Timestamp, Meditation, Attention, Delta, Theata, HighAlpha, LowAlpha, HighBeta, LowBeta, MidGamma, LowGamma} = Entry,
    counter:increment(Pid),
    LibFor = io_lib:format("~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p~n",[counter:value(Pid), Meditation, Attention, Delta, Theata, HighAlpha, LowAlpha, HighBeta, LowBeta, MidGamma, LowGamma]),
    LibFor.

%% @doc Formats Entry for compare chart
format_compchart(Entry,Pid) ->
    {_Id, MedAvg, AttAvg, DeltaAvg, ThetaAvg, HAlphaAvg, LAlphaAvg, HBetaAvg, LBetaAvg} = Entry,
    counter:increment(Pid),
    LibFor = io_lib:format("~p,~p,~p,~p,~p,~p,~p,~p,~p~n",[counter:value(Pid),MedAvg, AttAvg, DeltaAvg, ThetaAvg, HAlphaAvg, LAlphaAvg, HBetaAvg, LBetaAvg]),
    LibFor.


%% @doc Returns a human readable string from TimeStamp
format_date(TimeStamp) ->
  Year   = string:substr(erlang:integer_to_list(TimeStamp), 1, 4),
  Month  = string:substr(erlang:integer_to_list(TimeStamp), 5, 2),
  Day    = string:substr(erlang:integer_to_list(TimeStamp), 7, 2),
  Hour   = string:substr(erlang:integer_to_list(TimeStamp), 9, 2),
  Minute = string:substr(erlang:integer_to_list(TimeStamp), 11, 2),
  Second = string:substr(erlang:integer_to_list(TimeStamp), 13, 2),
  %{Month,Day,Year,Hour,Minute,Second}.
  Date = Month ++ "/" ++ Day ++ "/" ++ Year ++ " @ " ++ Hour ++ ":" ++ Minute ++ ":" ++ Second,
  Date.

% @doc Formats data for comparing session stats
format_compare(Session) ->
    {Id, MedAvg, AttAvg, DeltaAvg, ThetaAvg, HAlphAvg, LAlphAvg, HBetaAvg, LBetaAvg} = Session,
    [{Name}] = db_queries:get_sess_name(Id),
    [{"id", Id},
    {"name", Name},
    {"medavg", MedAvg},
    {"attavg", AttAvg},
    {"deltavg", DeltaAvg},
    {"thetavg", ThetaAvg},
    {"halavg", HAlphAvg},
    {"lalavg", LAlphAvg},
    {"hbetavg", HBetaAvg},
    {"lbetavg", LBetaAvg}].


%% @doc Convert a string to an integer
str_to_int(String) ->
    case string:to_integer(String) of
        {error, _} -> Integer = 1,
                        Integer;
        _ -> {Integer, []} = string:to_integer(String),
                Integer
    end.

%% @doc Returns the average of a session based on brainwave type
stats(Id, Wave, Type) ->
  [{_, Starttime, Stoptime,_,_}] = lists:flatten(db_queries:sessions(Id)),
  case Wave of
    "Meditation"  -> Val = [X || {_,X,_,_,_,_,_,_,_,_,_} <- db_queries:export(Starttime, Stoptime)];
    "Attention"   -> Val = [X || {_,_,X,_,_,_,_,_,_,_,_} <- db_queries:export(Starttime, Stoptime)];
    "Delta"       -> Val = [X || {_,_,_,X,_,_,_,_,_,_,_} <- db_queries:export(Starttime, Stoptime)];
    "Theta"       -> Val = [X || {_,_,_,_,X,_,_,_,_,_,_} <- db_queries:export(Starttime, Stoptime)];
    "HighAlpha"   -> Val = [X || {_,_,_,_,_,X,_,_,_,_,_} <- db_queries:export(Starttime, Stoptime)];
    "LowAlpha"    -> Val = [X || {_,_,_,_,_,_,X,_,_,_,_} <- db_queries:export(Starttime, Stoptime)];
    "HighBeta"    -> Val = [X || {_,_,_,_,_,_,_,X,_,_,_} <- db_queries:export(Starttime, Stoptime)];
    "LowBeta"     -> Val = [X || {_,_,_,_,_,_,_,_,X,_,_} <- db_queries:export(Starttime, Stoptime)];
    "MidGamma"    -> Val = [X || {_,_,_,_,_,_,_,_,_,X,_} <- db_queries:export(Starttime, Stoptime)];
    "LowGamma"    -> Val = [X || {_,_,_,_,_,_,_,_,_,_,X} <- db_queries:export(Starttime, Stoptime)]
  end,
  case Type of
    average -> average(Val);
    max     -> lists:max(Val);
    min     -> lists:min(Val);
    peaks   -> peaks(Val)
  end.

%% @doc Calculate stats for a session.
make_stats(Id) ->
      StatsRec  = #stats{
        id = Id,
        meditation_avg  = utils:stats(Id, "Meditation", average),
        attention_avg   = utils:stats(Id, "Attention", average),
        delta_avg       = utils:stats(Id, "Delta", average),
        theta_avg       = utils:stats(Id, "Theta", average),
        midgamma_avg   = utils:stats(Id, "MidGamma", average),
        highalpha_avg   = utils:stats(Id, "HighAlpha", average),
        lowgamma_avg    = utils:stats(Id, "LowGamma", average),
        lowalpha_avg    = utils:stats(Id, "LowAlpha", average),
        highbeta_avg    = utils:stats(Id, "HighBeta", average),
        lowbeta_avg      = utils:stats(Id, "LowBeta", average),
        meditation_max  = utils:stats(Id, "Meditation", max),
        attention_max   = utils:stats(Id, "Attention", max),
        delta_max       = utils:stats(Id, "Delta", max),
        theta_max       = utils:stats(Id, "Theta", max),
        midgamma_max   = utils:stats(Id, "MidGamma", max),
        highalpha_max   = utils:stats(Id, "HighAlpha", max),
        lowgamma_max    = utils:stats(Id, "LowGamma", max),
        lowalpha_max    = utils:stats(Id, "LowAlpha", max),
        highbeta_max    = utils:stats(Id, "HighBeta", max),
        lowbeta_max    = utils:stats(Id, "LowBeta", max),
        meditation_min  = utils:stats(Id, "Meditation", min),
        attention_min   = utils:stats(Id, "Attention", min),
        delta_min       = utils:stats(Id, "Delta", min),
        theta_min       = utils:stats(Id, "Theta", min),
        midgamma_min   = utils:stats(Id, "MidGamma", min),
        highalpha_min   = utils:stats(Id, "HighAlpha", min),
        lowgamma_min    = utils:stats(Id, "LowGamma", min),
        lowalpha_min    = utils:stats(Id, "LowAlpha", min),
        highbeta_min    = utils:stats(Id, "HighBeta", min),
        lowbeta_min    = utils:stats(Id, "LowBeta", min),
        meditation_peaks  = utils:stats(Id, "Meditation", peaks),
        attention_peaks   = utils:stats(Id, "Attention", peaks),
        delta_peaks       = utils:stats(Id, "Delta", peaks),
        theta_peaks       = utils:stats(Id, "Theta", peaks),
        midgamma_peaks   = utils:stats(Id, "MidGamma", peaks),
        highalpha_peaks   = utils:stats(Id, "HighAlpha", peaks),
        lowgamma_peaks    = utils:stats(Id, "LowGamma", peaks),
        lowalpha_peaks    = utils:stats(Id, "LowAlpha", peaks),
        highbeta_peaks    = utils:stats(Id, "HighBeta", peaks),
        lowbeta_peaks    = utils:stats(Id, "LowBeta", peaks)
    },
  case db_queries:stats_exist(Id) of
    [_] -> db_queries:update_stats(StatsRec);
    [] -> db_queries:make_stats(StatsRec)
  end.

%% @doc Formats Stats for templates
format_stats(Stats) ->
  [{_Id, Meditation_avg, Attention_avg, Delta_avg, Theta_avg, MidGamma_avg, HighAlpha_avg, LowGamma_avg, LowAlpha_avg, HighBeta_avg, LowBeta_avg, Meditation_max, Attention_max, Delta_max, Theta_max, MidGamma_max, HighAlpha_max, LowGamma_max, LowAlpha_max, HighBeta_max, LowBeta_max, Meditation_min, Attention_min, Delta_min, Theta_min, MidGamma_min, HighAlpha_min, LowGamma_min, LowAlpha_min, HighBeta_min, LowBeta_min, Meditation_peaks, Attention_peaks, Delta_peaks, Theta_peaks, MidGamma_peaks, HighAlpha_peaks, LowGamma_peaks, LowAlpha_peaks, HighBeta_peaks, LowBeta_peaks }] = Stats,
    [{"meditation_avg", Meditation_avg}, {"attention_avg", Attention_avg},
     {"delta_avg", Delta_avg}, {"theta_avg", Theta_avg},
     {"midgamma_avg", MidGamma_avg}, {"highalpha_avg", HighAlpha_avg},
     {"lowgamma_avg", LowGamma_avg}, {"lowalpha_avg", LowAlpha_avg},
     {"highbeta_avg", HighBeta_avg}, {"lowbeta_avg", LowBeta_avg},
     {"meditation_max", Meditation_max}, {"attention_max", Attention_max},
     {"delta_max", Delta_max}, {"theta_max", Theta_max},
     {"midgamma_max", MidGamma_max}, {"highalpha_max", HighAlpha_max},
     {"highbeta_max", HighBeta_max}, {"lowbeta_max", LowBeta_max},
     {"lowgamma_max", LowGamma_max}, {"lowalpha_max", LowAlpha_max},
     {"meditation_min", Meditation_min}, {"attention_min", Attention_min},
     {"delta_min", Delta_min}, {"theta_min", Theta_min},
     {"midgamma_min", MidGamma_min}, {"highalpha_min", HighAlpha_min},
     {"lowgamma_min", LowGamma_min}, {"lowalpha_min", LowAlpha_min},
     {"highbeta_min", HighBeta_min}, {"lowbeta_min", LowBeta_min},
     {"lowgamma_min", LowGamma_min}, {"lowalpha_min", LowAlpha_min},
     {"meditation_peaks", Meditation_peaks}, {"attention_peaks", Attention_peaks},
     {"delta_peaks", Delta_peaks}, {"theta_peaks", Theta_peaks},
     {"midgamma_peaks",MidGamma_peaks}, {"highalpha_peaks", HighAlpha_peaks},
     {"lowgamma_peaks", LowGamma_peaks}, {"lowalpha_peaks", LowAlpha_peaks},
     {"highbeta_peaks", HighBeta_peaks}, {"Highbeta_peaks", HighBeta_peaks},
     {"lowbeta_peaks", LowBeta_peaks}, {"lowbeta_peaks", LowBeta_peaks}].
     
%% @doc Average numbers for stats
average(X) -> average(X, 0, 0).

average([H|T], Length, Sum) ->
  average(T, Length + 1, Sum + H);
average([], Length, Sum) ->
  Avg = Sum / Length,
  round(Avg * 100) / 100.

% @doc Count the peaks in a waveform (Needs to be replaced with real code).
peaks(X) ->
  Max = lists:max(X),
  Half = Max / 2,
  Val = [Y || Y <- X, Y > Half+10],
  erlang:length(Val).

