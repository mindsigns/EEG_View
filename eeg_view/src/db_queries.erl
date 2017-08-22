%% ------------------------------------------------------------------
%% @author Jon Trembley <jon@deathray.tv>
%% @doc    Database queries
%% ------------------------------------------------------------------
-module(db_queries).
-author('Jon Trembley <jon@deathray.tv>').

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-export([
         export/2,
         sessions/0,
         sessions/1,
         update_session/5,
         stats/1,
         stats_exist/1,
         make_stats/1,
         update_stats/1,
         compare/0,
         get_sess_name/1,
         delete/1    
]).

%% ------------------------------------------------------------------
%% Header includes
%% ------------------------------------------------------------------
-include("eeg.hrl").

-define(DATABASE, eeg_env:get_env(database)).


%% ------------------------------------------------------------------
%% Functions
%% ------------------------------------------------------------------

%% @doc Return eeg data for sessions based on timestamps.
export(Starttime, Stoptime) ->
    {ok, Db} = esqlite3:open(?DATABASE),
    esqlite3:q("SELECT * FROM eeg WHERE Timestamp BETWEEN ? AND ?", [Starttime, Stoptime], Db).

%% @doc Return a list of all recorded sessions.
sessions()->
    {ok, Db} = esqlite3:open(?DATABASE),
    esqlite3:q("SELECT * FROM session ORDER BY id", Db).

%% @doc Return a single recorded session.
sessions(Id)->
    {ok, Db} = esqlite3:open(?DATABASE),
    esqlite3:q("SELECT * FROM session WHERE id=?",[Id], Db).

%% @doc Update a recorded session.
update_session(Id, StartTime, StopTime, Notes, Name)->
  {ok, Db} = esqlite3:open(?DATABASE),
  esqlite3:q("UPDATE session SET StartTime=?, StopTime=?, Notes=?, Name=? WHERE id=?",[StartTime, StopTime,Notes,Name,Id], Db).

%% @doc Return stats for a given ID.
stats(Id)->
    {ok, Db} = esqlite3:open(?DATABASE),
    esqlite3:q("SELECT * FROM stats WHERE id=?",[Id], Db).

%% @doc Checks to see if stats exist for a session.
stats_exist(Id) ->
    {ok, Db} = esqlite3:open(?DATABASE),
    esqlite3:q("SELECT id FROM stats WHERE id=?", [Id],Db).

%% @doc Store stats for a session.
make_stats(Rec) ->
    {ok, Db} = esqlite3:open(?DATABASE),
    esqlite3:q("INSERT INTO stats VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", [Rec#stats.id, Rec#stats.meditation_avg, Rec#stats.attention_avg, Rec#stats.delta_avg, Rec#stats.theta_avg, Rec#stats.midgamma_avg, Rec#stats.highalpha_avg, Rec#stats.lowgamma_avg, Rec#stats.lowalpha_avg, Rec#stats.highbeta_avg, Rec#stats.lowbeta_avg, Rec#stats.meditation_max, Rec#stats.attention_max, Rec#stats.delta_max, Rec#stats.theta_max, Rec#stats.midgamma_max, Rec#stats.highalpha_max, Rec#stats.lowgamma_max, Rec#stats.lowalpha_max, Rec#stats.highbeta_max, Rec#stats.lowbeta_max, Rec#stats.meditation_min, Rec#stats.attention_min, Rec#stats.delta_min, Rec#stats.theta_min, Rec#stats.midgamma_min, Rec#stats.highalpha_min, Rec#stats.lowgamma_min, Rec#stats.lowalpha_min, Rec#stats.highbeta_min, Rec#stats.lowbeta_min, Rec#stats.meditation_peaks, Rec#stats.attention_peaks, Rec#stats.delta_peaks, Rec#stats.theta_peaks, Rec#stats.midgamma_peaks, Rec#stats.highalpha_peaks, Rec#stats.lowgamma_peaks, Rec#stats.lowalpha_peaks, Rec#stats.highbeta_peaks, Rec#stats.lowbeta_peaks], Db).

%% @doc Update stats for a session.
update_stats(Rec) ->
    {ok, Db} = esqlite3:open(?DATABASE),
    esqlite3:q("UPDATE stats SET meditation_avg=?, attention_avg=?, delta_avg=?, theta_avg=?, midgamma_avg=?, highalpha_avg=?, lowgamma_avg=?,lowalpha_avg=?, highbeta_avg=?, lowbeta_avg=?, meditation_max=?,  attention_max=?,  theta_max=?,  delta_max=?,  midgamma_max=?,  highalpha_max=?,  lowgamma_max=?,  lowalpha_max=?, highbeta_max=?, lowbeta_max=?, meditation_min=?,  attention_min=?,  delta_min=?,  theta_min=?,  midgamma_min=?,  highalpha_min=?,  lowgamma_min=?,  lowalpha_min=?, highbeta_min=?, lowbeta_min=?, meditation_peaks=?,  attention_peaks=?,  delta_peaks=?,  theta_peaks=?,  midgamma_peaks=?,  highalpha_peaks=?, lowgamma_peaks=?, lowalpha_peaks=? , highbeta_peaks=?, lowbeta_peaks=? WHERE id=? ", [Rec#stats.meditation_avg, Rec#stats.attention_avg, Rec#stats.delta_avg, Rec#stats.theta_avg, Rec#stats.midgamma_avg, Rec#stats.highalpha_avg, Rec#stats.lowgamma_avg, Rec#stats.lowalpha_avg, Rec#stats.highbeta_avg, Rec#stats.lowbeta_avg, Rec#stats.meditation_max, Rec#stats.attention_max, Rec#stats.delta_max, Rec#stats.theta_max, Rec#stats.midgamma_max, Rec#stats.highalpha_max, Rec#stats.lowgamma_max, Rec#stats.lowalpha_max, Rec#stats.highbeta_max, Rec#stats.lowbeta_max, Rec#stats.meditation_min, Rec#stats.attention_min, Rec#stats.delta_min, Rec#stats.theta_min, Rec#stats.midgamma_min, Rec#stats.highalpha_min, Rec#stats.lowgamma_min, Rec#stats.lowalpha_min, Rec#stats.highbeta_min, Rec#stats.lowbeta_min, Rec#stats.meditation_peaks, Rec#stats.attention_peaks, Rec#stats.delta_peaks, Rec#stats.theta_peaks, Rec#stats.midgamma_peaks, Rec#stats.highalpha_peaks, Rec#stats.lowgamma_peaks, Rec#stats.lowalpha_peaks, Rec#stats.highbeta_peaks, Rec#stats.lowbeta_peaks, Rec#stats.id], Db).

%% @doc Return a list of session stats for comparison.
compare() ->
    {ok, Db} = esqlite3:open(?DATABASE),
    esqlite3:q("SELECT id, meditation_avg, attention_avg, delta_avg, theta_avg, highalpha_avg, lowalpha_avg, highbeta_avg, lowbeta_avg FROM stats ORDER BY id", Db).

%% @doc Return a sessions name based on a given ID.
get_sess_name(Id) ->
    {ok, Db} = esqlite3:open(?DATABASE),
    esqlite3:q("SELECT Name FROM session WHERE id=?",[Id], Db).
    
%% @doc Delete Stats and Session info based on ID.
delete(Id)->
    {ok, Db} = esqlite3:open(?DATABASE),
    esqlite3:q("DELETE FROM stats WHERE id=?",[Id], Db),
    esqlite3:q("DELETE FROM session WHERE id=?",[Id], Db).
