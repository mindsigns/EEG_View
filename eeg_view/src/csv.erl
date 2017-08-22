%% ------------------------------------------------------------------
%% @author Jon Trembley <jon@deathray.tv>
%% @doc    Handle writing CSV files 
%% ------------------------------------------------------------------
-module(csv).
-author('jon <jon@deathray.tv>').

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-export([write_file/2]).

-define(DATADIR, eeg_env:get_env(out_file_dir)).

%% ------------------------------------------------------------------
write_file(File, Id) ->
    Qentries = db_queries:sessions(Id),
    [{_Id, Starttime, Stoptime, _Name, _Desc}] = Qentries,

    List = db_queries:export(Starttime, Stoptime),

    OutFile = ?DATADIR ++ File,

    case filelib:is_regular(OutFile) of
        false -> 
                {ok, Stream} = file:open(OutFile, write),
                Pid = counter:start(),
                io:fwrite(Stream,"~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p~n", ["Tick","Meditation", "Attention", "Delta", "Theta", "HighAlpha", "LowAlpha", "HighBeta", "LowBeta", "MidGamma", "LowGamma"]),

                lists:foreach(fun(X) -> 
                    {_Timestamp, Meditation, Attention, Delta, Theata, HighAlpha, LowAlpha, HighBeta, LowBeta, MidGamma, LowGamma} = X,
                    counter:increment(Pid),
                    Count = counter:value(Pid),
                    io:format(Stream,"~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p~n",[Count, Meditation, Attention, Delta, Theata, HighAlpha, LowAlpha, HighBeta, LowBeta, MidGamma, LowGamma]) end, List),

                    counter:stop(Pid),
                    file:close(Stream);
        true -> error
    end.
 
