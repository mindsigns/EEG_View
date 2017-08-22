#!/bin/sh
cd `dirname $0`
#exec erl -config priv/sys.config -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s eeg_view
#exec /home/jon/src/Erlang/Erlang17.0/bin/erl -config priv/sys.config -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s eeg_view
exec erl -config priv/sys.config -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s eeg_view -name eegview
