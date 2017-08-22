#!/usr/bin/env python
"""
DESCRIPTION
    A basic EEG logging program for the NeuroSky Mindwave headset.
    Data is logged into a sqlite db for later use.

AUTHOR
    jon <jon@mindsign.net>

"""
# NeuroPy
# https://github.com/lihas/NeuroPy

from NeuroPy import NeuroPy
import sqlite3
import signal
import time
import sys


def timestamp():
   now = time.time()
   localtime = time.localtime(now)
   milliseconds = '%03d' % int((now - int(now)) * 1000)
   return time.strftime('%Y%m%d%H%M%S', localtime) + milliseconds


def signal_handler(signal, frame):
        print 'Finished logging'
        cur.execute('''UPDATE session Set StopTime = ? WHERE ID = ?''', (timestamp(), SessId))
        con.commit()
        object1.stop()
        con.close()
        sys.exit(0)

def attention_callback(attention_value):
    "this function will be called everytime NeuroPy has a new value for attention"
    print "Value of attention is",attention_value
    return None

# The MindWave device
object1=NeuroPy("/dev/ttyUSB0", 115200) 

con = sqlite3.connect('./eeg.db')
cur = con.cursor()

# Setup signal handler
signal.signal(signal.SIGINT, signal_handler)


# Get some session input before recording
SessName = raw_input('Session name: ')
SessNotes = raw_input('Session notes: ')
cur.execute('''INSERT INTO session(StartTime, Notes, Name) VALUES (?, ?, ?)''', (timestamp(), SessNotes, SessName))
con.commit()
cur.execute('''SELECT ID FROM session WHERE Name = ?''', (SessName,))
SessIdtmp = cur.fetchone()
# From tuple to int
SessId = SessIdtmp[0]

print ("Logging to session %s" % SessName);


#set call back:
object1.setCallBack("attention",attention_callback)
#call start method
object1.start()

while True:
    time.sleep(0.5)
    ts = timestamp()
    cur.execute('''INSERT INTO eeg(Timestamp, Meditation, Attention, Delta, Theta, HighAlpha, LowAlpha, HighBeta, LowBeta, MidGamma, LowGamma) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)''', (ts, object1.meditation, object1.attention, object1.delta, object1.theta, object1.highAlpha, object1.lowAlpha, object1.highBeta, object1.lowBeta, object1.midGamma, object1.lowGamma))
    con.commit()    
    if(object1.poorSignal>10):
        print "Poor Signal: ", object1.poorSignal
    #    object1.stop()

