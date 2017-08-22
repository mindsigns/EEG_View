#!/usr/bin/env python
"""
Graphs EEG data from a sqlite db.
"""
import matplotlib.pyplot as plt
from matplotlib.pyplot import *
import sqlite3


con = sqlite3.connect('./eeg.db')
cur = con.cursor()

cur.execute('''SELECT ID, Name, Notes FROM session''')
rows = cur.fetchall()

# LowFi menu
print (30 * '-')
print "ID : TITLE\t:: DESCRIPTION"
print (30 * '-')
for row in rows:
    print row[0],":", row[1], "\t:: ",row[2]
print (30 * '-')
Session= raw_input('Session to graph <ID>: ')

cur.execute('''SELECT StartTime, StopTime FROM session WHERE ID=?''', (Session,))
timestamps = cur.fetchone()
Start = timestamps[0]
Stop = timestamps[1]

cur.execute('''SELECT Meditation FROM eeg WHERE Timestamp BETWEEN ? AND ?''', (Start, Stop))
meditation=[r[0] for r in cur.fetchall()]

cur.execute('''SELECT Attention FROM eeg WHERE Timestamp BETWEEN ? AND ?''', (Start, Stop))
attention=[r[0] for r in cur.fetchall()]

cur.execute('''SELECT Delta FROM eeg WHERE Timestamp BETWEEN ? AND ?''', (Start, Stop))
delta=[r[0] for r in cur.fetchall()]

cur.execute('''SELECT Theta FROM eeg WHERE Timestamp BETWEEN ? AND ?''', (Start, Stop))
theta=[r[0] for r in cur.fetchall()]

cur.execute('''SELECT LowAlpha FROM eeg WHERE Timestamp BETWEEN ? AND ?''', (Start, Stop))
lowAlpha=[r[0] for r in cur.fetchall()]

cur.execute('''SELECT HighAlpha FROM eeg WHERE Timestamp BETWEEN ? AND ?''', (Start, Stop))
highAlpha=[r[0] for r in cur.fetchall()]

cur.execute('''SELECT LowBeta FROM eeg WHERE Timestamp BETWEEN ? AND ?''', (Start, Stop))
lowBeta=[r[0] for r in cur.fetchall()]

cur.execute('''SELECT HighBeta FROM eeg WHERE Timestamp BETWEEN ? AND ?''', (Start, Stop))
highBeta=[r[0] for r in cur.fetchall()]

cur.execute('''SELECT MidGamma FROM eeg WHERE Timestamp BETWEEN ? AND ?''', (Start, Stop))
midGamma=[r[0] for r in cur.fetchall()]

cur.execute('''SELECT LowGamma FROM eeg WHERE Timestamp BETWEEN ? AND ?''', (Start, Stop))
lowGamma=[r[0] for r in cur.fetchall()]


f, axarr = plt.subplots(5, sharex=True)

#plt.plot(delta, label="Delta")
#plt.plot(theta, label="Theta")
#plt.plot(lowAlpha, label="Low Alpha")
#plt.plot(highAlpha, label="High Alpha")
#plt.plot(lowBeta, label="Low Beta")
#plt.plot(highBeta, label="High Beta")
#plt.plot(lowGamma, label="Low Gamma")
#plt.plot(midGamma, label="Mid Gamma")
#plt.plot(meditation, label="Meditation")
#plt.plot(attention, label="Attention")

axarr[0].plot(delta, label="Delta")
axarr[0].plot(theta, label="Theta")

axarr[1].plot(highAlpha, label="High Alpha")
axarr[1].plot(lowAlpha, label="Low Alpha")

axarr[2].plot(lowBeta, label="Low Beta")
axarr[2].plot(highBeta, label="High Beta")

axarr[3].plot(lowGamma, label="Low Gamma")
axarr[3].plot(midGamma, label="Mid Gamma")

axarr[4].plot(meditation, label="Meditation")
axarr[4].plot(attention, label="Attention")

axarr[0].legend(bbox_to_anchor=(1, 1), loc=2, borderaxespad=0.)
axarr[1].legend(bbox_to_anchor=(1, 1), loc=2, borderaxespad=0.)
axarr[2].legend(bbox_to_anchor=(1, 1), loc=2, borderaxespad=0.)
axarr[3].legend(bbox_to_anchor=(1, 1), loc=2, borderaxespad=0.)
axarr[4].legend(bbox_to_anchor=(1, 1), loc=2, borderaxespad=0.)

#plt.title(row[1])
plt.show()
