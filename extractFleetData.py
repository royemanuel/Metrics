import time
import os
import pandas as pd

timeNow = time.strftime("%Y%m%d-%H%M%S")
os.makedirs(timeNow)
os.path.join(timeNow +'/')


def allBB(acDict):
    for num, ac in acDict.items():
        filename = timeNow + '/AC' + str(num) + '.csv'
        ac.blueBook.to_csv(filename)

def concBB(acDict, sr):
    df = []
    for num, ac in acDict.items():
        df.append(ac.blueBook)
    filename = timeNow + '/ALL' + sr + '.csv'
    if len(df) > 0:
        dfcsv = pd.concat(df)
        dfcsv.to_csv(filename)
        return(dfcsv)

def buildFiles(acListDict):
    masterDF = []
    for lname, lobject in acListDict.items():
        allBB(lobject)
        g = concBB(lobject, lname)
        masterDF.append(g)
    masterDF = pd.concat(masterDF)
    fn = timeNow + '/ALL.csv'
    masterDF.to_csv(fn)
    fnst = timeNow + '/skedTracker.csv'
    sked.tracker.to_csv(fnst)

        
# allBB(boneYard)
# allBB(flightLine)
# allBB(SLEPlist)
# 
# concBB(boneYard, 'BY')
# concBB(flightLine, 'FL')
# g = concBB(SLEPlist, 'SLEP')

buildFiles({'BY' : boneYard,
            'FL' : flightLine,
            'SLEP' : SLEPlist})
