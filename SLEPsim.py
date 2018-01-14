import simpy
import numpy as np
from math import log, exp, fabs, sqrt
import csv
import time
import random
import scipy.stats as st
import os
import shutil

def calcScale(Mode, Mean):
    Scale = (np.log(Mode) +2 *np.log(Mean))/3
    return Scale

def calcShape(Mode, Mean):
    Shape = np.sqrt((2/3)*np.log(Mean)-(2/3)*np.log(Mode))
    return Shape

def acGenerator(env, profile, SLEParrival, InductNumber, simDis):
    # Always generate the first two parts
    # For loop provides the initial year of 24 aircraft
    # 180 aircraft in full rate SLEP
    for i in range(InductNumber):
        f = i+1
        env.process(part(env, 'wing', 'Wing %d' % f,
                         wingSLEPline, wingScale, wingShape,
                         wingMode, wingHH, simDis, UseSideline))
        env.process(part(env, 'fuselage', 'Fuselage %d' % f,
                         fusSLEPline, fusScale, fusShape,
                         fusMode, fusHH, simDis, UseSideline))
        # Generate the profile of inductions
    while True:
        yield env.timeout(SLEParrival)
        # x = int(env.now/SLEParrival) - 1
        # y = env.now % SLEPyear
        # print("x" + str(x))
        # print("y" + str(y))
        if (profile[0] > 0 and env.now > 0):
            for k in range(profile[0]):
                f += 1
                env.process(part(env, 'wing', 'Wing %d' % f,
                                       wingSLEPline, wingScale, wingShape,
                                       wingMode, wingHH, simDis, UseSideline))
                env.process(part(env, 'fuselage', 'Fuselage %d' % f,
                                       fusSLEPline, fusScale, fusShape,
                                       fusMode, fusHH, simDis, UseSideline))
            profile = profile[1:]
def AllacGenerator(env, TotalSLEP, simDis):
    # Use this function to induct all aircraft at time 0.
    # It is equivalent to "just in time" induction for the entire
    # fleet
    for i in range(TotalSLEP):
        f = i+1
        env.process(part(env, 'wing', 'Wing %d' % f,
                         wingSLEPline, wingScale, wingShape,
                         wingMode, wingHH, simDis,UseSideline))
        env.process(part(env, 'fuselage', 'Fuselage %d' % f,
                         fusSLEPline, fusScale, fusShape,
                         fusMode, fusHH, simDis, UseSideline))

class SLEPline(object):
    # A SLEPline has a limited number of spots, wing and fuselage, for
    # doing work in parallel. An aircraft has to request one spot of
    # each. A sidelined part requests only the type of spot that matches
    # it.
    def __init__(self, env, numSpots, Mode, HH, sidelineAvail):
        self.env = env
        self.Spot = simpy.Resource(env, numSpots)
        self.Mode = Mode
        self.HH = HH
        self.SLA = sidelineAvail
        self.count = []
        self.runCount = env.process(self.countUsage(env))
    def repair(self, name, repTime, partType):
        if repTime > self.HH:
            if self.SLA == 'Yes':
                partRolls.append([partType, name, env.now, repTime,
                                  'YES', n+1])
                yield self.env.timeout(self.Mode)
            else:
                repTime = self.HH
                partRolls.append([partType, name, env.now, repTime,
                                  'NO', n+1])
                yield self.env.timeout(repTime)
                yield env.process(SLEP(env, partType))
            # print(name + '>>>is sidelined at time %d' % env.now)
        elif repTime < self.Mode:
            repTime = self.Mode
            partRolls.append([partType, name, env.now, repTime, 'NO', n+1])
            yield self.env.timeout(repTime)
            yield env.process(SLEP(env, partType))
            # print(name + "***completed at time %d" % env.now)
        else:
            partRolls.append([partType, name, env.now, repTime, 'NO', n+1])
            yield self.env.timeout(repTime)
            yield env.process(SLEP(env, partType))
            # print(name + "+++completed at time %d" % env.now)
    def countUsage(self, env):
        while True:
            self.count.append([self.Spot.count, env.now, n+1])
            yield env.timeout(1)

def SLEP(env, partType):
    if partType == "wing":
        yield wingSLEP.put(1)
        print("wing SLEP'ed at time %d" % env.now)
    elif partType == "fuselage":
        yield fusSLEP.put(1)
        print("fus SLEP'ed at time %d" % env.now)

def SL(env, name, SLtime, partType, simDis):
    sideline.append([partType, name, SLtime, 'into SideLine',
                     env.now, n+1])
    yield env.timeout(SLtime)
    sideline.append([partType, name, SLtime, 'out of SideLine',
                     env.now, n+1])
    # print(name + " is coming out of sideline at time %d" % env.now)
    if partType == "wing":
        yield env.process(part(env, partType, name, wingSLEPline,
                               wingScale, wingShape,wingMode, wingHH, simDis, UseSideline))
    elif partType == "fuselage":
        yield env.process(part(env, partType, name, fusSLEPline,
                               fusScale, fusShape, fusMode, fusHH, simDis, UseSideline))

def part(env, partType, name, SLPln, Scale, Shape, Mode, HH, simDis, SLA):
    # Assign a repair time to a part
    if simDis == 'L':
        repTime = np.random.lognormal(Scale, Shape)
    elif simDis == 'P':
        draw = np.random.uniform()
        if draw > 0.3:
            repTime = Mode
        else:
            repTime = Mode + np.exp((1/Shape)*log(Scale/draw))
    spotReq.append([partType, name, repTime, env.now, 'request', n+1])
    # print(name + " rolled a %f" % repTime)
    # Send it to a wing spot when available
    with SLPln.Spot.request() as request:
        yield request
        # print(name +' enters SLEP at time %d' % env.now)
        spotReq.append([partType, name, repTime, env.now, 'start', n+1])
        yield env.process(SLPln.repair(name, repTime, partType))
        spotReq.append([partType, name, repTime, env.now, 'complete', n+1])
    if SLA == "Yes":
        if repTime > HH:
            yield env.process(SL(env, name, HH-Mode, partType, simDis))


# buttonAC is the process that takes a completed fuselage and completed
# and puts them in the outpipe. It also records the status at every
# time tick
def buttonAC(env):
    while True:
        if (wingSLEP.level > 0 and fusSLEP.level > 0):
            yield wingSLEP.get(1)
            yield fusSLEP.get(1)
            yield outPipe.put(1)
            acComp.append([outPipe.level, env.now, n+1])
            # Status.append([env.now, inPipe.level, outPipe.level,
                           # wingSLEP.level, fusSLEP.level, self.name,
                           # self.wing.level, self.fus.level, i+1])
            # print("Aircraft %d buttoned up at time %d" %
                  # (outPipe.level, env.now))
        else:
            yield env.timeout(1)
            #Status.append([env.now, inPipe.level, outPipe.level,
            #               wingSLEP.level, fusSLEP.level, i+1])
###############################################################
## Building around the whole simulation to automate over many##
## different factors                                         ##
###############################################################
timeNow = time.strftime("%Y%m%d-%H%M%S")
timeNow = timeNow + 'ModeMeanSL'
os.makedirs(timeNow)
os.path.join(timeNow +'/')
shutil.copyfile('baseSLEP.csv', timeNow + '/' + 'baseSLEP.csv')
m = 0
with open("JustWingsPower.csv", newline='') as simfile:
    # quickrun.csv
    simReader = csv.reader(simfile, delimiter = ',', quotechar = '|')
    for row in simReader:
        print(row)
        simDis = row[len(row)-1]
        m = m+1
        if simDis == 'L':
            fusMode = int(row[0])
            fusHH = int(row[1])
            fusPercent = float(row[2])
            fusMean = fusMode * 1.33 #changed from 1.25 to 1.33 for the
            # lower mode values of 6 and 8 for the fus and wing
            wingMode = int(row[3])
            wingHH = int(row[4])
            wingPercent = float(row[5])
            wingMean = wingMode * 1.25
            seedWings = int(row[6])
            numSpots = int(row[7])
            UseSideline = str(row[8])
            TotalSLEP = int(row[9])
            Induction = str(row[10])
            NumberRuns = int(row[11])
            # Math for calculating the lognormal mu and sigma
            # In Triple quotes is the old method depending upon the HH
            # numbers and percentage
            '''wingZ = st.norm.ppf(1 - wingPercent)
            wingSigN    =  (-wingZ + sqrt(wingZ**2 -
                                          4*log(wingMode/wingHH)))/2
            wingMuN     =  log(wingHH) -wingZ*wingSigN
            fusZ = st.norm.ppf(1 - fusPercent)
            fusSigN    =  (-fusZ + sqrt(fusZ**2 -
                                        4*log(fusMode/fusHH)))/2
            fusMuN     =  log(fusHH) -fusZ*fusSigN'''
            # New calculation uses the Expected Mode and Mean for repair
            # times
            wingScale = calcScale(wingMode, wingMean)
            wingShape = calcShape(wingMode, wingMean)
            fusScale = calcScale(fusMode, fusMean)
            fusShape = calcShape(fusMode, fusMean)
        elif simDis == 'P':
            fusMode =  int(row[0])
            fusScale = float(row[1])
            fusShape = float(row[2])
            wingMode =int(row[3])
            wingScale = float(row[4])
            wingShape = float(row[5])
            seedWings = int(row[6])
            numSpots = int(row[7])
            UseSideline = str(row[8])
            TotalSLEP = int(row[9])
            Induction = str(row[10])
            NumberRuns = int(row[11])
            wingHH = int(row[12])
            fusHH = wingHH
        ######################
        # Run the simulation #
        ######################
randomSeed = [1]
m = 1
#[1,2,23,53,13,144,78,54, 583, 97, 620, 57, 663, 459,
             # 725, 265, 129, 18, 970, 412, 647, 713, 594, 76, 615,
             # 620, 621, 545, 299, 977, 677, 919, 851, 60, 977, 527,
             # 198, 706, 47, 462, 798, 995, 899, 661, 978, 362, 842,
             # 821, 254, 679]
Status = []
sideline = []
acComp = []
partRolls = []
spotReq = []
wingCount =[]
fusCount = []
numSpots = 4
wingMode = 13
wingHH = 20
fusMode = 13
fusHH = 20
simDis = 'P'
UseSideline = 'Yes'

sideline.append(['PartType','PartName','SidelineTime',
                 'InOut','TimeInOut', 'Run'])
acComp.append(['TotalComplete','Time', 'Run'])
partRolls.append(['PartType','PartName', 'TimeAtRoll',
                  'Repair Time', 'Sidelined?', 'Run'])
spotReq.append(['PartType','PartName', 'RepairTime',
                'TimeEnteredSpot', 'StartComplete','Run'])
print('simulating %d times for Scenario %d' %
      (len(randomSeed), m))
for n in range(len(randomSeed)):
    env = simpy.Environment()
    rs = randomSeed[n]
    np.random.seed(rs)
    wingSLEP = simpy.Container(env, init = 0)
    fusSLEP = simpy.Container(env, init = 0)
    outPipe = simpy.Container(env, init = 0)
    env.process(buttonAC(env))
    wingSLEPline = SLEPline(env, numSpots, wingMode, wingHH,
                            UseSideline)
    fusSLEPline = SLEPline(env, numSpots, fusMode,
                           fusHH, UseSideline)
    if Induction == "JIT":
        AllacGenerator(env, TotalSLEP, simDis)
    elif Induction == "Month":
        TSwhole = int(TotalSLEP/2) - 1
        TSrem = TotalSLEP % 2
        SLEParrival = 4.33
        profile = [2]*TSwhole  + [0]*1560
        env.process(acGenerator(env, profile, SLEParrival, 2, simDis))
    elif Induction == "Year":
        TSywhole = int(TotalSLEP/24) - 1
        TSyrem = TotalSLEP % 24
        SLEParrival = 52
        profile = [24]*TSywhole + [TSyrem] + [0]*30
        env.process(acGenerator(env, profile, SLEParrival, 24, simDis))
    env.run(until = 52*30)
    wingCount = wingCount +wingSLEPline.count
    fusCount = fusCount + fusSLEPline.count
        ###################
        ## End simulation##
        ## Start storing ##
        ###################
        ## Build the date-time stamp for the file to prevent
        ## writing more to the existing files
    timestr = 'Scen' +str(m) +simDis
    acCompName = timeNow + "/acComp" +timestr + '.csv'
    partRollsName = timeNow +"/PartRolls"+timestr + '.csv'
    sidelineName = timeNow +"/sideline"+timestr + '.csv'
    spotReqName = timeNow +"/spotReq"+timestr + '.csv'
    wingCtName = timeNow +"/wingSpotCount"+timestr + '.csv'
    fusCtName = timeNow +"/fusSpotCount"+timestr + '.csv'
    # Write the files
    # When Each Aircraft is completed
    acFile = open(acCompName, 'w', newline ='')
    acData = csv.writer(acFile)
    for a in range(len(acComp)):
        acData.writerow(acComp[a])
    del acData
    acFile.close()
    del acFile
    # The roll for each part
    prFile = open(partRollsName, 'w', newline = '')
    prData = csv.writer(prFile)
    for c in range(len(partRolls)):
        prData.writerow(partRolls[c])
    del prData
    prFile.close()
    del prFile
    # Which parts are sidelined
    sidelineFile = open(sidelineName, 'w', newline = '')
    sidelineData = csv.writer(sidelineFile)
    for d in range(len(sideline)):
        sidelineData.writerow(sideline[d])
    del sidelineData
    sidelineFile.close()
    del sidelineFile
    # Spot Requests
    spotReqFile = open(spotReqName, 'w', newline ='')
    spotReqData = csv.writer(spotReqFile)
    for g in range(len(spotReq)):
        spotReqData.writerow(spotReq[g])
    del spotReqData
    spotReqFile.close()
    del spotReqFile
    # Wing Spot Usage
    wingCtFile = open(wingCtName, 'w', newline ='')
    wingCtData = csv.writer(wingCtFile)
    for wCount in range(len(wingCount)):
        wingCtData.writerow(wingCount[wCount])
    del wingCtData
    wingCtFile.close()
    del wingCtFile
    # Fuselage Spot Usage
    fusCtFile = open(fusCtName, 'w', newline ='')
    fusCtData = csv.writer(fusCtFile)
    for fCount in range(len(fusCount)):
        fusCtData.writerow(fusCount[fCount])
    del fusCtData
    fusCtFile.close()
    del fusCtFile
    # Simulation Parameters
    SPname = timeNow + "/SimulationParameters" + timestr +".csv"
    simParam = open(SPname, 'w', newline='')
    spdata = csv.writer(simParam)
    if simDis =='L':
        spdata.writerow(['Distribution','Wing Scale', 'Wing Shape',
                         'Fuselage Scale', 'Fuselage Shape',
                        'Random Seed', '# of Seed Wings',
                        'Wing Mode', 'Wing Percentage', 'Wing Max',
                        'Fuselage Mode', 'Fuselage Percentage',
                        'HeadHurter','SidelineAvailability', 'NumSpots',
                        'Total SLEP', 'Induction Rate'])
        spdata.writerow(['LogNormal', wingScale, wingShape, fusScale,
                         fusShape, randomSeed, seedWings,
                         wingMode, wingPercent, wingHH, fusMode,
                         fusPercent, fusHH, UseSideline, numSpots,
                         TotalSLEP, Induction])
    elif simDis == 'P':
        spdata.writerow(['Distribution','Wing Mode',
                         'Wing Scale', 'Wing Shape', 'Fuselage Mode',
                         'Fuselage Scale', 'Fuselage Shape',
                        'HeadHurter', 'Random Seed',
                        '# of Seed Wings', 'SidelineAvailability',
                        'NumSpots', 'Total SLEP', 'Induction Rate'])
        spdata.writerow(['Levin Power Law', wingMode, wingScale,
                         wingShape, fusMode, fusScale, fusShape, wingHH,
                         randomSeed, seedWings, UseSideline, numSpots,
                         TotalSLEP, Induction])
    del spdata
    simParam.close()
    del simParam
