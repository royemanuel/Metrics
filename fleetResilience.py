
# Simulate the behavior of a fleet of aircraft. Begin by
# importing what we need.

import simpy
import numpy as np
# import scipy.stats as st
import pandas as pd
# import pdb
import random
import time
time.process_time()
time.perf_counter()
# pdb.set_trace()
# for now we will use the Python provided module random to build random
# numbers
# import random

######################################################################
# Build the historical DataFrames                                    #
######################################################################

######################################################################
# Aircraft Parts and assembling them to an aircraft                  #
######################################################################


# Build an aircraft part
# an aircraft part needs to work, fail, and be part of an aircraft
class Part(object):
    def __init__(self, env, ID, SLEP_limit, SLEPtime, lifeTime):
        self.ID = ID
        self.env = env
        self.bornDate = self.env.now
        self.age = 0
        self.fltHours = 0
        self.fltHrsSinceFail = 0
        self.status = True
        self.history = pd.DataFrame()
        # hardcoding this in to demo failtTime
        self.endtime = 25
        self.lifeTime = lifeTime
        self.fltFail = self.failTime(env, **{"endTime": self.endtime})
        self.SLEP_limit = SLEP_limit
        self.SLEP = False
        self.SLEPtime = SLEPtime
        self.request = None
        
    # Define a fail time for the particular part.
    def failTime(self, env, **kwargs):
        self.fltFail = np.random.randint(1, 25)
        # for kw in kwargs:
        #     if kwargs[kw] == "endTime":
        #         print("rollin' the dice")
        #         self.fltFail = np.random.random_integers(1, kwargs[kw])

    # Check the whether the part breaks during the operation
    def failFlight(self, env, fltTime):
        # print(np.allclose(self.fltFail, fltTime), self.ID)
        if (np.allclose(self.fltFail, fltTime)):
            self.fltHours += self.fltFail
            self.status = False
            self.age = self.env.now - self.bornDate
            self.history = self.history.append({"ID": self.ID,
                                                "Age": self.age,
                                                "FlightHours": self.fltHours,
                                                "TimeToFail": self.fltFail},
                                               ignore_index=True)
            # THis section will go with a repair function, but for now
            # I want to check the validity
            self.failTime(env, **{"endTime": self.endtime})
            self.fltHrsSinceFail = 0
        else:
            self.fltFail -= fltTime
            self.fltHours += fltTime
            self.age = self.env.now - self.bornDate
            self.history = self.history.append({"ID": self.ID,
                                                "Age": self.age,
                                                "FlightHours": self.fltHours,
                                                "TimeToFail": self.fltFail,
                                                "fhSinceFail": self.fltHrsSinceFail},
                                                ignore_index=True)

    # Check if the part needs to go to SLEP. Intend each part to call




def print_stats(res):
    print('%d of %d slots are allocated.' % (res.count, res.capacity))
    print('  Users:', res.users)
    print('  Queued events:', res.queue)


# The parts have different types of parts. I think this is the way
# to nest or do the inheritance part of OOP.
# Airframes are the source of the BuNo
class Airframe(Part):
    def __init__(self, env, ID):
        self.env = env
        self.obj = "Airframe"
        super().__init__(env, ID,
                         SLEP_limit = 80,
                         SLEPtime = 50,
                         lifeTime = 100)
        self.ageFail = 100
        # This needs to be a call to a method for part. ID the parameters
        # in the particular part
        self.fltFail = np.random.randint(1, 25 + 1)
        # print("Aircraft " + str(self.ID) + " " + str(self.fltFail))
        

# Avionics and Propusion behave the same. They will just draw from
# different distributions and be party to different updates / Tech
# refreshes.
class Avionics(Part):
    def __init__(self, env, ID):
        self.env = env
        self.obj = "Avionics"
        super().__init__(env, ID,
                         SLEP_limit = 120,
                         SLEPtime = 20000,
                         lifeTime = 10000)
        self.ageFail = 10
        self.fltFail = np.random.randint(1, 25 + 1)
        # print("Aircraft " + str(self.ID) + " " + str(self.fltFail))


class Propulsion(Part):
    def __init__(self, env, ID):
        self.env = env
        self.obj = "Propulsion"
        super().__init__(env, ID,
                         SLEP_limit = 120,
                         SLEPtime = 20000,
                         lifeTime = 10000)
        self.ageFail = 10
        self.fltFail = np.random.randint(1, 25 + 1)
        # print("Aircraft " + str(self.ID) + " " + str(self.fltFail))


class Aircraft(object):
    def __init__(self, env, af, av, puls):
        self.env = env
        self.obj = "Aircraft"
        self.af = Airframe(env, af)
        self.av = Avionics(env, av)
        self.puls = Propulsion(env, puls)
        self.BuNo = self.af.ID
        self.status = self.af.status & self.av.status & self.puls.status
        self.bornDate = self.env.now
        self.blueBook = pd.DataFrame()
        self.lifeTime = self.af.lifeTime
        self.fltHours = 0
        self.SLEP = False
        self.boneYard = False
        self.boneYardTime = None

    def updateBlueBook(self, env, fltTime):
        # print("Updating Blue Book")
        self.blueBook = self.blueBook.append({"Aircraft": self.BuNo,
                                              "FlightHours": fltTime,
                                              "AC Status": self.status,
                                              "Airframe ID": self.af.ID,
                                              "Airframe Status": self.af.status,
                                              "Avionics ID": self.av.ID,
                                              "Avionics Status": self.av.status,
                                              "Propulsion ID": self.puls.ID,
                                              "Propulsion Status": self.puls.status,
                                              "SLEP Status": self.af.SLEP,
                                              "Flight Date": self.env.now},
                                             ignore_index=True)
        if self.status is False:
        #     if self.af.status is False:
        #         repTime = 15
        #         yield env.timeout(repTime)
        #         self.af.status = True
        #         self.av.status = True
        #         self.puls.status = True
        #         print("The Airframe was broken, but now we fixed everything")
        #     elif self.puls.status is False:
        #         repTime = 15
        #         yield env.timeout(repTime)
        #         self.puls.status = True
        #         self.av.status = True
        #         print("The Engine busted, so we fixed that and checked the instruments")
        #     elif self.av.status is False:
        #         repTime = 15
        #         yield env.timeout(repTime)
        #         self.av.status = True
        #         print("Just the instruments were down. Up and at'em")
            afRepTime = 0
            avRepTime = 0
            pulsRepTime = 0
            if self.af.status is False:
                afRepTime = 15
                self.af.status = True
                # print("The Airframe was broken, but we bent some metal")
            if self.av.status is False:
                avRepTime = 15
                self.av.status = True
                # print("The instruments were down. Up and at'em")
            if self.puls.status is False:
                pulsRepTime = 15
                self.puls.status  = True
                # print("The Engine busted, so now it purrs like a kitten")
            repTime = max(afRepTime,
                              avRepTime,
                              pulsRepTime)
            yield self.env.timeout(repTime)
            self.status = self.af.status & self.av.status & self.puls.status
            self.blueBook = self.blueBook.append({"Aircraft": self.BuNo,
                                                  "FlightHours": fltTime,
                                                  "AC Status": self.status,
                                                  "Airframe Status": self.af.status,
                                                  "Avionics Status": self.av.status,
                                                  "Propulsion Status": self.puls.status,
                                                  "RepairTime": repTime},
                                                 ignore_index=True)
        tmpBB = self.blueBook.copy()
        self.fltHours = tmpBB.dropna(subset=["Flight Date"]).sum().FlightHours
        self.lifeTime = self.af.lifeTime

    def SLEP_Aircraft(self, SLEP_line, SLEP_TTR, SLEP_addition):
        with SLEP_line.request() as req:
            yield req
            print('%d of %d slots are allocated.' % (af_SLEPline.count,
                                                     af_SLEPline.capacity))
            self.af.SLEPtime = SLEP_TTR
            print(self.af.SLEPtime)
            yield self.env.timeout(self.af.SLEPtime)
            # yield SLEP_line.release(self.request)
            print("releasing " + self.BuNo + "at time %d" %self.env.now)
            self.af.SLEP_limit = 20000
            self.af.lifeTime = 150

        

            
    def flyAircraft(self, env, fltTime, stud, inst):
        # Check to see if any of the parts failed in flight
        attrit = .2
        # print("flight time = " +str(fltTime))
        # print("av " + str(self.av.fltFail))
        # print("af " + str(self.af.fltFail))
        # print("puls " + str(self.puls.fltFail))
        fltTime = min(fltTime,
                          self.av.fltFail,
                          self.af.fltFail,
                          self.puls.fltFail)
        # fltTime = fltTime - np.ceil(max(0,
        #                             (fltTime - self.av.fltFail) * 10,
        #                             (fltTime - self.af.fltFail) * 10,
        #                             (fltTime - self.puls.fltFail) * 10)) / 10
        # print("right after" + str(fltTime))
        self.av.failFlight(env, fltTime)
        # histUpdate(self.av)
        self.af.failFlight(env, fltTime)
        self.puls.failFlight(env, fltTime)
        # Update the aircraft status
        self.status = self.af.status & self.av.status & self.puls.status
        # self.blueBook = self.blueBook.append({"Aircraft": self.BuNo,
        #                                       "FlightHours": fltTime,
        #                                       "AC Status": self.status,
        #                                       "Airframe Status": self.af.status,
        #                                       "Avionics Status": self.av.status,
        #                                       "Propulsion Status": self.puls.status,
        #                                       "Flight Date": env.now},
        #                                      ignore_index=True)
        # self.updateBlueBook(env, fltTime)
        # Update aircrew values. For now, updating flight time
        # whether up or down, and not counting a syllabus event if down
        # if self.status:
            # print("Airplane " + str(self.BuNo) + " still worky")
        # else:
            # print("Plane broke dick")
        # print("flight time" + str(fltTime))
        stud.hours += fltTime
        inst.hours = inst.hours + fltTime
        inst.flightLog(env, fltTime, self.BuNo, "NA")
        self.env.process(self.updateBlueBook(env, fltTime))
        # add the event to the student's flight log with the result
        if self.status is True:
            grading(self, env, stud, attrit, fltTime)
            stud.checkAttrite(env)
            stud.checkGraduate(env)
        else:
            stud.flightLog(env, fltTime, self.BuNo, "Incomplete")

   

######################################################################
# People                                                             #
######################################################################


# Class of Aircrew which will be tied to an aircraft for an event.
# Each flight has an instructor and a student. A student is complete.
class Aircrew(object):
    def __init__(self, env, ID):
        self.env = env
        self.ID = ID
        self.hours = 0
        self.dailyFlights = 0
        self.flightDF = pd.DataFrame()

    def flightLog(self, env, fltTime, ac, result):
        # print("Updating Flight Log")
        self.flightDF = self.flightDF.append({"Aircrew ID": self.ID,
                                              "Flight Time": fltTime,
                                              "Aircraft": ac,
                                              # "Result":,
                                              "Takeoff Time": self.env.now,
                                              "Outcome": result},
                                             ignore_index=True)


# A student collects a number ofsyllabus events and graduates
# A student is limited to 2 daily flights
class Student(Aircrew):
    def __init__(self, env, ID):
        self.obj = "Student"
        super().__init__(env, ID)
        self.syllabus = 0
        self.downs = 0
        self.graduated = False
        self.gradDate = None
        self.attrited = False
        self.attritDate = None

    def checkGraduate(self, env):
        if self.syllabus > 15:
            self.graduated = True
            self.gradDate = self.env.now
            # print("I'm going to TOPGUN!!")
            gradStuds.update({self.ID: studList.pop(self.ID)})
            # gradStuds[self.ID] = self.env.now()

    def checkAttrite(self, env):
        if self.downs > 4:
            self.attrited = True
            self.attritDate = self.env.now
            self.graduated = False
            # print("Truck driving school for me.")
            # print("<sad trombone>")
            attritStuds.update({self.ID: studList.pop(self.ID)})




# An instructor cannot instruct until the syllabus is complete
# (qual set to True)
# An instructor is limited to 3 daily flights
class Instructor(Aircrew):
    def __init__(self, env, ID, syl):
        self.obj = "Instructor"
        super().__init__(env, ID)
        self.syllabus = syl
        self.qual = self.syllabus > 9


# Defining the maintainers although this might need to be a resource.
class Maintainer(object):
    def __init__(self, env, ID):
        self.env = env
        self.ID = ID
        self.exp = exp


class AvTech(Maintainer):
    def __init__(self, env, ID, exp):
        super().__init__(env, ID, exp)


class AirFramer(Maintainer):
    def __init__(self, env, ID, exp):
        super().__init__(env, ID, exp)


class AvMech(Maintainer):
    def __init__(self, env, ID, exp):
        super().__init__(env, ID, exp)


######################################################################
# Processes Defined                                                  #
######################################################################

def flight(env, ac, stud, inst):
    if ac.status:
        ft = np.random.randint(low=5, high=20, size=1) / 10
        ft = ft[0]
        ac.flyAircraft(env, ft, stud, inst)
        # print(inst.ID, "and", stud.ID, "tempted death again in aircraft",
        #       ac.BuNo, "at time", env.now, "for", ft, "hours!")
        yield env.timeout(0)
    # else:
        # print("Side number " + str(ac.BuNo) + " is broke, fool!")
    # Hard code three hours to the next event 
    yield env.timeout(0)


def grading(self, env, stud, attrit, fltTime):
    grade = np.random.random(1)
    if (grade > attrit):
        stud.syllabus += 1
        stud.flightLog(env, fltTime, self.BuNo, "Pass")
    else:
        stud.downs += 1
        stud.flightLog(env, fltTime, self.BuNo, "Down")


class Scheduler(object):
    def __init__(self, env, fl, studList, instList, indocPeriod, SLEP_af,
                 SLEP_av, SLEP_puls, SLEPlist):
        self.env = env
        self.studList = studList
        self.nextStudNo = max(self.studList.keys())
        self.flightLine = fl
        self.instList = instList
        self.action = self.env.process(self.dailyFlightSked(SLEP_af))
        self.indocPeriod = indocPeriod
        self.nextIndoc = indocPeriod
        self.eventsPerDay = 4
        self.SLEPlist = SLEPlist

    # Build a class of students to start flight training
    def fltClassIndoc(self, env, minSize, maxSize):
        numClass = np.random.randint(minSize, maxSize)
        # print("Adding " + str(numClass) + " more idiots.")
        self.studList.update({x: Student(env, x) for
                              x in range(self.nextStudNo + 1,
                                         self.nextStudNo + numClass + 1)})
        self.nextStudNo = max(self.studList.keys())

    # Trying to build a calendar so the weekends aren't flown, 
    # def calSked(self):
    #     day = 0
    #     while True:
    #         day += 1
    #         if (day % 7 < 2):
    #             day += 1
    #             break
    def SLEPcheck(self, SLEP_af):
        fl = self.flightLine.copy()
        for num, ac in fl.items():
            if(ac.af.fltHours > ac.af.SLEP_limit):
                print("flightline has %d aircraft; SLEP has %d aircraft" %(len(flightLine),len(SLEPlist)))
                print("Aircraft " + str(ac.BuNo) + " is off to the FST at time " +
                      str(self.env.now))
                self.SLEPlist.update({int(ac.BuNo[2:]):
                                      self.flightLine.pop(int(ac.BuNo[2:]))})
                self.env.process(ac.af.SLEP_Part(env,
                                SLEP_line = af_SLEPline,
                                SLEP_TTR = 50,
                                SLEP_addition = 150))
                print("SLEPline")
                for bn, ac in self.SLEPlist.items():
                    print(bn)
                print("flightline")
                for bn, ac in self.flightLine.items():
                    print(bn)
                with SLEP_af.request() as req:
                    yield req
                    print('%d of %d slots are allocated.' % (af_SLEPline.count, af_SLEPline.capac))
                    ac.af.SLEPtime = 50 #ac.env.now + 50 #SLEP_TTR
                    print(ac.af.SLEPtime)
                    yield ac.env.timeout(ac.af.SLEPtime)
                    # yield SLEP_line.release(self.request)
                    print("releasing " + ac.BuNo + "at time %d" %ac.env.now)
                    print("Aircraft " + str(ac.BuNo) + " is back from SLEP at time " +
                          str(ac.env.now))
                    self.flightLine.update({int(ac.BuNo[2:]):
                                            self.SLEPlist.pop(int(ac.BuNo[2:]))})
                    print('%d of %d slots are allocated.' % (af_SLEPline.count, af_SLEPline.capacity))

    def returnAC(self, env):
        sl = self.SLEPlist.copy()
        for num, SLEPac in sl.items():
            if (SLEPac.af.SLEPtime < self.env.now):
                print("Aircraft " + str(SLEPac.BuNo) + " is back from SLEP at time " +
                      str(self.env.now))
                SLEPac.af.SLEP_limit = 20000
                SLEPac.af.lifeTime = 150
                self.flightLine.update({int(SLEPac.BuNo[2:]):
                                        self.SLEPlist.pop(int(SLEPac.BuNo[2:]))})
                print('%d of %d slots are allocated.' % (af_SLEPline.count, af_SLEPline.capacity))
         

# Goals for this. Pick out a student. Assign an instructor from top of
# the instructor list. Pick an aircraft at random from the aircraft
# list. The aircraft list should contain aircraft where status is True
    def dailyFlightSked(self, SLEP_af):
        i = 0
        while True:
            # numStuds = len(self.studList)
            # numAC = len(self.flightLine)
            # num = numStuds if numStuds <= numAC else numAC
            # this for-loop is intended to get the entire flightLine
            # in the air. We can make it a percentage
            # This is the start of the day
            # self.env.process(self.SLEPcheck(SLEP_af))
            # self.returnAC(self.env)
            availStuds = self.studList.copy()
            availInst = self.instList.copy()
            AClist = self.flightLine.copy()
            availAC = {}
            for num, ac in AClist.iter():
                if ac.SLEP == False:
                    availAC[num] = ac
            # if (len(availStuds) == 0 or
            #     len(availInst) == 0 or
            #     len(availAC) == 0):
            #     yield self.env.timeout(1)
            i += 1
            # print("Ready to fly! for event" + str(i) + " at time " +
                    #  str(self.env.now))
            for flt in range(len(self.flightLine)):
                # if (len(availStuds) > 0 and
                #         len(availInst) > 0 and
                #         len(availAC) > 0):
                if (len(availStuds) == 0):
                    # print("At Time " + str(self.env.now) + "All the students are flying")
                    # yield self.env.timeout(1)
                    break
                elif (len(availInst) == 0):
                    # print("At Time " + str(self.env.now) + "No one is left to teach!")
                    # yield self.env.timeout(1)
                    break
                elif (len(availAC) == 0):
                    # print("At Time " + str(self.env.now) + "Nothing to fly!")
                    # yield self.env.timeout(1)
                    break
                fltStud = availStuds.pop(random.choice(list(availStuds.keys())))
                if (fltStud.graduated == False and
                        fltStud.attrited == False):
                    fltInst = availInst.pop(random.choice(list(availInst.keys())))
                    # acPull = np.random.randint(0, len(flightLine))
                    # ac = self.flightLine[acPull]
                    ## modifying the above acPull to use popitem()
                    # print(availAC)
                    ac = availAC.pop(random.choice(list(availAC.keys())))
                    # print("got ac" + ac.BuNo)
                    # self.acList[np.random.random_integers(0, len(self.acList) - 1)]
                    # print("Stud Vars ")
                    # print(vars(fltStud))
                    # print("Inst Vars ")
                    # print(vars(fltInst))
                    yield self.env.process(flight(self.env,
                                                  ac,
                                                  fltStud,
                                                  fltInst))
                    ## Check to see if the aircraft has used up its lifetime. If it
                    ## has, it is placed in the boneYard list and removed from the
                    ## flightLine list
                    if (ac.lifeTime < ac.fltHours):
                        ac.boneYard = True
                        ac.boneYardTime = ac.env.now
                        # boneYard.update({int(ac.BuNo[2:]):
                        #                  self.flightLine.pop(int(ac.BuNo[2:]))})
                        print("Aircraft " + str(ac.BuNo) + " is off to Davis-Monthan")
                    # self.flightLine.append(ac)
                    # self.studList.extend([fltStud])
                    # print(str(self.studList[0].ID) + str(self.studList[1].ID))
                    # self.instList.extend([fltInst])
                    # print(str(self.instList[0].ID) + str(self.instList[1].ID))
            nextEvent =  3 if int(self.env.now) % 3 == 0 else self.env.now % 3
            # outOfPipeline = len(gradStuds) + len(attritStuds)
            if (len(studList) == 0):
                # print("There is no one left to learn at time " + str(self.env.now))
                yield self.env.timeout(10)
                if (self.env.now > self.nextIndoc):
                    self.fltClassIndoc(env, 3, 10)
                    self.nextIndoc = self.env.now + self.indocPeriod
                    if i > 4:
                        nextEvent = 12
                        # print("Break Time")
                        # Not sure why I started recounting events
                        # i = 0
            
            yield self.env.timeout(nextEvent)
            if len(flightLine) == 0 and len(self.SLEPlist) > 0:
                next_SLEP_complete_dict = {}
                for num, ac in self.SLEPlist.items():
                    next_SLEP_complete_dict[num] = ac.af.SLEPtime
                t = next_SLEP_complete_dict[max(next_SLEP_complete_dict)]    
                yield self.env.timeout(t)
            if len(self.flightLine) == 0 and len(self.SLEPlist) == 0:
                # print(self.env.now)
                break





# Build an aircraft. If it is the start, it will build 
def buildAC(env, numAC, fl):
    for n in range(numAC):
        af = "af" + str(n)
        av = "av" + str(n)
        puls = "puls" + str(n)
        fl[n] = Aircraft(env, af, av, puls)
        


######################################################################
# Useful Functions and Classes                                       #
######################################################################

# This is a catch all function for updating the dataframe storing the
# history of whatever it is, airframe, aircraft, etc. I'm keeping it all
# here so I know where to change it if I need to.
## ***** Right now I just store the vars, but I know that is wrong for
## Stud, inst, and AC ***************
# I think I'm going to build the methods into the classes. So delete this
# if you do that
# def histUpdate(item):
#     if item.obj == "Airframe" or item.obj == "Avionics" or item.obj == "Propulsion":
#         partHistory = partHistory.append(vars(item))
#     elif item.obj == "Student":
#         studHistory = studHistory.append(vars(item))
#     elif item.obj == "Instructor":
#         instHistory = instHistory.append(vars(item))
#     elif item.obj == "Aircraft":
#         aircraftHistory == aircraftHistory.append(vars(item))


######################################################################
# Constants                                                          #
######################################################################

RANDOM_SEED = 42
NUM_AIRCRAFT = 20
NUM_STUDENT = 1
NUM_INSTRUCTOR = 1

######################################################################
# Build Aircraft, Students, and instructors                          #
######################################################################


np.random.seed([RANDOM_SEED])
random.seed(RANDOM_SEED)


# def repair(env, ac)
env = simpy.Environment()
flightLine = {}
boneYard = {}
SLEPlist = {}
buildAC(env, NUM_AIRCRAFT, flightLine)

af_SLEPline = simpy.Resource(env, capacity=4)
av_SLEPline = simpy.Resource(env, capacity=4)
puls_SLEPline = simpy.Resource(env, capacity=4)


studList = {0: Student(env, 0),
            1: Student(env, 1),
            2: Student(env, 2),
            3: Student(env, 3),
            4: Student(env, 4),
            5: Student(env, 5),
            6: Student(env, 6),
            7: Student(env, 7),
            8: Student(env, 8),
            9: Student(env, 9)}
indocPeriod = 300

gradStuds = {}
attritStuds = {}
instList = {0: Instructor(env, 10, 10),
            1: Instructor(env, 11, 10),
            2: Instructor(env, 12, 10),
            3: Instructor(env, 13, 10),
            4: Instructor(env, 14, 10),
            5: Instructor(env, 15, 10),
            6: Instructor(env, 16, 10),
            7: Instructor(env, 17, 10),
            8: Instructor(env, 18, 10),
            9: Instructor(env, 19, 10)}
sked = Scheduler(env,
                 flightLine,
                 studList,
                 instList,
                 indocPeriod,
                 SLEP_af = af_SLEPline,
                 SLEP_av = av_SLEPline,
                 SLEP_puls = puls_SLEPline,
                 SLEPlist = SLEPlist)
env.run(until=5000)

######################################################################
#                    Data Collection                                 #
######################################################################

partHistory = pd.DataFrame()
studHistory = pd.DataFrame()
instHistory = pd.DataFrame()
aircraftHistory = pd.DataFrame()

for stud in studList:
    studHistory = studHistory.append(studList[stud].flightDF)

for grad in gradStuds:
    studHistory = studHistory.append(gradStuds[grad].flightDF)

for flnk in attritStuds:
    studHistory = studHistory.append(attritStuds[flnk].flightDF)

for inst in instList:
    instHistory = instHistory.append(instList[inst].flightDF)

for ac in flightLine:
    aircraftHistory = aircraftHistory.append(flightLine[ac].blueBook)
print(time.process_time())
print(time.perf_counter())
