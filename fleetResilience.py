######################################################################
## NOTES From 25 MAY.
## in the SLEP portion I have been very sloppy with who "self" refers to
## inspect this carefully. It is important that the aircraft/part is
## the owner of the environment. I am having problems popping aircraft out
## of lists. I am thinking this is linear, and it isn't It may be easier
## to identify the SLEP aircraft and the flying aircraft and the boneyard
## aircraft in separate dictionaries at the beginning of every
## flight event. That should be fairly simple. Then from there use the
##  appropriate list for the flight schedule. The SLEP list should just
## be a holding pen. In fact, SLEP and FLE complete aircraft should just
## be taken off the list before every flight event. When they are good to
## go, their SLEP flag should return to True. That will do some good.
## The first thing to try though is the TTR + env.now. That is wrong for certain
######################################################################


# Simulate the behavior of a fleet of aircraft. Begin by
# importing what we need.
print("starting simulation...")
import time
import os
import simpy
import numpy as np
# import scipy.stats as st
import pandas as pd
# import pdb
import random
tic = time.clock()
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
    def __init__(self, env, ID, SLEP_limit, lifeTime, endTime, repTime):
        self.bornDate = env.now
        self.ID = ID
        self.SLEP_limit = SLEP_limit
        self.lifeTime = lifeTime
        self.endTime = endTime
        self.repTime = repTime
        self.age = 0
        self.fltHours = 0
        self.fltHrsSinceFail = 0
        self.status = True
        self.history = pd.DataFrame()
        self.SLEP = False
        self.SLEPtime = 20000
        
    # Define a fail time for the particular part.
    def failTime(self, env, **kwargs):
        # print("from failTime" + str(kwargs['endTime']))
        time_of_failure = np.ceil(np.random.exponential(kwargs['endTime']))
        # print(self.obj + " will fail at time " + str(time_of_failure))
        return(time_of_failure)
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
            self.age = env.now - self.bornDate
            #??# Commented out data storage sections for easy removal.
            #??#self.history = self.history.append({"ID": self.ID,
            #??#                                    "Age": self.age,
            #??#                                    "FlightHours": self.fltHours,
            #??#                                    "TimeToFail": self.fltFail},
            #??#                                   ignore_index=True)
            # THis section will go with a repair function, but for now
            # I want to check the validity
            # print("from failFlight " + self.obj + str(self.endTime))
            self.fltFail = self.failTime(env, **{"endTime": self.endTime})
            self.fltHrsSinceFail = 0
        else:
            self.fltFail -= fltTime
            self.fltHours += fltTime
            self.age = env.now - self.bornDate
            #??# Commented out data storage sections for easy removal.
            #??#self.history = self.history.append({"ID": self.ID,
            #??#                                    "Age": self.age,
            #??#                                    "FlightHours": self.fltHours,
            #??#                                    "TimeToFail": self.fltFail,
            #??#                                    "fhSinceFail": self.fltHrsSinceFail},
            #??#                                    ignore_index=True)

    # Check if the part needs to go to SLEP. Intend each part to call
    def SLEP_Part(self, env, SLEP_line, SLEP_TTR, SLEP_addition):
        request = SLEP_line.request()
        yield request
        self.SLEPtime = SLEP_TTR
        # print(self.SLEPtime)
        yield env.timeout(self.SLEPtime)
        yield SLEP_line.release(request)




def print_stats(res):
    print('%d of %d slots are allocated.' % (res.count, res.capacity))
    print('  Uses:', res.users)
    print('  Queued events:', res.queue)


# The parts have different types of parts. I think this is the way
# to nest or do the inheritance part of OOP.
# Airframes are the source of the BuNo
class Airframe(Part):
    def __init__(self, env, ID, endTime, repTime):
        self.env = env
        self.obj = "Airframe"
        self.ageFail = 1000
        self.fltFail = 0
        self.fltFail = self.failTime(env, **{"endTime": endTime})
        super().__init__(env,
                         ID,
                         SLEP_limit = 7000,
                         lifeTime = 7200,
                         endTime = endTime,
                         repTime = repTime)
        # This needs to be a call to a method for part. ID the parameters
        # in the particular part
        # self.fltFail = np.random.randint(1, 100 + 1)
        # print("Aircraft " + str(self.ID) + " " + str(self.fltFail))
        

# Avionics and Propusion behave the same. They will just draw from
# different distributions and be party to different updates / Tech
# refreshes.
class Avionics(Part):
    def __init__(self, env, ID, endTime, repTime):
        self.env = env
        self.obj = "Avionics"
        self.fltFail = 0
        super().__init__(env,
                         ID,
                         SLEP_limit = 120,
                         lifeTime = 10000,
                         endTime = endTime,
                         repTime = repTime)
        self.ageFail = 20
        self.fltFail = self.failTime(env, **{"endTime": self.endTime})
        # self.fltFail = np.random.randint(1, 20 + 1)
        # print("Aircraft " + str(self.ID) + " " + str(self.fltFail))


class Propulsion(Part):
    def __init__(self, env, ID, endTime, repTime):
        self.env = env
        self.obj = "Propulsion"
        self.fltFail = 0
        self.fltFail = self.failTime(env, **{"endTime": endTime})
        super().__init__(env,
                         ID,
                         SLEP_limit = 120,
                         lifeTime = 10000,
                         endTime = endTime,
                         repTime = repTime)
        self.ageFail = 30
        self.fltFail = self.failTime(env, **{"endTime": self.endTime})
        # self.fltFail = np.random.randint(1, 30 + 1)
        # print("Aircraft " + str(self.ID) + " " + str(self.fltFail))


class Aircraft(object):
    def __init__(self, env, af, av, puls, attrit, endTime, repTime):
        self.env = env
        self.obj = "Aircraft"
        self.af = Airframe(env, af, endTime['af'], repTime['af'])
        self.av = Avionics(env, av, endTime['av'], repTime['av'])
        self.puls = Propulsion(env, puls, endTime['puls'], repTime['puls'])
        self.BuNo = self.af.ID
        self.status = self.af.status & self.av.status & self.puls.status
        self.bornDate = env.now
        #self.blueBook = pd.DataFrame()
        self.lifeTime = self.af.lifeTime
        self.fltHours = 0
        self.attrit = attrit

    def updateBlueBook(self, env, fltTime):
        # print("Updating Blue Book")
        # self.blueBook = self.blueBook.append({"Aircraft": self.BuNo,
        #                                       "FlightHours": fltTime,
        #                                       "AC Status": self.status,
        #                                       "Airframe ID": self.af.ID,
        #                                       "Airframe Status": self.af.status,
        #                                       "Avionics ID": self.av.ID,
        #                                       "Avionics Status": self.av.status,
        #                                       "Propulsion ID": self.puls.ID,
        #                                       "Propulsion Status": self.puls.status,
        #                                       "SLEP Status": self.af.SLEP,
        #                                       "Flight Date": env.now},
        #                                      ignore_index=True)
        if self.status is False:
            afRepTime = 0
            avRepTime = 0
            pulsRepTime = 0
            if self.af.status is False:
                afRepTime = np.random.randint(1, self.af.repTime)
                # print("The Airframe was broken, but we bent some metal %d" % self.env.now)
                # print("Airframe won't break again until %d." % self.af.fltFail)
            if self.av.status is False:
                avRepTime = np.random.randint(1, self.av.repTime)
                # print("The instruments were down. Up and at'em %d" % self.env.now)
                # print("Avionics won't break again until %d." % self.av.fltFail)
            if self.puls.status is False:
                pulsRepTime = np.random.randint(1, self.puls.repTime)
                # print("The Engine busted, so now it purrs like a kitten %d. " % self.env.now)
                # print("Engine won't break again until" + str(self.puls.fltFail))
            repTime = max(afRepTime,
                              avRepTime,
                              pulsRepTime)
            yield self.env.timeout(repTime)
            self.puls.status  = True
            self.af.status = True
            self.av.status = True
            # print("fixed at %d" % self.env.now)
            self.status = True
            # self.blueBook = self.blueBook.append({"Aircraft": self.BuNo,
            #                                       "FlightHours": fltTime,
            #                                       "AC Status": self.status,
            #                                       "Airframe Status": self.af.status,
            #                                       "Avionics Status": self.av.status,
            #                                       "Propulsion Status": self.puls.status,
            #                                       "RepairTime": repTime},
            #                                      ignore_index=True)
        # tmpBB = self.blueBook.copy()
        # self.fltHours = tmpBB.dropna(subset=["Flight Date"]).sum().FlightHours
        # self.lifeTime = self.af.lifeTime


        

            
    def flyAircraft(self, env, fltTime, stud, inst, day):
        # Check to see if any of the parts failed in flight
        attrit = .02
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
        #??# Commented out data storage sections for easy removal.
        #??# self.blueBook = self.blueBook.append({"Aircraft": self.BuNo,
        #??#                                       "FlightHours": fltTime,
        #??#                                       "AC Status": self.status,
        #??#                                       "Airframe Status": self.af.status,
        #??#                                       "Avionics Status": self.av.status,
        #??#                                       "Propulsion Status": self.puls.status,
        #??#                                       "Flight Date": env.now},
        #??#                                      ignore_index=True)
        #??# self.updateBlueBook(env, fltTime)
        # Update aircrew values. For now, updating flight time
        # whether up or down, and not counting a syllabus event if down
        # if self.status:
            # print("Airplane " + str(self.BuNo) + " still worky")
        # else:
            # print("Plane broke dick")
        # print("flight time" + str(fltTime))
        stud.hours += fltTime
        inst.hours = inst.hours + fltTime
        stud.timeInSquadron = env.now - stud.gainDate
        inst.timeInSquadron = env.now - inst.gainDate
        #??# inst.flightLog(env, fltTime, self.BuNo, "NA", day)
        env.process(self.updateBlueBook(env, fltTime))
        # add the event to the student's flight log with the result
        if self.status is True:
            grading(self, env, stud, self.attrit, fltTime, day)
            stud.checkAttrite(env)
            stud.checkGraduate(env)
            # inst.checkNewOrders(env)
        #??# else:
            #??# stud.flightLog(env, fltTime, self.BuNo, "Incomplete", day)

    # run the slep line check. inputs for the function
    # def SLEP_Part(self, env, SLEP_line, SLEP_TTR, SLEP_addition):
    # I'm hard-coding in the slep-lines because I don't know what else
    # to do right now.                  af_SLEPline,



######################################################################
# People                                                             #
######################################################################


# Class of Aircrew which will be tied to an aircraft for an event.
# Each flight has an instructor and a student. A student is complete.
class Aircrew(object):
    def __init__(self, env, ID, gainDate):
        self.ID = ID
        self.hours = 0
        self.dailyFlights = 0
        # self.flightDF = pd.DataFrame({"Aircrew_ID": [],
        #                                       "Flight_Time": [],
        #                                       "Aircraft": [],
        #                                       # "Result":,
        #                                       "Takeoff_Time": [],
        #                                       "Day": [],
        #                                       "Outcome": []})
        self.gainDate = gainDate
        self.timeInSquadron = 0

    # def flightLog(self, env, fltTime, ac, result, day):
    #     # print("Updating Flight Log")
    #     self.flightDF = self.flightDF.append({"Aircrew_ID": self.ID,
    #                                           "Flight_Time": fltTime,
    #                                           "Aircraft": ac,
    #                                           # "Result":,
    #                                           "Takeoff_Time": env.now,
    #                                           "Day": day,
    #                                           "Outcome": result},
    #                                          ignore_index=True)


# A student collects a number ofsyllabus events and graduates
# A student is limited to 2 daily flights
class Student(Aircrew):
    def __init__(self, env, ID, gainDate):
        self.obj = "Student"
        super().__init__(env, ID, gainDate)
        self.syllabus = 0
        self.downs = 0
        self.graduated = False
        self.gradDate = "learning, yo"
        self.attrited = False

    def checkGraduate(self, env):
        if self.syllabus > 61:
            self.graduated = True
            self.gradDate = env.now
            # print("I'm going to TOPGUN!!")
            gradStuds.update({self.ID: studList.pop(self.ID)})
            # gradStuds[self.ID] = env.now()

    def checkAttrite(self, env):
        if self.downs > 4:
            self.attrited = True
            # print("Truck driving school for me.")
            # print("<sad trombone>")
            attritStuds.update({self.ID: studList.pop(self.ID)})




# An instructor cannot instruct until the syllabus is complete
# (qual set to True)
# An instructor is limited to 3 daily flights
class Instructor(Aircrew):
    def __init__(self, env, ID, syl, gainDate):
        self.obj = "Instructor"
        super().__init__(env, ID, gainDate)
        self.syllabus = syl
        self.qual = self.syllabus > 9
    #    self.action = env.process(self.checkNewOrders(env))


    # def checkNewOrders(self, env):
    #     if self.timeInSquadron > 24 :
    #         inactiveInstList.update({self.ID: instList.pop(self.ID)})



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

def flight(env, ac, stud, inst, day):
    if ac.status:
        ft = np.random.randint(low=5, high=20, size=1) / 10
        ft = ft[0]
        ac.flyAircraft(env, ft, stud, inst, day)
        # print(inst.ID, "and", stud.ID, "tempted death again in aircraft",
        #       ac.BuNo, "at time", env.now, "for", ft, "hours!")
        yield env.timeout(0)
    # else:
        # print("Side number " + str(ac.BuNo) + " is broke, fool!")
    # Hard code three hours to the next event 
    yield env.timeout(0)


def grading(self, env, stud, attrit, fltTime, day):
    grade = np.random.random(1)
    if (grade > attrit):
        stud.syllabus += 1
        #stud.flightLog(env, fltTime, self.BuNo, "Pass", day)
    else:
        stud.downs += 1
        #stud.flightLog(env, fltTime, self.BuNo, "Down", day)


class Scheduler(object):
    def __init__(self, env, fl, studList, instList,
                 indocPeriod, size_of_class,
                 SLEP_af, SLEP_av, SLEP_puls, SLEPlist):
        self.env = env
        self.studList = studList
        self.nextStudNo = str(len(studList)) + "S"
        self.flightLine = fl
        self.instList = instList
        self.env.process(self.dailyFlightSked())
        self.indocPeriod = indocPeriod
        self.nextIndoc = indocPeriod
        self.classSize = size_of_class
        self.eventsPerDay = 4
        self.SLEPlist = SLEPlist
        self.tracker = pd.DataFrame()

    # Build a class of students to start flight training
    def fltClassIndoc(self, env, minSize, maxSize):
        numClass = np.random.randint(minSize, maxSize)
        # print("Adding " + str(numClass) + " more idiots.")
        for newStud in range(numClass):
            studID = int(self.nextStudNo[:-1]) + newStud + 1
            studList[str(studID) + "S"] = Student(env,
                                                  str(studID) +"S",
                                                  env.now)
            self.nextStudNo = str(studID) + "S"
        # self.studList.update({x: Student(env, x, env.now) for
        #                       x in range(self.nextStudNo + 1,
        #                                  self.nextStudNo + numClass + 1)})

    # Trying to build a calendar so the weekends aren't flown, 
    # def calSked(self):
    #     day = 0
    #     while True:
    #         day += 1
    #         if (day % 7 < 2):
    #             day += 1
    #             break
    def SLEPcheck(self, env):
        fl = self.flightLine.copy()
        for num, ac in fl.items():
            if(ac.af.fltHours > ac.af.SLEP_limit):
                self.env.process(ac.af.SLEP_Part(env,
                                SLEP_line = af_SLEPline,
                                SLEP_TTR = 1000,
                                                 SLEP_addition = 9900))
                # print("Aircraft " + str(ac.BuNo) + " is off to the FST")
                self.SLEPlist.update({int(ac.BuNo[2:]):
                                      self.flightLine.pop(int(ac.BuNo[2:]))})
                # print('%d of %d slots are allocated.' % (af_SLEPline.count, af_SLEPline.capacity))

    def returnAC(self, env):
        sl = self.SLEPlist.copy()
        for num, SLEPac in sl.items():
            if (SLEPac.af.SLEPtime < env.now):
                # print("Aircraft " + str(SLEPac.BuNo) + " is back from SLEP")
                SLEPac.af.SLEP_limit = 20000
                SLEPac.af.lifeTime = 19800
                self.flightLine.update({int(SLEPac.BuNo[2:]):
                                        self.SLEPlist.pop(int(SLEPac.BuNo[2:]))})
                # print('%d of %d slots are allocated.' % (af_SLEPline.count, af_SLEPline.capacity))
         

# Goals for this. Pick out a student. Assign an instructor from top of
# the instructor list. Pick an aircraft at random from the aircraft
# list. The aircraft list should contain aircraft where status is True
    def dailyFlightSked(self):
        i = 0
        daytrack = 0
        todaytrack = 0
        flownStuds = {}
        flownInsts = {}
        while True:
            # numStuds = len(self.studList)
            # numAC = len(self.flightLine)
            # num = numStuds if numStuds <= numAC else numAC
            # this for-loop is intended to get the entire flightLine
            # in the air. We can make it a percentage
            # This is the start of the day
            ##################################################
            # At the start of the flight period, we will record the
            # aircraft in the SLEPlist, flightLine, and boneYard
            if (SLEP_in_run):
                self.SLEPcheck(self.env)
                self.returnAC(self.env)
            availStuds = self.studList.copy()
            availInst = self.instList.copy()
            availAC = self.flightLine.copy()
            # Keep track of the current day so we can assess whether
            # student has flown or not
            if todaytrack != daytrack:
                flownStuds = {}
                flownInsts = {}
                todaytrack = daytrack
            # if (len(availStuds) == 0 or
            #     len(availInst) == 0 or
            #     len(availAC) == 0):
            #     yield env.timeout(1)
            i += 1
            # print("Ready to fly! for event" + str(i) + " at time " +
                  # str(env.now))
            for flt in range(len(self.flightLine)):
                # if (len(availStuds) > 0 and
                #         len(availInst) > 0 and
                #         len(availAC) > 0):
                if (len(availStuds) == 0):
                    # print("At Time " + str(env.now) + "All the students are flying")
                    # yield env.timeout(1)
                    break
                elif (len(availInst) == 0):
                    # print("At Time " + str(env.now) + "No one is left to teach!")
                    # yield env.timeout(1)
                    break
                elif (len(availAC) == 0):
                    # print("At Time " + str(env.now) + "Nothing to fly!")
                    # yield env.timeout(1)
                    break
                k = 0
                while k == 0:
                    fltStud = availStuds.pop(random.choice(list(availStuds.keys())))
                    fltStudID = fltStud.ID
                    if fltStudID in flownStuds and flownStuds[fltStudID] < 2:
                        flownStuds[fltStudID] += 1
                        k = 1
                    elif fltStudID not in flownStuds:
                        flownStuds[fltStudID] = 1
                        k = 1
                    elif (len(availStuds) == 0):
                        # print("At Time " + str(env.now) + "All the students are tired")
                        break
                #if len(availStuds) == 0:
                    # print("At time " +str(self.env.now) + "all studs have 2 flights")
                #    break
                if (fltStud.graduated == False and
                        fltStud.attrited == False):
                    # SElect an instructor. Supposed to randomly draw
                    # the instructor list, check if they have more than
                    # four flights for the day. If yes, pick aonother
                    # one until it is empty. If not continue with that instructor
                    j = 0
                    while j == 0:
                        fltInst = availInst.pop(random.choice(list(availInst.keys())))
                        fltInstID = fltInst.ID
                        if fltInstID in flownInsts and flownInsts[fltInstID] < 4:
                            flownInsts[fltInstID] += 1
                            j = 1
                        elif fltInstID not in flownInsts:
                            flownInsts[fltInstID] = 1
                            j = 1
                        elif (len(availInst) == 0):
                            # print("At Time " + str(env.now) + "All the students are tired")
                            break
                        if (len(availInst) == 0):
                            # print("At Time " + str(env.now) + "All the students are tired")
                            break
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
                    # print(str(fltStud.ID) + "instructed by " +
                    #       str(fltInst.ID) + " in " + str(ac.BuNo) +
                    #       " at time " + str(env.now))
                    yield self.env.process(flight(self.env,
                                                  ac,
                                                  fltStud,
                                                  fltInst,
                                                  daytrack))
                    ## Check to see if the aircraft has used up its lifetime. If it
                    ## has, it is placed in the boneYard list and removed from the
                    ## flightLine list
                    if (ac.af.lifeTime < ac.af.fltHours):
                        boneYard.update({int(ac.BuNo[2:]):
                                         self.flightLine.pop(int(ac.BuNo[2:]))})
                        # print("Aircraft " + str(ac.BuNo) + " is off to Davis-Monthan")
                    # self.flightLine.append(ac)
                    # self.studList.extend([fltStud])
                    # print(str(self.studList[0].ID) + str(self.studList[1].ID))
                    # self.instList.extend([fltInst])
                    # print(str(self.instList[0].ID) + str(self.instList[1].ID))
            outOfPipeline = len(gradStuds) + len(attritStuds)
            stat_prv = self.tracker.tail(1)
            upAC = 0
            for key, item in self.flightLine.items():
                if item.status:
                    upAC += 1
            status_now = {'Time':[self.env.now],
                          'Day':[daytrack],
                          'flightLine':[len(self.flightLine)],
                          'upAircraft':[upAC],
                          'SLEPlist':[len(self.SLEPlist)],
                          'boneYard':[len(boneYard)],
                          'students':[len(self.studList)],
                          'graduates':[len(gradStuds)],
                          'instructors':[len(self.instList)],
                          'attrites':[len(attritStuds)]
            }
            status_now = pd.DataFrame(data=status_now)
            self.tracker = self.tracker.append(status_now,
                                               ignore_index = True)
            # This if statement generates new students. I don't think
            # this will do for the long run, and should probably be removed
            # in favor of a periodic introduction of students that is
            # a method or something.
            if (env.now > self.nextIndoc):
                # print("There is no one left to learn at time " + str(env.now))
                self.fltClassIndoc(env,
                                   self.classSize * .7,
                                   self.classSize * 1.3)
                self.nextIndoc = env.now + self.indocPeriod
                # Pretty sure this if statement is in the wrong place
                # if i > 4:
                #    nextEvent = 12
                # print("Break Time")
                # Not sure why I started recounting events
                # i = 0

            # Build the clock for weekends and off hours. We will say that
            # there are 5 events/day. Events occur every 3 hours. We have that already.
            # 0 is Monday... 5 is Saturday, 6 is Sunday.
            time_of_day = self.env.now % 24
            if (time_of_day >= 12 and daytrack % 7 == 4):
                # skip the date to Monday and yield the environment
                # to Monday at time 0
                daytrack += 3
                skiptime = 24 * 7 - self.env.now % (24 * 7)
                yield self.env.timeout(skiptime)
            elif (time_of_day >= 12):
                # yield the environment to the next day
                daytrack += 1
                skiptime = 24 - self.env.now % 24
                yield self.env.timeout(skiptime)
            else:
                nextEvent =  3 if int(env.now) % 3 == 0 else env.now % 3                
                yield self.env.timeout(nextEvent)

            if self.env.now % 100 == 0:
                print("Simulation at time " + str(self.env.now), end='\r')
            if len(flightLine) == 0 and len(self.SLEPlist) > 0:
                next_SLEP_complete_dict = {}
                for num, ac in self.SLEPlist.items():
                    next_SLEP_complete_dict[num] = ac.af.SLEPtime
                t = next_SLEP_complete_dict[max(next_SLEP_complete_dict)]    
                yield self.env.timeout(t)
            if len(self.flightLine) == 0 and len(self.SLEPlist) == 0:
                print(self.env.now)
                break





# Build an aircraft. If it is the start, it will build 
def buildAC(env, numAC, fl, attrit, endTime, repTime):
    for n in range(numAC):
        af = "af" + str(n)
        av = "av" + str(n)
        puls = "puls" + str(n)
        fl[n] = Aircraft(env, af, av, puls, attrit, endTime, repTime)
        

def newStuds(env, listname, numStud):
    for s in range(numStud + len(listname)):
        listname[str(s) + "S"] = Student(env, str(s) + "S", env.now)


def newInsts(env, listname, numInst, syllabus):
    for i in range(numInst + len(listname)):
        listname[str(i) + "I"] = Instructor(env, str(i) + "I", syllabus, env.now )



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
#                    Data Collection                                 #
######################################################################

# partHistory = pd.DataFrame()
# studHistory = pd.DataFrame()
# instHistory = pd.DataFrame()
# aircraftHistory = pd.DataFrame()
# 
# for stud in studList:
#     studHistory = studHistory.append(studList[stud].flightDF)
# 
# for grad in gradStuds:
#     studHistory = studHistory.append(gradStuds[grad].flightDF)
# 
# for flnk in attritStuds:
#     studHistory = studHistory.append(attritStuds[flnk].flightDF)
# 
# for inst in instList:
#     instHistory = instHistory.append(instList[inst].flightDF)
# 
# for ac in flightLine:
#     aircraftHistory = aircraftHistory.append(flightLine[ac].blueBook)

timeNow = "fleetData" + time.strftime("%Y%m%d-%H%M%S")
os.makedirs(timeNow)
os.path.join(timeNow +'/')


def allBB(sim_run, acDict):
    acStor = []
    for dictName, dictObj in acDict.items():
        for num, ac in dictObj.items():
            acHist = pd.concat([ac.af.history, ac.av.history, ac.puls.history],
                               ignore_index = True)
            acStor.append(acHist)
            print(acStor[-1])
    allacHist = pd.concat(acStor)
    filename = timeNow + '/AC' + str(num) + 'history' + str(sim_run) + '.csv'
    allacHist.to_csv(filename)

def concBB(sim_run, acDict, sr):
    df = []
    for num, ac in acDict.items():
        df.append(ac.blueBook)
    if len(df) > 0:
        dfcsv = pd.concat(df)
        filename = timeNow + '/ALL' + sr + 'run' + str(sim_run) + '.csv'
        dfcsv.to_csv(filename)
        return(dfcsv)

def buildFiles(sim_run, acListDict, st, gr, at):
    fn = timeNow + '/ALL' + 'run' + str(sim_run) + '.csv'
    fnst = timeNow + '/skedTracker' + 'run' + str(sim_run) + '.csv'
    sked.tracker.to_csv(fnst)
    allBB(sim_run, acListDict)
    aircrewInfo = studInfo(sim_run, st, gr, at)
    aircrewInfo.to_csv(timeNow + '/aicrewRun' +str(sim_run) +'.csv')
    aircraftInfo = acInfo(sim_run, acListDict)
    aircraftInfo.to_csv(timeNow +'/ACRun' + str(sim_run) + '.csv')
    

def studInfo(sim_run, studs, grads, attrits):
    sgaDF = pd.DataFrame({'Run': [],
                             'ID' :  [],
                             'Disp': [],
                             'TimeInSqdn': []})
    if len(studs) > 0:
        for sname, sobj in studs.items():
            stud = pd.DataFrame({'Run': [sim_run],
                                 'ID': [sobj.ID],
                                 'Disp': ['S'],
                                 'TimeInSqdn': [sobj.timeInSquadron]})
            sgaDF = sgaDF.append(stud)
    if len(grads) > 0:
        for gname, gobj in grads.items():
            grad = pd.DataFrame({'Run': [sim_run],
                                 'ID': [gobj.ID],
                                 'Disp': ['G'],
                                 'TimeInSqdn': [gobj.timeInSquadron]})
            sgaDF = sgaDF.append(grad)
    if len(attrits) > 0:
        for aname, aobj in attrits.items():
            attrit = pd.DataFrame({'Run': [sim_run],
                                   'ID': [aobj.ID],
                                   'Disp': ['A'],
                                   'TimeInSqdn': [aobj.timeInSquadron]})
            sgaDF = sgaDF.append(attrit)
    return(sgaDF)
    
def acInfo(sim_run, acLists):
    acDF = pd.DataFrame({'Run': [],
                             'ID' :  [],
                             'Disp': [],
                             'FlightHours': []})
    for acListName, acList in acLists.items():
        if len(acList) > 0:
            for fname, fobj in acList.items():
                ac = pd.DataFrame({'Run': [sim_run],
                                   'ID': [fobj.BuNo],
                                   'Disp': [acListName],
                                   'FlightHours': [fobj.af.fltHours]})
                acDF = acDF.append(ac)
    return(acDF)

        
            

# allBB(boneYard)
# allBB(flightLine)
# allBB(SLEPlist)
# 
# concBB(boneYard, 'BY')
# concBB(flightLine, 'FL')
# g = concBB(SLEPlist, 'SLEP')

# buildFiles({'BY' : boneYard,
#            'FL' : flightLine,
#             'SLEP' : SLEPlist})


######################################################################
# Constants                                                          #
######################################################################
def calcScale(Mode, Mean):
    Scale = (np.log(Mode) +2 *np.log(Mean))/3
    return Scale

def calcShape(Mode, Mean):
    Shape = np.sqrt((2/3)*np.log(Mean)-(2/3)*np.log(Mode))
    return Shape


NUM_AIRCRAFT =   [10, 90, 80, 70, 60, 50,
                  100, 90, 80, 70, 60, 50]#    [15, 30, 80]
NUM_STUDENT =    [100, 100, 100, 100, 100, 100,
                  100, 100, 100, 100, 100, 100]#    [20, 30, 50]
NUM_INSTRUCTOR = [80, 80, 80, 80, 80, 80,
                  80, 80, 80, 80, 80, 80] #    [15, 25, 50]
s_o_c =          [25, 25, 25, 25, 25, 25,
                  25, 25, 25, 25, 25, 25]
rl =             [42, 42, 23, 23, 122809, 122809,
                  42, 42, 23, 23, 122809, 122809] 
ip =             [720, 720, 720, 720, 720, 720,
                  720, 720, 720, 720, 720, 720]   
attrit =         [.035, .035, .035, .035, .035, .035,
                  .035, .035, .035, .035, .035, .035,]
time_line =      [5*24*365,
                  50*24*365,
                  50*24*365,
                  50*24*365,
                  50*24*365,
                  50*24*365,
                  50*24*365,
                  50*24*365,
                  50*24*365,
                  50*24*365,
                  50*24*365,
                  50 *24*365]
et = {'af':720,
      'av':240,
      'puls':360}
rt = {'af':24,
      'av':6,
      'puls':6}
SLEP_in_run = [False, False, False, False, False, False,
               True, True, True, True, True, True]      
SLEPspots =      [4, 8, 4, 8, 4, 8,
                  4, 8, 4, 8, 4, 8]

######################################################################
# Build Aircraft, Students, and instructors                          #
######################################################################

for r in range(len(rl)):
    tic_run = time.clock()
    np.random.seed([rl[r]])
    random.seed(rl[r])
    # Make all variables None to start it out
    env = None
    flightLine = None
    boneYard = None
    SLEPlist = None
    af_SLEPline = None
    av_SLEPline = None
    puls_SLEPline = None
    indocPeriod = None
    studList = None
    gradStuds = None
    attritStuds = None
    instList = None
    inactiveInstList = None
    sked = None
    # Build everything again
    env = simpy.Environment()
    flightLine = {}
    boneYard = {}
    SLEPlist = {}
    buildAC(env, NUM_AIRCRAFT[r], flightLine, attrit[r], et, rt)
    af_SLEPline = simpy.Resource(env, capacity=SLEPspots[r])
    av_SLEPline = simpy.Resource(env, capacity=SLEPspots[r])
    puls_SLEPline = simpy.Resource(env, capacity=SLEPspots[r])
    ac_status_history = []
    indocPeriod = ip[r]
    studList = {}
    newStuds(env, studList, NUM_STUDENT[r])
    gradStuds = {}
    attritStuds = {}
    inactiveInstList = {}
    instList = {}
    newInsts(env, instList, NUM_INSTRUCTOR[r], 0)
    sked = Scheduler(env,
                     flightLine,
                     studList,
                     instList,
                     indocPeriod,
                     s_o_c[r],
                     SLEP_af = af_SLEPline,
                     SLEP_av = av_SLEPline,
                     SLEP_puls = puls_SLEPline,
                     SLEPlist = SLEPlist)
    print(time_line[r])
    env.run(until=time_line[r])
    current_run = r + 1
    print(current_run)
    buildFiles(current_run,
               {'BY' : boneYard,
                'FL' : flightLine,
                'SLEP' : SLEPlist},
               studList, gradStuds, attritStuds)
    toc_run = time.clock()    
    print('Completed Run ' + str(r + 1) + ' of ' + str(len(rl)) +
          ' in ' + str(toc_run - tic_run) + ' seconds.')

toc = time.clock()    
print('Completed all runs in ' + str(toc - tic) + ' seconds.')



        
      

    


