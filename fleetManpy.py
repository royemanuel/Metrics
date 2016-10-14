from dream.simulation.imports import Source, Queue, Machine, Exit
from dream.simulation.Globals import runSimulation

# This is the baby step to building a complicated model of the behavior
# of a fleet of systems with multiple stakeholders.
# The baby step includes:
#     A source to generate students
#     A Queue for students to wait for a flight
#     A machine (aircraft) to give students time
#     An exit for graduated students

# The source is API for Aviation Preflight Indocrination
API = Source('API', 'Source', interArrivalTime={'Fixed': {'mean': 0.5}},
             entity='Dream.Part')
RR = Queue('ReadyRoom', 'Queue', capacity=1)
AC = Machine('AC1', 'Machine', processingTime={'Fixed': {'mean': 0.25}})
E = Exit('The Fleet', 'The Fleet')

# The predecessors and successors for the objects
API.defineRouting(successorList=[RR])
RR.defineRouting(predecessorList=[API], successorList=[AC])
AC.defineRouting(predecessorList=[RR], successorList=[E])
E.defineRouting(predecessorList=[AC])

def main(test=0):
    # add all the objects in a list
    objectList=[API, RR, AC, E]
    # set the length of the experiment
    maxSimTime = 1440.0
    # call the runSimulation giving the objects and the length of the
    # experiment
    runSimulation(objectList, maxSimTime)

    # calculate metrics
    working_ratio = (AC.totalWorkingTime/maxSimTime) * 100

    # return results for the test
    if test:
        return {"parts": E.numOfExits,
                "working_ratio": working_ratio}

    # print the results
    print "the pipeline produced", E.numOfExits, "winged aviators"
    print "the total flying ratio of the Aircraft is", working_ratio

if __name__ == '__main__':
    main()
