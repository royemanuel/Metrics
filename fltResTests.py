for ac in flightLine:
    # print("AC " + str(flightLine[ac].BuNo) + str(flightLine[ac].fltHours))
    # print("AF " + str(flightLine[ac].af.ID) + str(flightLine[ac].af.fltHours))
    # print("AV " + str(flightLine[ac].av.ID) + str(flightLine[ac].av.fltHours))
    # print("PULS " + str(flightLine[ac].puls.ID) + str(flightLine[ac].puls.fltHours))
    print(np.allclose(flightLine[ac].fltHours, flightLine[ac].af.fltHours,
                          flightLine[ac].av.fltHours, flightLine[ac].puls.fltHours))
