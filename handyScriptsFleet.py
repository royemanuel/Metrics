def sqdnFltHours(acList):
    flthrs = 0
    g = {}
    for key, item in acList.items():
        flthrs += item.af.fltHours
        g[key] = item.af.fltHours
    print(flthrs)
    return({'flightHours':flthrs,
            "EachAC": g})

def instFltHours(il):
    fh = 0
    for key, item in il.items():
        fh += item.hours
    print(fh)
    return(fh)
