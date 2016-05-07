# This is the true start of my dissertation
import numpy as np
import pandas as pd
from scipy.stats import uniform

# We will start with much of the work we have done in chartbuilder.py
# but try to be as open as we can to including the new metrics easily
# The file is pretty slow right now, so there may be a faster way to
# do this.


# Build a time range
timeH = 50
resolution = 0.01
timeH = np.arange(0, timeH, resolution)
chartArray = pd.DataFrame({'Time': timeH})


# Build the performance curve and append to the time
def buildPerf(resArray, pFunc, *args):
    return resArray['Time'].apply(lambda x: pFunc(x, *args))
