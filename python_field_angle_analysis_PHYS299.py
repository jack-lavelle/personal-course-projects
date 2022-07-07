import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns1
import statistics
from scipy import interpolate
from matplotlib import rc

cutoff = 300
runs = []
days = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

start = 1
stop = 100

runs = [0] * stop #runs list cannot be empty when assigning values
    
class Run:
    ##perhaps update code to use get-methods rather than repeating.
    def __init__(self, num):
        self.label = "run" + str(num)
        self.path = path + num + ".txt"
        self.sweep = "unknown" ##refers to type of sweep, field 0 or angle 1
        if float(num) >= 222: ##not all data same length
            self.timestamp = pd.read_csv(self.path, sep = '	', skiprows = range(0, cutoff)).values.T[0]
            self.temp = pd.read_csv(self.path, sep = '	', skiprows = range(0, cutoff)).values.T[1]
            self.temp2 = pd.read_csv(self.path, sep = '	', skiprows = range(0, cutoff)).values.T[2]
            self.counter = pd.read_csv(self.path, sep = '	', skiprows = range(0, cutoff)).values.T[3]
            self.field = pd.read_csv(self.path, sep = '	', skiprows = range(0, cutoff)).values.T[4]
            self.angle = pd.read_csv(self.path, sep = '	', skiprows = range(0, cutoff)).values.T[5]
        else: 
            self.timestamp = pd.read_csv(self.path, sep = '	', skiprows = range(0, cutoff)).values.T[0]
            self.temp = pd.read_csv(self.path, sep = '	', skiprows = range(0, cutoff)).values.T[1]
            self.temp2 = [0]
            self.counter = pd.read_csv(self.path, sep = '	', skiprows = range(0, cutoff)).values.T[2]
            self.field = pd.read_csv(self.path, sep = '	', skiprows = range(0, cutoff)).values.T[3]
            self.angle = pd.read_csv(self.path, sep = '	', skiprows = range(0, cutoff)).values.T[4]
            
        self.getday()
        self.chkfldang()
        self.const()
            
    def getday(self):
            f = open(self.path, "r") 
            first = f.readline() #assigns first line to string first (eg Agosta.005.txt Fri, Feb 28, 2020 11:25:21 PM Coniglio\n')
            f.close()
            self.day = first.split()[2] + first.split()[3].split(",")[0] #split takes the string and splits each word into an element of a list (sep = " "). The second term splits the string, takes the third element, splits it again at the comma and takes "28"
            
    def chkfldang(self): #Check Field Angle, checks if the run is a field sweep or angle sweep
        if abs(self.angle[0] - self.angle[-1]) < 0.5:
            self.sweep = "field"

        elif abs(self.field[0] - self.field[-1]) < 0.01:
            self.sweep = "angle"
            
    def const(self): ##if sweep if field sweep then angle is constant (and vice versa) and this method finds that constant with stddev
        if self.sweep == "field":
            self.const = str(statistics.mean(self.angle))
            
        elif self.sweep == "angle":
            self.const = str(statistics.mean(self.field))
        
        elif self.sweep == "unknown":
            self.const = "N/A"
        

for i in range(start, stop): #allows instances of the Run class to be interated through
    if len(pd.read_csv(path + str(str(i).zfill(3)) + ".txt", sep = '	', skiprows = range(0, 7)).values.T[0]) < 385:
        continue
        
    runs[i] = Run(str(i).zfill(3))   

def createlog(start, stop): #this method accumulates all the data into one text file using pandas.
    labels = []
    sweeps = []
    consts = []
    temps = []
    temps2 = []
    dayz = []
    
    for i in range(start, stop):
        if len(pd.read_csv(path + str(str(i).zfill(3)) + ".txt", sep = '	', skiprows = range(0, 7)).values.T[0]) < 385:
            continue
        labels.append(runs[i].label)
        sweeps.append(runs[i].sweep)
        temps.append(round(float(statistics.mean(runs[i].temp)), 3))
        temps2.append(round(float(statistics.mean(runs[i].temp2)), 3))
        dayz.append(runs[i].day)
        if runs[i].const == 'N/A':
            consts.append('N/A')
        else:
            consts.append(round(float(runs[i].const), 3))
        
    d = {'run': labels, 'sweep': sweeps, 'constant field/angle' : consts, 'avgtemp' : temps, 'avgtemp2' : temps2, 'day' : dayz}
    log = pd.DataFrame(data=d)
    
    log.to_csv("log.txt", sep = "\t")

    return log

def rawfldplt(start, stop, step): #raw field plot
    fig, ax = plt.subplots(figsize=(12,8))
    
    for i in range(start, stop, step):
        if runs[i].sweep == "field":
            plt.plot(runs[i].field, runs[i].counter, label = runs[i].label)
        
        
    box = ax.get_position()
    ax.set_position([box.x0, box.y0, box.width * 1, box.height])
    ax.legend(loc='best', ncol = 4, columnspacing = 0.5, fontsize = 12)
    
def findmin(start, stop):
    minval = 0
    for i in range(start, stop):
        
        for x in runs[i].counter:
            if x < minval:
                minval = x
                
    print(minval)
                
def fieldplot(start, stop, step): #cleaner field plot
    while stop - start > 22:
        finalstop = stop
        stop = start + 22
        
        fig, ax = plt.subplots(figsize=(12,8))
        minval = 0
        for i in range(start, stop, step):
            if runs[i].sweep == "field":
                df = pd.DataFrame({"field" : runs[i].field,
                       "counter" : runs[i].counter})
    
                df["counter"] = df["counter"] * -1
    
                runs[i].field = df["field"]
                runs[i].counter = df["counter"]
                
    
                if min(runs[i].counter) < minval:
                    minval = min(runs[i].counter)
                
                runs[i].counter = df["counter"] + abs(minval)
    
                plt.plot(runs[i].field, runs[i].counter, label = runs[i].label)
            
                box = ax.get_position()
                ax.set_position([box.x0, box.y0, box.width * 1, box.height])
                ax.legend(loc='best', ncol = 4, columnspacing = 0.5, fontsize = 12)
        
        start = stop + 2
        stop = finalstop
    
def angleplot(start, stop, step): #cleaner angle plot
    fig, ax = plt.subplots(figsize=(12,8))
    minval = 0
    
    for i in range(start, stop, step):
        if runs[i].sweep == "angle":
            df = pd.DataFrame({"angle" : runs[i].angle,
                   "counter" : runs[i].counter})

            df["counter"] = df["counter"] * -1

            runs[i].field = df["angle"]
            runs[i].counter = df["counter"]
            
            if min(runs[i].counter) < minval:
                minval = min(runs[i].counter)
            
            runs[i].counter = df["counter"] + abs(minval)

            plt.plot(runs[i].angle, runs[i].counter, label = runs[i].label)
            
    box = ax.get_position()
    ax.set_position([box.x0, box.y0, box.width * 1, box.height])
    ax.legend(loc='best', ncol = 4, columnspacing = 0.5, fontsize = 12)