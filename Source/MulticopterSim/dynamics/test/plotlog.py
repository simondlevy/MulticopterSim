import numpy as np
import matplotlib.pyplot as plt

LOGFILE = 'C:\\Users\sim\Desktop\log.csv'

data = np.genfromtxt(LOGFILE, delimiter=',')

labels = ['acc', 'vel', 'pos']

for k in range(3):

    plt.subplot(3,1,k+1)
    plt.plot(data[:,0],data[:,k+1])
    plt.ylim(plt.ylim()[::-1])
    plt.ylabel(labels[k])

plt.show()
