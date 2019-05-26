import numpy as np
import matplotlib.pyplot as plt

LOGFILE = 'C:\\Users\sim\Desktop\log.csv'

data = np.genfromtxt(LOGFILE, delimiter=',')

t = data[:,0]

plt.subplot(3,1,1)
plt.plot(t,data[:,1])
plt.ylabel("Z''")

plt.subplot(3,1,2)
plt.plot(t, data[:,2])
plt.ylabel("Z'")
plt.ylim(plt.ylim()[::-1])

plt.subplot(3,1,3)
plt.plot(t, data[:,3])
plt.ylabel("Z");
plt.ylim(plt.ylim()[::-1])

plt.show()
