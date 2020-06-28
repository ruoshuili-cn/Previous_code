#################################################
# Radar plot
# last edit in 2019.2.1


import numpy as np
from pylab import *
import matplotlib.pyplot as plt

labels=np.array(['$|D_P|$(%)','$|D_A|$(%)','$|D_L|$(%)','$|D_M|$(%)','$|D_F|$(%)'])
datalength=5
data_CO2=np.array([420,1540,1590,87.7,0])
data_SO2=np.array([225,846,849,31,827])
data_water=np.array([226,844,1025,122,280])
# change the data here

angles=np.linspace(0,2*np.pi,datalength,endpoint=False)
data_CO2=np.concatenate((data_CO2,[data_CO2[0]]))
data_SO2=np.concatenate((data_SO2,[data_SO2[0]])) 
data_water=np.concatenate((data_water,[data_water[0]])) 
data_0=np.concatenate((data_0,[data_0[0]])) 
angles=np.concatenate((angles,[angles[0]])) 

fig=figure(figsize=[12,10])
ax=fig.add_subplot(111,polar=True) 
plt.rc('font',family='Times New Roman')
fig.subplots_adjust(left=0.05)
plt.rc('font',family='Times New Roman')
plt.rcParams['savefig.dpi'] = 400 
plt.rcParams['figure.dpi'] = 400
linea,=ax.plot(angles,data_CO2,'o-',linewidth=2,color='#0067A6') 
ax.fill(angles,data_CO2,facecolor='#0067A6',alpha=0.25) 
lineb,=ax.plot(angles,data_SO2,'o-',linewidth=2,color='#DB6968') 
ax.fill(angles,data_SO2,facecolor='#DB6968',alpha=0.25) 
linec,=ax.plot(angles,data_water,'o-',linewidth=2,color='#008972') 
ax.fill(angles,data_water,facecolor='#008972',alpha=0.25) 

ax.set_thetagrids(angles * 180/np.pi, labels, fontproperties="SimHei",fontsize=30)
ax.set_rlim()
ax.grid(True)
legend((linea,lineb,linec,),('$CO_2$ emission','$SO_2$ emission','Water withdrawal'),fontsize=18,loc='center left')
plt.show()

