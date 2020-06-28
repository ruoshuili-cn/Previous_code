#############################################
# Visulization of a cascade chart 
# plot 3 subsets in one paragraph and share x axis
# Last edit in 2018.8.9


import numpy as np
from pylab import *
import matplotlib.pyplot as plt

data=np.loadtxt(open('8.9_c.csv'),delimiter=',',skiprows=0)
data_x_c=data[0]
data_s_c=data[1]
data_x_s=data[2]
data_s_s=data[3]
data_x_w=data[4]
data_s_w=data[5]

N=7
ind=np.arange(N)
width=0.55

fig=figure(figsize=[12,10])
plt.rc('font',family='Times New Roman')
plt.rcParams['savefig.dpi'] = 400 
plt.rcParams['figure.dpi'] = 400


ax1=plt.subplot(311)
p1=plt.bar(ind,data_x_c,width,color='#FFFFFF')
Y_0,P,A,L,M,F,Y_T=plt.bar(ind,data_s_c,width,bottom=data_x_c)
Y_0.set_facecolor('#212A3F')
P.set_facecolor('#7BA3A8')
A.set_facecolor('#F4F3DE')
L.set_facecolor('#BEAD92')
M.set_facecolor('#F35A4A')
F.set_facecolor('#5B4947')
Y_T.set_facecolor('#212A3F')
plt.yticks(fontsize=16)
plt.setp(ax1.get_xticklabels(),visible=False)

ax2=plt.subplot(312,sharex=ax1)
p2=plt.bar(ind,data_x_s,width,color='#FFFFFF')
Y_0_1,P_1,A_1,L_1,M_1,F_1,Y_T_1=plt.bar(ind,data_s_s,width,bottom=data_x_s)
Y_0_1.set_facecolor('#212A3F')
P_1.set_facecolor('#7BA3A8')
A_1.set_facecolor('#F4F3DE')
L_1.set_facecolor('#BEAD92')
M_1.set_facecolor('#F35A4A')
F_1.set_facecolor('#5B4947')
Y_T_1.set_facecolor('#212A3F')
plt.yticks(fontsize=16)
ax=plt.gca()
ax.yaxis.get_major_formatter().set_powerlimits((0,1)) 
plt.setp(ax2.get_xticklabels(),visible=False)
plt.ylabel('Impact of each influence factor',fontsize=24)


ax3=plt.subplot(313,sharex=ax1)
p3=plt.bar(ind,data_x_w,width,color='#FFFFFF')
Y_0_2,P_2,A_2,L_2,M_2,F_2,Y_T_2=plt.bar(ind,data_s_w,width,bottom=data_x_w)
Y_0_2.set_facecolor('#212A3F')
P_2.set_facecolor('#7BA3A8')
A_2.set_facecolor('#F4F3DE')
L_2.set_facecolor('#BEAD92')
M_2.set_facecolor('#F35A4A')
F_2.set_facecolor('#5B4947')
Y_T_2.set_facecolor('#212A3F')
plt.yticks(fontsize=16)
plt.xticks(ind,('$R_0$','$\Delta R_P$','$\Delta R_A$','$\Delta R_L$','$\Delta R_M$','$\Delta R_F$','$R_T$'),fontsize=22)

