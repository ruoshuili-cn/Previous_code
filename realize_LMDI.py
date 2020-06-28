################################################
# Code for implementing LMDI model to decompose CO2, SO2, water withdrawal
# First edit in 2018.6.15
#

import numpy as np
from pylab import *
import matplotlib.pyplot as plt
from scipy import interpolate

data_c=np.loadtxt(open('/water_with.csv'),delimiter=',',skiprows=0)
## the data to be decomposed
data_0=np.loadtxt(open('/decompose_with.csv'),delimiter=',',skiprows=0)
## decompose to these several part(GDP,Population...)
data_year=np.loadtxt(open('/year.csv'),delimiter=',',skiprows=0)
## the data of year/time when decomposing


year=12 ## how many years are studied
year_s=year-1 ## how many years of rate of change can be studies

Year=[]
for em in data_year:
	Year.append(str(int(em)))


################ MAIN PART######################
def LMDI(C0,C1,I0,I1):
	return ((C1-C0)/np.log(C1/C0))*np.log(I1/I0)

def Contribution(I0,I1,pt):
	return ((I1-I0)/I0)*pt

## add '%' to the percent value on y axis
def str1(a):
    strii=[]
    for ai in a:
        temp=str(ai)
        strii.append(temp+'%')
    return np.array(strii)

## Some data treatment before decompose
def adjust_1(data_c,data_0,data_year):
	c=data_c[0]
	p=data_0[0,0]
	g=data_0[1,0]
	l=data_0[2,0]
	o=data_0[3,0]
	e=data_0[4,0]
	## c for total, p/g/l for three decomposotion parameter
	c0=np.zeros([year_s])
	p0=np.zeros([year_s])
	g0=np.zeros([year_s])
	l0=np.zeros([year_s])
	o0=np.zeros([year_s])
	e0=np.zeros([year_s])
	c0[0:year_s]=c
	p0[0:year_s]=p
	g0[0:year_s]=g
	l0[0:year_s]=l
	o0[0:year_s]=o
	e0[0:year_s]=e

	c1=data_c[1:year]
	p1=data_0[0,1:year]
	g1=data_0[1,1:year]
	l1=data_0[2,1:year]
	o1=data_0[3,1:year]
	e1=data_0[4,1:year]

	dC=c1-c0
	dP=LMDI(c0,c1,p0,p1)
	dG=LMDI(c0,c1,g0,g1)
	dL=LMDI(c0,c1,l0,l1)
	dO=LMDI(c0,c1,o0,o1)
	dE=LMDI(c0,c1,e0,e1)
	test_1=dP+dG+dL+dE+dO
	## Set for test(total decompose==sum of depart decompose)
	for_sum=[]
	for_sum.append(dP)
	for_sum.append(dG)
	for_sum.append(dL)
	for_sum.append(dO)
	for_sum.append(dE)

	return(dP,dG,dL,dE,dO,c1,c0,test_1,for_sum)


## Some data treatment before calculate contribution
def adjust_2(dP,dG,dL,dE,dO,c1,c0):
	dC=c1-c0
	pt_p=dP/dC
	pt_g=dG/dC
	pt_l=dL/dC
	pt_o=dO/dC
	pt_e=dE/dC
	test_2=pt_p+pt_g+pt_l+pt_o+pt_e
	## Set for test(test_2 should be 1)

	bC=(c1-c0)/c0
	bP=Contribution(c0,c1,pt_p)
	bG=Contribution(c0,c1,pt_g)
	bL=Contribution(c0,c1,pt_l)
	bO=Contribution(c0,c1,pt_o)
	bE=Contribution(c0,c1,pt_e)

	return(bP,bG,bL,bE,bO,test_2)


def adjust_3(bP,bG,bL,bE,bO):
	bc=np.zeros([year])
	bp=np.zeros([year])
	bg=np.zeros([year])
	bl=np.zeros([year])
	bo=np.zeros([year])
	be=np.zeros([year])
	bC=(c1-c0)/c0
	bc[1:year]=bC
	bp[1:year]=bP
	bg[1:year]=bG
	bl[1:year]=bL
	bo[1:year]=bO
	be[1:year]=bE
	zong=[]
	zong.append(bp)
	zong.append(bg)
	zong.append(bl)
	zong.append(bo)
	zong.append(be)
	return(bc,bp,bg,bl,bo,be,zong)

def figure_1(c0,c1,dP,dG,dL,dE,dO):
	fig_1=figure(figsize=[10,6])
	fig_1.subplots_adjust(bottom=0.2)
	plt.rc('font',family='Times New Roman')
	plt.rcParams['savefig.dpi'] = 400 
	plt.rcParams['figure.dpi'] = 400
	plt.title('Driving forces of thermoelectric $SO_2$ emission')
	plt.title('Driving forces of thermoelectric water consumption')
	plt.xticks(arange(year_s),['1997-2000','2000-2002','2002-2004','2004-2005','2005-2006','2006-2007','2007-2008','2008-2009','2009-2010','2010-2011','2011-2012','2012-2015'],rotation=45)

	#dE=np.zeros([year_s])
	dC=c1-c0
	plot(Year[1:],dC)
	plot(Year[1:],dP)
	plot(Year[1:],dG)
	plot(Year[1:],dL)
	plot(Year[1:],dO)
	plot(Year[1:],dE)
	linea,=plt.plot(Year[1:],dP,'^-',color='#EEAEEE')
	lineb,=plt.plot(Year[1:],dG,'*-',color='#EEC591')
	linec,=plt.plot(Year[1:],dL,'s-',color='#9BCD9B')
	lined,=plt.plot(Year[1:],dO,'+-',color='#DE9DD6')
	linee,=plt.plot(Year[1:],dE,'d-',color='#9FE0F6')
	linef,=plt.plot(Year[1:],dC,'o-',color='#FF3030')

	xlabel('year',fontsize=15)
	ylabel('water withdrawal change($t$)',fontsize=18)
	legend((linea, lineb, linec,lined,linee,linef,), ('$R_P$','$R_A$','$R_L$','$R_M$','$R_F$','Total withdrawal'),fontsize=14,frameon=False)
	return(fig_1)


## radar plot
def figure_2(zong):
	d=[]
	for em in zong:
		d.append(sum(em))
	labels=np.array(['Population Growth effect(%)','Economic Growth effect(%)','Energy Intensity effect(%)','Energy Fix effect(%)','Emission Coefficient effect(%)'])
	datalength=5
	data_t=np.array(d)
	data=abs(data_t*100)

	angles=np.linspace(0,2*np.pi,datalength,endpoint=False)
	data=np.concatenate((data,[data[0]])) 
	angles=np.concatenate((angles,[angles[0]])) 

	fig=figure(figsize=[8,8])
	ax=fig.add_subplot(111,polar=True) 
	ax.plot(angles,data,'bo-',linewidth=2) 
	ax.fill(angles,data,facecolor='r',alpha=0.25) 
	ax.set_thetagrids(angles * 180/np.pi, labels, fontproperties="SimHei")
	ax.set_title("matplotlib-radar", va='bottom', fontproperties="SimHei")
	ax.set_rlim()
	ax.grid(True)
	plt.show()

	return(fig)

###################################################################

dP,dG,dL,dO,dE,c1,c0,test_1,for_sum=adjust_1(data_c,data_0,data_year)
bP,bG,bL,bO,bE,test_2=adjust_2(dP,dG,dL,dO,dE,c1,c0)
bc,bp,bg,bl,bo,be,zong=adjust_3(bP,bG,bL,bO,bE)

np.savetxt('contribution_c_w.csv',zong,delimiter=',')
np.savetxt('for_sum.csv',for_sum,delimiter=',')

figure_decompose=plt.figure()
figure_radar=plt.figure()

figure_decompose=figure_1(c0,c1,dP,dG,dL,dO,dE)
figure_radar=figure_2(zong)
plt.show()












