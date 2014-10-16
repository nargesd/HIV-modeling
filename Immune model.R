
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
dev.off()
graphics.off(); #close all graphics windows
library(deSolve)
require(deSolve)  #loads ODE solver package

#Differential equations for uninfected cells, infected cells and virus
#FOr time t=0 to t=600  and after t=750 there is no treatment 

odeequations=function(t,y,parms) 
{ 
  U=y[1]; #uninfected cells
  I=y[2]; #infected cells
  VI=y[3]; #infectious virus
  VNI=y[4]; #Noninfectious virus
  
  if (t<treat.init | (900<t & t<1100) | (1300<t & t<1500) | (1700<t & t<1900)){
    dUdt=lambda-d*U-(1-eRTI1)*b*VI*U;
    dIdt=(1-eRTI1)*b*VI*U-delta*I;
    dVIdt=(1-ePI1)*p*I-c*VI;
    dVNIdt=ePI1*p*I-c*VNI;
  }
  else {
    dUdt=lambda-d*U-(1-eRTI2)*b*VI*U;
    dIdt=(1-eRTI2)*b*VI*U-delta*I;
    dVIdt=(1-ePI2)*p*I-c*VI;
    dVNIdt=ePI2*p*I-c*VNI;
  }
  
 
  return(list(c(dUdt,dIdt,dVIdt,dVNIdt)));
  
  
} #end function specifying the ODEs



#############
#main program
#############
tmax=2100;              
timevec=seq(0,tmax,1); #vector of times
treat.init=700
treat.int=850

#values for model parameters,
lambda=10000;#unit: [/mm3 day]
d=0.01;#unit: [/day]
b=2.4e-8;#unit: [mm3/day]
delta=0.4;#unit: [/day]
p=390;#unit: [/cell day]
c=13;#unit: [/day]
eRTI1=0; #Reverse Transcriptise Inhibitor efficacy when patient is not getting treatment
ePI1=0; #Protease Inhibitor efficacy when patient is not getting treatment
eRTI2=0.3; #Reverse Transcriptise Inhibitor efficacy when patient is getting treatment
ePI2=0.3; #Protease Inhibitor efficacy when patient is getting treatment

#Initial condition 
U=1e6; #initial number of uninfected cells  
VI=1e-6;#initial number for free infectious virus V
VNI=0; #initial number for free noninfectious virus v
I=0; #initial number of infected cells 
Y0=c(U, I, VI, VNI);  #combine initial conditions into a vector 


  odeoutput1 = lsoda(Y0,timevec,odeequations,parms="",atol=1e-5)



#layout(matrix(c(1,2),ncol=1, byrow=T))

#plot results


#ymin=1e-5; ymax=max(odeoutput3[,4]);

#Plot uninfected cells with its upper and lower bounds
plot(odeoutput1[,1],odeoutput1[,2]/1000,type="l",xlab="time (days)",ylab="Cell per microliter",main="Uninfected cells", col="purple",lwd=2,xlim=c(0,tmax),ylim=c(0,1e3))

#lines(odeoutput2[,2],lty=2,col="purple")
#lines(odeoutput3[,2],lty=3,col="purple")
abline(v=seq(treat.init,tmax,200),lty=c(2))


#Plot infected T cell with its upper and lower bounds
#plot(odeoutput1[,1],odeoutput1[,3],type="l",xlab="time (days)",ylab="Cell per ml",main="Infected cells",col="blue",lwd=2,xlim=c(0,tmax),log="y")
#lines(odeoutput2[,3],lty=2,col="blue")
#lines(odeoutput3[,3],lty=3,col="blue")
#abline(v=seq(treat.init,tmax,100),lty=2)



#Plot viral load with its upper and lower bounds
plot(odeoutput1[,1],odeoutput1[,4]+odeoutput1[,5],type="l",xlab="time (days)",ylab="Virus per ml",main="Viral load",col="red",lwd=2,xlim=c(0,tmax),log="y")
#lines(odeoutput2[,4],lty=2,col="red")
#lines(odeoutput3[,4],lty=3,col="red")
abline(v=seq(treat.init,tmax,200),lty=c(2))

#par(mar=c(0, 0, 0, 0))
#plot.new()

#legend("center", 'groups',inset=0.05,cex=0.8,xpd = TRUE, title="Sensitivity analysis for the CD4+ T cells birth rate (Lambda)",
#    c("Lambda=5.6","Lambda=10","Lambda=36"), lty=c(2,1,3), horiz=TRUE)


