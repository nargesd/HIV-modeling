
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
  
  if (t<treat.init) {
    dUdt=lambda-d*U-(1-eRTI1)*b*VI*U;
    dIdt=(1-eRTI1)*b*VI*U-delta*I;
    dVIdt=(1-ePI1)*p*I-c*VI;
  }
  else {
    dUdt=lambda-d*U-(1-eRTI2)*b*VI*U;
    dIdt=(1-eRTI2)*b*VI*U-delta*I;
    dVIdt=(1-ePI2)*p*I-c*VI;
  }
 
  return(list(c(dUdt,dIdt,dVIdt)));
  
  
} #end function specifying the ODEs



#############
#main program
#############
tmax=1000;              
timevec=seq(0,tmax,1); #vector of times
treat.init=700
treat.int=850

#values for model parameters,
lambda=10;#unit: [/mm3 day]
d=0.01;#unit: [/day]
b=4.57e-5;#unit: [mm3/day]
delta=0.4;#unit: [/day]
p=45;#unit: [/cell day]
c=2.4;#unit: [/day]
eRTI1=0; #Reverse Transcriptise Inhibitor efficacy when patient is not getting treatment
ePI1=0; #Protease Inhibitor efficacy when patient is not getting treatment
eRTI2=0.4; #Reverse Transcriptise Inhibitor efficacy when patient is getting treatment
ePI2=0.4; #Protease Inhibitor efficacy when patient is getting treatment
hb=1e6
#Initial condition 
U=1000; #initial number of uninfected cells  
VI=1e-6;#initial number for free infectious virus V
I=0; #initial number of infected cells 
Y0=c(U, I, VI);  #combine initial conditions into a vector 


odeoutput1 = lsoda(Y0,timevec,odeequations,parms="",atol=1e-5)



layout(matrix(c(1,2),ncol=2, byrow=T))

#plot results



#Plot uninfected cells with its upper and lower bounds
plot(odeoutput1[,2],type="l",xlab="time (days)",ylab="Cell per microliter",main="Uninfected CD4+ T cells", col="purple",lwd=2,xlim=c(0,tmax),ylim=c(0,1200))
abline(v=treat.init,lty=c(2))

#Plot viral load with its upper and lower bounds
viral.treat=matrix(0,nrow=tmax,ncol=1)
viral.treat
for (i in 0:tmax) {
  viral.treat[i]=max(0,odeoutput1[i,4])
}
plot(viral.treat*1000,type="l",xlab="time (days)",ylab="Virus per ml",main="Viral load",col="red",lwd=2,xlim=c(0,tmax),log="y")
abline(v=treat.init,lty=c(2))

