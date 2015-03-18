
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
  M=y[3]
  MI=y[4]
  VI=y[5]; #infectious virus
  
  if (t<treat.init) {
    dUdt=lambda-d*U-(1-eRTI1)*b*VI*U + r*VI*U/(CaT+VI);
    dIdt=(1-eRTI1)*b*VI*U-delta*I;
    dMdt= lambdaM-dM*M-(1-eRTI1)*bM*VI*M + rM*VI*M/(CaM+VI)
    dMIdt=(1-eRTI1)*bM*VI*M-deltaM*MI;
    dVIdt=(1-ePI1)*p*I + (1-ePI1)*pM*MI - c*VI;
  }
  else {
    dUdt=lambda-d*U-(1-eRTI2)*b*VI*U + r*VI*U/(CaT+VI);
    dIdt=(1-eRTI2)*b*VI*U-delta*I;
    dMdt= lambdaM-dM*M-(1-f*eRTI2)*bM*VI*M + rM*VI*M/(CaM+VI)
    dMIdt=(1-f*eRTI2)*bM*VI*M-deltaM*MI;
    dVIdt=(1-ePI2)*p*I + (1-f*ePI2)*pM*MI - c*VI;
  }
 
  return(list(c(dUdt,dIdt,dMdt,dMIdt,dVIdt)));
  
  
} #end function specifying the ODEs



#############
#main program
#############
tmax=5000;              
timevec=seq(0,tmax,1); #vector of times
treat.init=6000
treat.int=6000

#values for model parameters,
lambda=10;#unit: [/mm3 day]
d=0.01;#unit: [/day]
b=4.57e-5;#unit: [mm3/day]
delta=0.4;#unit: [/day]
p=38;#unit: [/cell day]
c=2.4;#unit: [/day]
r=0.01
CaT=  300

lambdaM=0.15
dM=1e-3
bM=4.33e-8
deltaM=1e-3
pM=35
rM=0.003
CaM=  220
eRTI1=0; #Reverse Transcriptise Inhibitor efficacy when patient is not getting treatment
ePI1=0; #Protease Inhibitor efficacy when patient is not getting treatment
eRTI2=0.4; #Reverse Transcriptise Inhibitor efficacy when patient is getting treatment
ePI2=0.4; #Protease Inhibitor efficacy when patient is getting treatment
f=0.34
#Initial condition 
U=1000; #initial number of uninfected cells  
VI=1e-2;#initial number for free infectious virus V
I=0; #initial number of infected cells 
M=150
MI=0
Y0=c(U, I, M, MI, VI);  #combine initial conditions into a vector 


odeoutput1 = lsoda(Y0,timevec,odeequations,parms="",atol=1e-5)





layout(matrix(c(1,2,3),ncol=3, byrow=T))

#plot results



#Plot uninfected cells with its upper and lower bounds
plot(odeoutput1[,2],type="l",xlab="time (days)",ylab="Cell per microliter",main="Uninfected CD4+ T cells", col="green",lwd=2,xlim=c(0,tmax),ylim=c(0,1200))
abline(v=treat.init,lty=c(2))

fee=seq(0.2, 0.6, by=0.1)
for (j in 1:length(fee)) {
  eRTI2=fee[j] ; #Reverse Transcriptise Inhibitor efficacy when patient is getting treatment
  ePI2= fee[j] ; #Protease Inhibitor efficacy when patient is getting treatment
  testode=lsoda(Y0,timevec,odeequations,parms="",atol=1e-5)
  lines(testode[,2],type="l",xlab="time (days)",ylab="Cell per microliter",main="Uninfected CD4+ T cells", col=j+1,lwd=1,xlim=c(0,tmax),ylim=c(0,1200))
  
}

plot(odeoutput1[,4],type="l",xlab="time (days)",ylab="Cell per microliter",main="Uninfected Macrophages", col="yellow",lwd=2,xlim=c(0,tmax),ylim=c(0,14000))
fee=seq(0.2, 0.6, by=0.1)
for (j in 1:length(fee)) {
  eRTI2=fee[j] ; #Reverse Transcriptise Inhibitor efficacy when patient is getting treatment
  ePI2= fee[j] ; #Protease Inhibitor efficacy when patient is getting treatment
  testode=lsoda(Y0,timevec,odeequations,parms="",atol=1e-5)
  lines(testode[,4],type="l",xlab="time (days)",ylab="Cell per microliter",main="Uninfected CD4+ T cells", col=j+1,lwd=1,xlim=c(0,tmax),ylim=c(0,1200))
  
}


#Plot viral load with its upper and lower bounds
viral.treat=matrix(0,nrow=tmax,ncol=1)
viral.treat
for (i in 0:tmax) {
  viral.treat[i]=max(0,odeoutput1[i,6])
}
plot(viral.treat*1000,type="l",xlab="time (days)",ylab="Virus per ml",main="Viral load",col="red",lwd=2,xlim=c(0,tmax),log="y")
abline(v=treat.init,lty=c(2))

fee=seq(0.2, 0.6, by=0.1)
for (j in 1:length(fee)) {
  eRTI2=fee[j] ; #Reverse Transcriptise Inhibitor efficacy when patient is getting treatment
  ePI2= fee[j] ; #Protease Inhibitor efficacy when patient is getting treatment
  testode=lsoda(Y0,timevec,odeequations,parms="",atol=1e-5)
  
  viral.treat=matrix(0,nrow=tmax,ncol=1)
  viral.treat
  for (i in 0:tmax) {
    viral.treat[i]=max(0,testode[i,6])
  }
  lines(viral.treat*1000,type="l",xlab="time (days)",ylab="Virus per ml",main="Viral load",col=j+1,lwd=1,xlim=c(0,tmax),log="y")
  
}
legend("bottomright", cex=0.8, c("0.2","0.3","0.4","0.5","0.6") , lwd=1, col=c(2,3,4,5,6), title="Drug efficacy")
