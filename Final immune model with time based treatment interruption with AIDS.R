#immune.treat.interrupt=function(tmax,CD4thresh.l){
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
dev.off()
graphics.off(); #close all graphics windows
library(deSolve)
require(deSolve)  #loads ODE solver package

#Differential equations for uninfected cells, infected cells and virus
#FOr time t=0 to t=600  and after t=750 there is no treatment 


main.program.AIDS <- function(tmax,int.period){
#values for model parameters,
  
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

no.treatment=function(t,y,parms)   
{ 
  
  U=y[1]; #uninfected cells
  I=y[2]; #infected cells
  M=y[3]
  MI=y[4]
  VI=y[5]; #infectious virus
  
    dUdt=lambda-d*U-(1-eRTI1)*b*VI*U + r*VI*U/(CaT+VI);
    dIdt=(1-eRTI1)*b*VI*U-delta*I;
    dMdt= lambdaM-dM*M-(1-eRTI1)*bM*VI*M + rM*VI*M/(CaM+VI)
    dMIdt=(1-eRTI1)*bM*VI*M-deltaM*MI;
    dVIdt=(1-ePI1)*p*I + (1-ePI1)*pM*MI - c*VI;
  
  return(list(c(dUdt,dIdt,dMdt,dMIdt,dVIdt)))
}


treatment=function(t,y,parms) 
{ 
  U=y[1]; #uninfected cells
  I=y[2]; #infected cells
  M=y[3]
  MI=y[4]
  VI=y[5]; #infectious virus
  
  dUdt=lambda-d*U-(1-eRTI2)*b*VI*U + r*VI*U/(CaT+VI);
  dIdt=(1-eRTI2)*b*VI*U-delta*I;
  dMdt= lambdaM-dM*M-(1-f*eRTI2)*bM*VI*M + rM*VI*M/(CaM+VI)
  dMIdt=(1-f*eRTI2)*bM*VI*M-deltaM*MI;
  dVIdt=(1-ePI2)*p*I + (1-f*ePI2)*pM*MI - c*VI;
  
  return(list(c(dUdt,dIdt,dMdt,dMIdt,dVIdt)))
} #end function specifying the ODEs



#############
#main program
#############
#tmax=2000
timevec=seq(0,tmax,1); #vector of times
treat.init=700
treat.int=800
#int.period=200
n.max=(tmax-treat.int)/int.period
seq.n = seq(0,n.max,2)


main=matrix(,nrow=tmax+1,ncol=6)
#Initial condition 


main[1,2]=1e3; #initial number of uninfected cells  
main[1,3]=0;#initial number for free infectious virus V
main[1,4]=150; #initial number for free noninfectious virus v
main[1,5]=0; #initial number of infected cells 
main[1,6]=1e-2; #initial number of infected cells 
#Y0=c(U, I, VI, VNI);  #combine initial conditions into a vector 
milestone=matrix()
t=2
for (n in 1:length(seq.n)){
  print(n)
  while(t<treat.int+(seq.n[n]+2)*int.period & t<tmax){
    print(t)
    if (t<treat.init) {
      if(t==1) {Y0=c(1e3,0,150,0,1e-2)} else {Y0=main[t-1,2:6]}
      timevec1=seq(t-1,t)
      odeoutput=lsoda(Y0,timevec1,no.treatment,parms="",atol=1e-5)
      main[t,]=odeoutput[2,]
      t=t+1
      while (t<treat.init){
        Y0=main[t-1,2:6]
        timevec1=seq(t-1,t)
        odeoutput=lsoda(Y0,timevec1,no.treatment,parms="",atol=1e-5)
        main[t,]=odeoutput[2,]
        t=t+1}}
      
    else {if ((t>=treat.init) & (t<treat.int)) {
     milestone=rbind(milestone,t)
     Y0=main[t-1,2:6]
     timevec1=seq(t-1,t)
     odeoutput=lsoda(Y0,timevec1,treatment,parms="",atol=1e-5)
     main[t,]=odeoutput[2,]
     t=t+1
     while ((t>=treat.init) & (t<treat.int)){
       Y0=main[t-1,2:6]
       timevec1=seq(t-1,t)
       odeoutput=lsoda(Y0,timevec1,treatment,parms="",atol=1e-5)
       main[t,]=odeoutput[2,]
       t=t+1}}
     
    else {if (t>=treat.int) {
      if (t< treat.int+(seq.n[n]+1)*int.period) {  
        milestone=rbind(milestone,t)
        Y0=main[t-1,2:6]
        timevec1=seq(t-1,t)
        odeoutput=lsoda(Y0,timevec1,no.treatment,parms="",atol=1e-5)
        main[t,]=odeoutput[2,]
        t=t+1
        while (t< treat.int+(seq.n[n]+1)*int.period & t<tmax){
          Y0=main[t-1,2:6]
          timevec1=seq(t-1,t)
          odeoutput=lsoda(Y0,timevec1,no.treatment,parms="",atol=1e-5)
          main[t,]=odeoutput[2,]
          t=t+1}}
      
      else { print("i solve u")
        while ((t>= treat.int+(seq.n[n]+1)*int.period) & (t < treat.int+(seq.n[n]+2)*int.period & t<tmax)){
        print("*")
        print(t)
        Y0=main[t-1,2:6]
        timevec1=seq(t-1,t)
        odeoutput=lsoda(Y0,timevec1,treatment,parms="",atol=1e-5)
        main[t,]=odeoutput[2,]
        t=t+1}}
        }}
      }}
print("out of bond")
}

a.plot <- plot(main[,2],type="l",xlab="time (days)",ylab="Cell per microliter",main=paste("Uninfected cells with interruption period:",int.period,"day(S)") , col=int.period+1,lwd=2,xlim=c(0,tmax),ylim=c(0,1e3))


b.plot <- abline(v=treat.init,lty=c(2))


#c.plot <- abline(v=seq(treat.int,tmax,int.period),lty=c(2))

d.plot <- for (n in 1: length(seq.n)) { 
  rect(treat.int+(seq.n[n])*int.period,-100,treat.int+(seq.n[n]+1)*int.period,1200,col = rgb(0.5,0.5,0.5,1/4),border=NA)}


return(list(a.plot))
}





legend("topright",legend=c("Interruption periods","Upper and lower thresholds (500,700)"),lty=c(NA,3),pch=c(15,NA),col=c("gray","black"),cex=0.7)

plot(main[,6]*1000,type="l",xlab="time (days)",ylab="virus per ml",main="Viral load", col="red",lwd=2,xlim=c(0,tmax),log="y")

abline(v=milestone,lty=c(2))
rect(milestone[3],1e-20,milestone[4],1e8,col = rgb(0.5,0.5,0.5,1/4),border=NA)
rect(milestone[5],1e-20,milestone[6],1e8,col = rgb(0.5,0.5,0.5,1/4),border=NA)

milestone
milestone2=matrix()
for (t in 2:dim(milestone)[1]){
  milestone2[t-1]=milestone[t]-milestone[t-1]
}
milestone2
plot(milestone2,type="l",ylab="duration",xlab="on-off treatment")
points(milestone2,col=rep(1:2,14),pch=15)
ifelse(treat.init<tmax,legend("topright",cex=0.7,legend=c("On-treatment duration","Off-treatment duration"),pch=15,col=c("red","black")),)

barplot(milestone2,type="l",ylab="duration",xlab="on-off treatment")
legend("topright",cex=0.7,legend=c("on-treatment duration","Off-treatment duration"),pch=15,col=c("red","black"))
