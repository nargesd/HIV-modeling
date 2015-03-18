#immune.treat.interrupt=function(tmax,CD4thresh.l){
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
dev.off()
graphics.off(); #close all graphics windows
library(deSolve)
require(deSolve)  #loads ODE solver package

#Differential equations for uninfected cells, infected cells and virus
#FOr time t=0 to t=600  and after t=750 there is no treatment 


main.program.viral <- function(tmax,int.period){
#values for model parameters,
lambda=10;#unit: [/mm3 day]
d=0.01;#unit: [/day]
b=4.57e-5;#unit: [mm3/day]
delta=0.39;#unit: [/day]
p=45;#unit: [/cell day]
c=2.4;#unit: [/day]
eRTI1=0; #Reverse Transcriptise Inhibitor efficacy when patient is not getting treatment
ePI1=0; #Protease Inhibitor efficacy when patient is not getting treatment
eRTI2=0.4; #Reverse Transcriptise Inhibitor efficacy when patient is getting treatment
ePI2=0.4; #Protease Inhibitor efficacy when patient is getting treatment

no.treatment=function(t,y,parms)   
{ 
  
  U=y[1]; #uninfected cells
  I=y[2]; #infected cells
  VI=y[3]; #infectious virus
  VNI=y[4]; #Noninfectious virus
  
  dUdt=lambda-d*U-(1-eRTI1)*b*VI*U;
  dIdt=(1-eRTI1)*b*VI*U-delta*I;
  dVIdt=(1-ePI1)*p*I-c*VI;
  dVNIdt=ePI1*p*I-c*VNI;
  
  return(list(c(dUdt,dIdt,dVIdt,dVNIdt)))
}


treatment=function(t,y,parms) 
{ 
  U=y[1]; #uninfected cells
  I=y[2]; #infected cells
  VI=y[3]; #infectious virus
  VNI=y[4]; #Noninfectious virus
  
  dUdt=lambda-d*U-(1-eRTI2)*b*VI*U;
  dIdt=(1-eRTI2)*b*VI*U-delta*I;
  dVIdt=(1-ePI2)*p*I-c*VI;
  dVNIdt=ePI2*p*I-c*VNI;
  
  return(list(c(dUdt,dIdt,dVIdt,dVNIdt)))
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


main=matrix(,nrow=tmax+1,ncol=5)
#Initial condition 

main[1,2]=1e3; #initial number of uninfected cells  
main[1,4]=1e-6;#initial number for free infectious virus V
main[1,5]=0; #initial number for free noninfectious virus v
main[1,3]=0; #initial number of infected cells 
#Y0=c(U, I, VI, VNI);  #combine initial conditions into a vector 
milestone=matrix()
t=2
for (n in 1:length(seq.n)){
  print(n)
  while(t<treat.int+(seq.n[n]+2)*int.period & t<tmax){
    print(t)
    if (t<treat.init) {
      if(t==1) {Y0=c(1e3,0,1e-6,0)} else {Y0=main[t-1,2:5]}
      timevec1=seq(t-1,t)
      odeoutput=lsoda(Y0,timevec1,no.treatment,parms="",atol=1e-5)
      main[t,]=odeoutput[2,]
      t=t+1
      while (t<treat.init){
        Y0=main[t-1,2:5]
        timevec1=seq(t-1,t)
        odeoutput=lsoda(Y0,timevec1,no.treatment,parms="",atol=1e-5)
        main[t,]=odeoutput[2,]
        t=t+1}}
      
    else {if ((t>=treat.init) & (t<treat.int)) {
     milestone=rbind(milestone,t)
     Y0=main[t-1,2:5]
     timevec1=seq(t-1,t)
     odeoutput=lsoda(Y0,timevec1,treatment,parms="",atol=1e-5)
     main[t,]=odeoutput[2,]
     t=t+1
     while ((t>=treat.init) & (t<treat.int)){
       Y0=main[t-1,2:5]
       timevec1=seq(t-1,t)
       odeoutput=lsoda(Y0,timevec1,treatment,parms="",atol=1e-5)
       main[t,]=odeoutput[2,]
       t=t+1}}
     
    else {if (t>=treat.int) {
      if (t< treat.int+(seq.n[n]+1)*int.period) {  
        milestone=rbind(milestone,t)
        Y0=main[t-1,2:5]
        timevec1=seq(t-1,t)
        odeoutput=lsoda(Y0,timevec1,no.treatment,parms="",atol=1e-5)
        main[t,]=odeoutput[2,]
        t=t+1
        while (t< treat.int+(seq.n[n]+1)*int.period & t<tmax){
          Y0=main[t-1,2:5]
          timevec1=seq(t-1,t)
          odeoutput=lsoda(Y0,timevec1,no.treatment,parms="",atol=1e-5)
          main[t,]=odeoutput[2,]
          t=t+1}}
      
      else { print("i solve u")
        while ((t>= treat.int+(seq.n[n]+1)*int.period) & (t < treat.int+(seq.n[n]+2)*int.period & t<tmax)){
        print("*")
        print(t)
        Y0=main[t-1,2:5]
        timevec1=seq(t-1,t)
        odeoutput=lsoda(Y0,timevec1,treatment,parms="",atol=1e-5)
        main[t,]=odeoutput[2,]
        t=t+1}}
        }}
      }}
print("out of bond")
}

a.plot <- plot(main[,4]*1000,type="l",xlab="time (days)",ylab="virus per ml",main=paste("Vial load with interruption period:",int.period,"day(s)"), col=int.period+3,lwd=2,xlim=c(0,tmax),log="y")



b.plot <- abline(v=treat.init,lty=c(2))


#c.plot <- abline(v=seq(treat.int,tmax,int.period),lty=c(2))


d.plot <- for (n in 1: length(seq.n)) { 
  rect(treat.int+(seq.n[n])*int.period,1e-20,treat.int+(seq.n[n]+1)*int.period,1e8,col = rgb(0.5,0.5,0.5,1/4),border=NA)}


return(list(a.plot,d.plot))
}





