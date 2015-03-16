
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
  

  if (t<treat.init)
  { 
    dUdt=lambda-d*U-(1-eRTI1)*b*VI*U;
    dIdt=(1-eRTI1)*b*VI*U-delta*I;
    dVIdt=(1-ePI1)*p*I-c*VI;
    dVNIdt=ePI1*p*I-c*VNI;
  }
  else if (treat.init < t & t < treat.int){
    dUdt=lambda-d*U-(1-eRTI2)*b*VI*U;
    dIdt=(1-eRTI2)*b*VI*U-delta*I;
    dVIdt=(1-ePI2)*p*I-c*VI;
    dVNIdt=ePI2*p*I-c*VNI;
  }
  else { for (n in 1: length(seq.n)) { 
    print(n)
    
    if (t< treat.int+(seq.n[n]+1)*int.period) {  
    print(t)
    dUdt=lambda-d*U-(1-eRTI1)*b*VI*U;
    dIdt=(1-eRTI1)*b*VI*U-delta*I;
    dVIdt=(1-ePI1)*p*I-c*VI;
    dVNIdt=ePI1*p*I-c*VNI;
    }
    else if (t >= treat.int+(seq.n[n]+1)*int.period & t<treat.int+(seq.n[n]+2)*int.period){
      #print("*")
      #print(t)
    dUdt=lambda-d*U-(1-eRTI2)*b*VI*U;
    dIdt=(1-eRTI2)*b*VI*U-delta*I;
    dVIdt=(1-ePI2)*p*I-c*VI;
    dVNIdt=ePI2*p*I-c*VNI;
  }
  
}}

  return(list(c(dUdt,dIdt,dVIdt,dVNIdt)));
  
  
} #end function specifying the ODEs



#############
#main program
#############
tmax=900;              
timevec=seq(0,tmax,1); #vector of times
treat.init=700
treat.int=800
int.period=100
n.max=(tmax-treat.int)/int.period
seq.n = seq(0,n.max,2)

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

#Initial condition 
U=1e3; #initial number of uninfected cells  
VI=1e-6;#initial number for free infectious virus V
VNI=0; #initial number for free noninfectious virus v
I=0; #initial number of infected cells 
Y0=c(U, I, VI, VNI);  #combine initial conditions into a vector 


  odeoutput1 = lsoda(Y0,timevec,odeequations,parms="",atol=1e-5)



layout(matrix(c(1,2),ncol=2, byrow=T))

#plot results


#ymin=1e-5; ymax=max(odeoutput3[,4]);

#Plot uninfected cells with its upper and lower bounds
plot(odeoutput1[,1],odeoutput1[,2],type="l",xlab="time (days)",ylab="Cell per microliter",main="Uninfected cells", col="purple",lwd=2,xlim=c(0,tmax),ylim=c(0,1e3))

#lines(odeoutput2[,2],lty=2,col="purple")
#lines(odeoutput3[,2],lty=3,col="purple")
abline(v=treat.init,lty=c(2))
abline(v=seq(treat.int,tmax,int.period),lty=c(2))

for (n in 1: length(seq.n)) { 
 rect(treat.int+(seq.n[n])*int.period,-100,treat.int+(seq.n[n]+1)*int.period,1200,col = rgb(0.5,0.5,0.5,1/4),border=NA)
}


#Plot infected T cell with its upper and lower bounds
#plot(odeoutput1[,1],odeoutput1[,3],type="l",xlab="time (days)",ylab="Cell per ml",main="Infected cells",col="blue",lwd=2,xlim=c(0,tmax),log="y")
#lines(odeoutput2[,3],lty=2,col="blue")
#lines(odeoutput3[,3],lty=3,col="blue")
#abline(v=seq(treat.init,tmax,100),lty=2)



#Plot viral load with its upper and lower bounds
plot(odeoutput1[,1],odeoutput1[,4]*1000,type="l",xlab="time (days)",ylab="Virus per ml",main="Viral load",col="red",lwd=2,xlim=c(0,tmax),log="y")
#lines(odeoutput2[,4],lty=2,col="red")
#lines(odeoutput3[,4],lty=3,col="red")
abline(v=treat.init,lty=c(2))
abline(v=seq(800,tmax,30),lty=c(2))

rect(800,1e-20,830,1e8,col = rgb(0.5,0.5,0.5,1/4),border=NA)
rect(860,1e-20,890,1e8,col = rgb(0.5,0.5,0.5,1/4),border=NA)
rect(920,1e-20,950,1e8,col = rgb(0.5,0.5,0.5,1/4),border=NA)

#par(mar=c(0, 0, 0, 0))
#plot.new()

#legend("center", 'groups',inset=0.05,cex=0.8,xpd = TRUE, title="Sensitivity analysis for the CD4+ T cells birth rate (Lambda)",
#    c("Lambda=5.6","Lambda=10","Lambda=36"), lty=c(2,1,3), horiz=TRUE)


