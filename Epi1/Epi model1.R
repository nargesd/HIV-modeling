rm(list = ls())

library(statnet)
library(ergm)
library(EpiModel)
library(sna)
library(coda)



num.females <- 500
num.males <- 400
nw <- network.initialize(num.females + num.males,bipartite=num.females,directed=F)
nw %v% 'sex' <- c(rep(1,num.females),rep(2,num.males))

deg.dist.males <- c(0.27,
                    0.53,
                    0.17,
                    0.030
                    
)
deg.dist.females <- c(0.256,
                      0.722,
                      0.02,
                      0.002
                      
)

#target stat
tg.edges <- round(sum(num.males*deg.dist.males%*%c(0,1,2,3)),0)
tg.m.0 <- round(num.males*deg.dist.males[1],0)
tg.m.1 <- round(num.males*deg.dist.males[2],0)
tg.f.0 <- round(num.females*deg.dist.females[1],0)
tg.f.1 <- round(num.females*deg.dist.females[2],0)
tg.f.2 <- round(num.females*deg.dist.males[3],0)
tg.f.3 <- round(num.females*deg.dist.males[4],0)
tg.f.4 <- round(num.females*deg.dist.males[5],0)
tg.m.2 <- round(num.females*deg.dist.females[3],0)
tg.m.3 <- round(num.females*deg.dist.females[4],0)
tg.m.4 <- round(num.females*deg.dist.females[5],0)

ergm.deg=c(tg.m.0+tg.f.0,tg.m.1+tg.f.1,tg.m.2+tg.f.2,tg.m.3+tg.f.3) 


target.stats <- c(tg.edges,tg.f.0,tg.m.0)


check_bip_degdist(num.females, num.males, deg.dist.females, deg.dist.males)


ergm.fit <- ergm(nw ~ edges + b1degree(1) + b2degree(1),
                 target.stats=target.stats)


mcmc.diagnostics(ergm.fit)


#Degeneracy is an indicator of a poorly specified model. It is not a property of all ERGMs, but it is associated with
#some dyadic-dependent terms, in particular, the reduced homogenous Markov specifications (e.g., 2-stars and triangle terms).
#For a good technical discussion of unstable terms see Schweinberger 2012. For a discussion of alternative terms that exhibit
#more stable behavior see Snijders et al. 2006. and for the gwesp term (and the curved exponential family terms in general) 
#see Hunter and Handcock 2006.


#Once we have estimated the coefficients of an ERGM, the model is completely specified. It defines a probability distribution
#across all networks of this size. If the model is a good fit to the observed data, then networks drawn from this distribution
#will be more likely to “resemble” the observed data. 

#Simulate 100 cross-sectional network from this model. Confirm that they do indeed display the expected features

netsim <- simulate(ergm.fit,nsim=10)
class(netsim)
summary(netsim)
netsim[[1]]

plot (netsim[[10]],vertex.cex=0.6,vertex.col='sex')

target.stats


#Goodness of fit
#Degree: degree of nodes
#esp: Edgewise share partners
#distance:geodesic distance
ergm.fit.gof <- gof(ergm.fit)
ergm.fit.gof

par(mfrow=c(1,3))
par(oma=c(0.5,2,1,0.5))
plot(gofflo)
plot(ergm.fit.gof)


boxplot(attributes(netsim)$stats)

#look at other features of this network beyond the terms we directly controlled. 
#Quantify reachable path distribution using the command from sna package:

#table(geodist(my.network)$gdist)/2

plot(table(geodist(netsim[[1]])$gdist)/2,ylim=c(0,1000),xlab='distance',ylab='Number of dyads')

#Diagnostic

ergm.sim100 <- simulate(ergm.fit, nsim=100)
ergm.sim100
summary(ergm.sim100)

sim.stats <- attr(ergm.sim100,"stats")
rbind(sim=colMeans(sim.stats), obs=target.stats)

#target.stats diagnosis

matplot(1:nrow(sim.stats), sim.stats, 
        pch=c("e","m","0","+"), cex=.65, 
        main="100 simulations from ego.fit model", sub="(default settings)",
        xlab="Replicate", ylab="frequency")
abline(h=target.stats, col=c(1:4))

sim.stats
 #improvement 

ergm.sim100.2 <- simulate(ergm.fit, nsim=100,
                       control=control.simulate.ergm(MCMC.interval=10000))
sim.stats <- attr(ergm.sim100.2,"stats")
matplot(1:nrow(sim.stats), sim.stats,
        pch=c("e","f1","m1"), cex=.65,
        main="100 simulations from ergm.fit model", sub="(MCMC.interval=10000)",
        xlab="Replicate", ylab="frequency")
abline(h=target.stats, col=c(1:3))
abline(h=0.9*target.stats, col=c(1:3), lty=2)
abline(h=1.1*target.stats, col=c(1:3), lty=2)

#Node degree diagnosis

sim.fulldeg <- summary(ergm.sim100.2 ~ degree(1:6))
#colnames(sim.fulldeg) <- paste("f0","f1","f2","f3","f4","f5"
#,"f6","m0","m1","m2","m3","m4","m5","m6")
sim.fulldeg[1:5,]
rbind(sim=colMeans(sim.fulldeg), obs=)


sim.degb1 <- cbind(sim.fulldeg[,1:3], apply(sim.fulldeg[,4:6],1,sum))
colnames(sim.deg) <- c(colnames(sim.fulldeg)[1:3],"b1degree3+")
#rbind(sim=colMeans(sim.deg), obs=c(tg.f.0,tg.f.1,tg.f.2,tg.f.3))

matplot(1:nrow(sim.degb1), sim.degb1, pch=as.character(0:3), cex=.5,
        main="Comparing ergm.sims to non-targeted degree frequencies",
        sub = "(only total edges targeted)",
        xlab = "Replicate", ylab = "Frequencies")
abline(h=c(tg.f.0,tg.f.1,tg.f.2,tg.f.3), col=c(1:4))


#2

ergm.sim <- simulate(ergm.fit)
par(mar=c(0,0,0,0))
vcols <- ifelse(nw %v% 'sex'==1,'pink2','dodgerblue')
plot(ergm.sim,vertex.cex=0.9,vertex.col=vcols)

#STERGM simulation

dissolution <- ~ offset(edges)
duration <- 30
#This function takes an input the dossolution formulaand average duration
#and transforms it into a logged coefficient for use in estimation of the temporal ERGM.
coef.diss <- dissolution_coefs(dissolution,duration)
coef.diss


coef.form <- ergm.fit$coef
coef.form[1] <- coef.form[1] - coef.diss$coef.adj

sim <- simulate(ergm.sim ,
                formation = ~edges + b1degree(1) + b2degree(1),
                dissolution = ~ offset(edges),
                coef.form = coef.form,
                coef.diss=coef.diss$coef.adj,
                time.slice=10,
                constraints = ~.,
                monitor = ~edges + b1degree(0:5) + b2degree(0:5),
                control = control.simulate.network(MCMC.burnin.min=10000))
sim

#Diagnostic:
est1 <- netest(nw,formation = ~edges + b1degree(1) + b2degree(1),target.stats,
               dissolution = ~ offset(edges),coef.diss)

dx <- netdx(est1,nsims=5,nsteps=500)
dx
plot(dx)


colMeans(attributes(sim)$stats)[c(1,3:4,9:10)]

target.stats

#Static plot

par(mfrow=c(1,1),mar=c(0,0,0,0))
nw.to.plot <- network.extract(sim,at=50)
plot(nw.to.plot,vertex.cex=0.9,vertex.col=vcols)

#Epidemic model
#inf.prob: transmission probability per act per time unit
#act.rate: number of acts per time unit (week)
param <- param.net(trans.rate=0.0007, act.rate=2)

#initial prevalence
init <- init.net(i.num=52,i.num.m2=35)
#visualize in nDTV

#Control
control <-control.net(type="SI",nsims=5,nsteps=500,verbose.init=0)

#epidemic
sim1 <- netsim(est1, param,init,control)
sim1
#network statistics plot
plot(sim1,type="formation")

#Epidemic plots
par(mfrow=c(1,1))
plot(sim1)

plot(sim1,qnts=FALSE,mean.line=FALSE)

#incidence plot
plot(sim1,y='si.flow',sim.lines=FALSE,main='Incidence')
plot(sim1,y='si.flow',sim.lines=FALSE,mean.smooth=TRUE,mean.col='black',add=TRUE)

#NEtwork plots
par(mar=c(0,0,0,0))
plot(sim1,type="network",col.status=TRUE,at=1,sim=1)

par(mar=c(0,0,0,0))
plot(sim1,type="network",col.status=TRUE,at=500,sim=1)


slice.par=list(start = 0, 
               end = 25, 
               interval = 1, 
               aggregate.dur = 1, 
               rule = 'any')

compute.animation(sim, slice.par = slice.par)

render.par = list(tween.frames = 10, 
                  show.time = T, 
                  show.stats = "~b1concurrent+b2concurrent")

cols <- transco(c('firebrick', 'steelblue'), 0.7)
vcols <- ifelse(nw %v% 'sex' == 1, cols[1], cols[2])
render.animation(sim, 
                 render.par = render.par, 
                 vertex.cex = 0.9, 
                 vertex.col = vcols, 
                 edge.col = 'da rkgrey', 
                 vertex.border = 'darkgrey',
                 displaylabels = FALSE)

X11()
ani.replay()
