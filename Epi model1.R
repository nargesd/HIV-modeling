#Reproducing the sexual contact network of a sample in Uganda.

rm(list=ls()) 
graphics.off()

#Packages that we need:
require('statnet')
require('coda')


#Creating the empty netwrok with 500 male and 400 female.
ego.net <- network.initialize(900,directed=F)
ego.net %v% 'sex' <- c(rep(0,500),rep(1,400))

#Degree distribution of nodes based on the Measure DHS data in Uganda (2004)
ego.deg <- c(248,543,93,11,5) #Node distribution
#The assumption is that population is hetrosexual. Following command, shows how number
#of reported partners are defferent among men and women.
ego.mixmat <- matrix(c(0,360,292,0)/2,nrow=2,byrow=T)
#Total number of edges(Sexual contacts)
ego.edges <- sum(ego.mixmat)
#Total number of homosexual contacts, which is zero
ego.sexmatch <- ego.mixmat[1,1]+ego.mixmat[2,2]
#Target statistics for the model
ego.target.stats <- c(ego.edges,ego.sexmatch)
ego.target.stats

#Building an ERGM model based on the edges and hetrosexuality in the population
ego.fit <- ergm(ego.net~edges+nodematch('sex'),target.stats=ego.target.stats)
summary(ego.fit)

#Simulation of the current network from the ERGM model
ego.sim1 <-simulate(ego.fit)
plot(ego.sim1,vertex.cex=0.65,vertex.col="sex")

#Comparing model results with observed data
rbind(sim=summary(ego.sim1 ~ degree(c(0:4))), obs=ego.deg)
mixingmatrix(ego.sim1, "sex")
ego.mixmat

#We simulate the ergm model for 100 times to see if observed data
#matches the model results
ego.sim100 <- simulate(ego.fit, nsim=100)
ego.sim100

summary(ego.sim100)

#Compare the model results with observed data
sim.stats <- attr(ego.sim100,"stats")
rbind(sim=colMeans(sim.stats), obs=ego.target.stats)

#Model results look pretty close to the observed data
matplot(1:nrow(sim.stats), sim.stats, 
        pch=c("e","m","0","+"), cex=.65, 
        main="100 simulations from ego.fit model", sub="(default settings)",
        xlab="Replicate", ylab="frequency")
abline(h=ego.target.stats, col=c(1:2))
#The lines show the target statistics in the observed data
#To get rid of the auto correlation. We increase MCMC
#interval to get more indeoendent realization.


ego.sim100 <- simulate(ego.fit, nsim=100,
                       control=control.simulate.ergm(MCMC.interval=10000))
sim.stats <- attr(ego.sim100,"stats")
matplot(1:nrow(sim.stats), sim.stats,
        pch=c("e","m"), cex=.65,
        main="100 simulations from ego.fit model", sub="(MCMC.interval=10000)",
        xlab="Replicate", ylab="frequency")
abline(h=ego.target.stats, col=c(1:2))
#Total edges randomly distributed around the observed data and there is no
#autocorrelation


#Now we will fit the degree distribution.
sim.fulldeg <- summary(ego.sim100 ~ degree(c(0:10)))
colnames(sim.fulldeg) <- paste("deg",0:10, sep='')
sim.fulldeg[1:5,]

#The data shows that less than 0.1% of population,
#have more than 4 partners. We put them in 4+degree been
sim.deg <- cbind(sim.fulldeg[,1:4], apply(sim.fulldeg[,5:11],1,sum))
colnames(sim.deg) <- c(colnames(sim.fulldeg)[1:4],"degree4+")
rbind(sim=colMeans(sim.deg), obs=ego.deg)
#Results show that the the difference between the results of single simulation
#and observed data is quite large.

#plot the degree distribution for the observed data in line 
#and simulated results in numbers.
matplot(1:nrow(sim.deg), sim.deg, pch=as.character(0:4), cex=.5,
        main="Comparing ego.sims to non-targeted degree frequencies",
        sub = "(only total edges targeted)",
        xlab = "Replicate", ylab = "Frequencies")
abline(h=c(248,543,93,11,5), col=c(1:5))

#We refit the model with targeting degree 0 people
ego.isolates <- ego.deg[1]
ego.target.stats <- c(ego.edges, ego.sexmatch, ego.isolates)
ego.fit <- ergm(ego.net ~ edges + nodematch('sex') + degree(0),
                target.stats = ego.target.stats) 

summary(ego.fit)

ego.sim100 <- simulate(ego.fit, nsim=100,
                       control=control.simulate.ergm(MCMC.interval=10000))
sim.stats <- attr(ego.sim100,"stats")
rbind(sim=colMeans(sim.stats), obs=ego.target.stats)

sim.fulldeg <- summary(ego.sim100 ~ degree(c(0:10)))
sim.deg <- cbind(sim.fulldeg[,1:4], apply(sim.fulldeg[,5:11],1,sum))
colnames(sim.deg) <- c(colnames(sim.fulldeg)[1:4],"degree4+")
rbind(sim=colMeans(sim.deg), obs=ego.deg)

matplot(1:nrow(sim.deg), sim.deg, pch=as.character(0:3), cex=.5,
        main="Comparing ego.sims to non-targeted degree frequencies",
        sub = "(only 0, 2+ and total edges targeted)",
        xlab = "Replicate", ylab = "Frequencies")
abline(h=c(248,543,93,11,5), col=c(1:5))
