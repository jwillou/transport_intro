setwd("~/GDrive/Willoughby lab/transport risk/")
#example IBM code

#parameters
wildtocargo  = c(10)
survivetrans = 0.9
exitcargo    = 0.5
newenv       = 1

#initialize population <----- for mosquitos we will have to think carefully about how to do this without 
#simulating every individual right away. I made this mistake before and crashed a cluster node!
pop = matrix(nrow=100, ncol=5)            #each individual gets its own row
pop[,1] = seq(1,100,1)                    #each individual has unique ID name
pop[,2] = sample(c(0,1),100,replace=T)    #each individual assigned male (1) or female (0)
pop[,3] = rep(0,100)                      #mom id - later will not be 0, this is useful for debugging
pop[,4] = rep(0,100)                      #dad id - later will not be 0, this is useful for debugging
#...you can add other columsn for other species-specific characteristics you want to track

for(r in 1:10){ #this controls the number of times you want to run the simulation - we will run it more than 10 times eventually
  #sample individuals that move from wild to cargo - we will vary this value for sure, so it is a parameter set above
  intransit = pop[sample(seq(1,nrow(pop),1), wildtocargo, replace=F),,drop=F] #randomly select rows from pop that have moved from wild into cargo/ship
  
  #apply some mortality while in transit
  surviveornot = sample(c(0,1), nrow(intransit), replace=T, prob=c((1-survivetrans), survivetrans))
  intransit    = intransit[which(surviveornot==1), , drop=F] #keep only rows that survived, based on survival probabilty in above line
  
  #some proportion of individuals leave cargo chain and enter wild area at recipient port
  exitornot = sample(c(0,1), nrow(intransit), replace=T, prob=c(exitcargo, (1-exitcargo)))
  inwild    = intransit[which(exitornot==1), , drop=F] #keep only rows that exited, based on  probabilty in above line
  intransit = intransit[which(exitornot==0), , drop=F] #update who is in cargo still (this will give some info about repeated port risks)
  
  #some mortality because environment is inhospitable
  liveornot = sample(c(1,0), nrow(inwild), replace=T, prob=c(newenv, (1-newenv)))
  inwild    = inwild[which(liveornot==1), , drop=F] #keep only rows that exited, based on  probabilty in above line
  
  #reproduction in new location
  
  #ACK! this is a complicated section I don't have time to write today, but generally we'd have to make sure there were males and females, 
  #suitable habitat, and also apply some probability of mating. Then, we could also generate the number of expected offspring to give 
  #insight into how stable the population might be (if only 1 offspring, generation 3 would rely on repeated invasions, for example)
  
  #write simulation output to file - currently not written, but we'd want to output that runs parameters plus anything that 
  #is possibly interesting down the line, such as the number of mozzies left intransit and how many offspring were "created" in the new location
  
}
###NOTE: the sample function right now is binary, but to the extent possible we will substitute those better distributions. For example, probablitly
#of surviving in transit is set to 0.9, but if we know the tails of that estimate we can make the model function better.

         