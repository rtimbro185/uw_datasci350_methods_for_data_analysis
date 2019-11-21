# Core simulation function to measure the number of wins a contestant could expect based on a sample input size
montyHall = function(isSwitchingDoor=FALSE, nsim=1000){
  # integer set representing the possible door selections
  doors = 1:3
  wins = 0
  losses = 0
  p.wins = 0.0
  p.losses = 0.0
  
  # loop through each realization creating a taily of wins for the sample 
  for(i in 1:nsim){
    prize.door = NULL
    player.selected.door = NULL
    host.selected.door = NULL
    final.selected.door = NULL
    
    #randomize which door has the prize
    prize.door = floor(runif(1,1,4))
    
    #guess a door at random
    player.selected.door = floor(runif(1,1,4))
    
    #host selects a door that does not have a prize
    if(prize.door!=player.selected.door){
      host.selected.door = doors[-c(prize.door,player.selected.door)]
    }else{
      host.selected.door = sample(doors[-c(prize.door,host.selected.door)],1)
    }
    
    # stay or switch?
    if(isSwitchingDoor){
      final.selected.door = doors[-c(host.selected.door,player.selected.door)]
    }else{
      final.selected.door = player.selected.door
    }
    
    # count wins versus losses
    if(final.selected.door==prize.door){
      wins = wins + 1
      
    }else{
      losses = losses + 1
    }
    
  }
  p.wins = wins/nsim*100
  p.losses = losses/nsim*100
  wins.losses = c(wins, p.wins, losses, p.losses, nsim)
  
  return(wins.losses)
}


# Run a series of trial for the given sample set
run.trials.sim = function(isSwitchingDoor=FALSE, nsim=1000, trials=5){
  require(dplyr)

  trial.data = data.frame(matrix(ncol=5))
  colnames(trial.data) = c("wins","p.wins", "losses","p.losses" ,"count")
  
  for(i in 1:trials){
    result = montyHall(isSwitchingDoor,nsim)
    trial.data = rbind(trial.data,result)
  }
  t.data = na.omit(trial.data)
  return(t.data)
}


# Plot the results
plot.sims = function(stay,switch){
  require(ggplot2)
  require(repr)
  require(gridExtra)
  options(repr.plot.width=6, repr.plot.height = 6)
  
  minx = min(min(stay$p.wins),min(switch$p.wins))
  maxx = max(max(stay$p.wins),max(switch$p.wins))
  bw = (maxx-minx)/60
  p.stay = ggplot(stay,aes(p.wins)) + geom_histogram(binwidth = bw) +
    xlab("Frequency of Wins") +
    ggtitle("Stay Propability Frequencys")
  p2.stay = p.stay + geom_vline(aes(xintercept=summarise(stay,mean.p.wins = mean(p.wins,na.rm=TRUE))))
    

  p.switch = ggplot(switch,aes(p.wins)) + geom_histogram(binwidth = bw) +
    xlab("Frequency of Wins") +
    ggtitle("Switch Probability Frequencys")
  p2.switch = p.switch + geom_vline(aes(xintercept=summarise(switch,mean.p.wins = mean(p.wins,na.rm=TRUE))))
  
  print(grid.arrange(p2.stay,p2.switch,nrow=2))
  
}


#Run non switch similation test with 1000 guesses and 10000 trials
stay = run.trials.sim(FALSE, 1000, 1000)
#print(stay)
print(summary(stay))

#Run switch simulation test with with 1000 guesses and 10000 trials
switch = run.trials.sim(TRUE, 1000, 1000)
#print(switch)
print(summary(switch))

#Plot the probability outcome results for staying and switching
plot.sims(stay,switch)



