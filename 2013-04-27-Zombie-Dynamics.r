# The Dynamics of a Zombie Apocalypse

# Zombies vs Humans Agent Based Simulation

# This simulation is a simple repeated matching simulation in which each agent is matched with a random different agent.

# I believe this is an appropriate way of modelling a human zombie exchange as portrayed in the movies.  Generally speaking, each encounter can be thought of as a probabilistic draw in which either the human becomes zombified or the zombie is permanently killed.

# Agents all start out as humans (except a random percent which are initially zombified).  Each human is defined on a scale of 0 to 90 percentile with increments of 10 which reflect the chance of that human vanquishing a zombie that the human encountered or being in turn vanquished.

# If a human is vanquished a zombie is added to the zombie population.

# number of humans (approximated due to rounding issues)
start.humans = 100000

# matrix of humand types
htypes = seq(0,90,10)

# Frequency of each time of human from 0 to 90 percentile.
# This number is only relative to the scale of the other frequencies so long as it is positive.
# Thus if only type of human had a 5 then it would be 5 times more likely than a type of human with a frequency of 1.
freq = rep(1,length(htypes))

# frequency is the most important parameter choice in the model as will be seen below.

# Bind the information into a single matrix.
human.types = cbind(htypes,freq)

# Now we calculate what percentage of our start.humans are each type.
perc = round((freq/sum(freq))*start.humans)

# Finally we generate our moving things data.
# Initially the only thing moving is humans.
walking.things = rep(htypes,perc)

# Looking good
walking.things

# Some percentage of the humans are initially infected.
infected.per = .025

# Calculate the  number initially that become infected.
nselected = round(start.humans*infected.per)

# Now we randomly select the humans infected initially.
initial.zombies = sample(1:length(walking.things), nselected)

walking.things=c(walking.things[-initial.zombies], rep(-77,nselected))
             
# -77 Is the number for a zombie.
      
# After the intial infection phase peopole get their guns out and start acting defensively.
walking.things

# Percent zombies
perc.zombies = mean(walking.things==-77)

# Total population (living and dead)
nthings.vector = nthings = length(walking.things)

# Count the number of zombies
nzombies = sum(walking.things==-77)

# Count the number of humans
nhumans = sum(walking.things!=-77)

  nhumans0  = sum(walking.things==00)
  nhumans10 = sum(walking.things==10)
  nhumans20 = sum(walking.things==20)
  nhumans30 = sum(walking.things==30)
  nhumans40 = sum(walking.things==40)
  nhumans50 = sum(walking.things==50)
  nhumans60 = sum(walking.things==60)
  nhumans70 = sum(walking.things==70)
  nhumans80 = sum(walking.things==80)
  nhumans90 = sum(walking.things==90)


# This command pairs up a vector.  It is used to match humans with zombies.
pairup = function(x, unmatched.self=T) {
  
  # Calculate the length of the x vector.
  xleng = length(x)
  
  # This checks if the input vector is a scalar.
  if (xleng==1) x = 1:(xleng=x)
  
  # Half the length of x rounded down.
  hleng = floor(xleng/2)
  
  # Randomize x
  x <- x[order(runif(xleng))]
  
  pairs = cbind(x[1:hleng],x[(hleng+1):(2*hleng)])
  
  # If there is a odd number of xs then this will match the remaining unmatched x with itself if unmatched.self is T. 
  if ((unmatched.self)&(xleng/2!=hleng)) pairs=rbind(pairs, c(x[2*hleng+1]))
  
  return (pairs)
}

#
max.rounds = 45

# Let's start the simulation:
n = 1
while  (nzombies[n]>0 & nhumans[n]>0 & n<max.rounds) {
  n = n+1
  
  # This calls the previously defined function pairup to match two different individuals together.
  # This matches them by position in the walking.things vector.
  encounter=pairup(nthings)
  
  # This assigns to the matrix the values
  types = cbind(walking.things[encounter[,1]],walking.things[encounter[,2]])

  # Create a vector of terminated or zombified things
  conflict = types*0
     # 0 Unresolved
     # 1 Zombified
     # 2 Permenent Death
     # 3 No conflict
     # 4 win conflict
   
   # This code will check if a zombie is in the right column and human in the left.
   hvz = (types[,2]==-77)&(types[,1]>=0)
     # If so, the human and zombie places will be switched.
     types.temp = types
     types[hvz,1]=types.temp[hvz,2]
     types[hvz,2]=types.temp[hvz,1]
     
     encounter.temp = encounter
     encounter[hvz,1]=encounter.temp[hvz,2]
     encounter[hvz,2]=encounter.temp[hvz,1]
     
   # Zombie encounters human
   zvh = (types[,1]==-77)&(types[,2]>=0)
     # Calculate the win count of the conflict
     win.zvh = (runif(sum(zvh))>types[zvh,2]/100)
     
     # Translate a zombie win onto the conflict map
     conflict[zvh,1][win.zvh]=4
     conflict[zvh,2][win.zvh]=1    
     
     # Translate a human win onto the conflict map
     conflict[zvh,1][!win.zvh]=2
     conflict[zvh,2][!win.zvh]=4

     # Resolve non-conflict. Zombies don't fight zombies and humans don't fight humans.
     conflict[types[,1]==types[,2],] = 3
     conflict[(types[,1]>=0)&(types[,2]>=0),] = 3
     
   # Finally, adjust the walking.things vector to adjust for the changes.
   # Zombify some
   walking.things[encounter[conflict==1]] = -77
     
   # Remove others
   walking.things=walking.things[-encounter[conflict==2]]

  # Store stats
  # Percent zombies
  perc.zombies = c(perc.zombies, mean(walking.things==-77))

  # Total population (living and dead)
  nthings = length(walking.things)
  nthings.vector = c(nthings.vector, nthings)

  # Count the number of zombies
  nzombies = c(nzombies, sum(walking.things==-77))

  # Count the number of humans and save them to vectors
  nhumans = c(nhumans,  sum(walking.things!=-77))
  
  nhumans0  = c(nhumans0,   sum(walking.things==0))
  nhumans10 = c(nhumans10,  sum(walking.things==10))
  nhumans20 = c(nhumans20,  sum(walking.things==20))
  nhumans30 = c(nhumans30,  sum(walking.things==30))
  nhumans40 = c(nhumans40,  sum(walking.things==40))
  nhumans50 = c(nhumans50,  sum(walking.things==50))
  nhumans60 = c(nhumans60,  sum(walking.things==60))
  nhumans70 = c(nhumans70,  sum(walking.things==70))
  nhumans80 = c(nhumans80,  sum(walking.things==80))
  nhumans90 = c(nhumans90,  sum(walking.things==90))

}

# Count the number of rounds completed.
nrounds.completed = length(nhumans0)

plot(c(1,nrounds.completed), c(0,max(nhumans0,nzombies)), type="n",
     ylab="Population", xlab="Round\n*Note: The number at the end of each line is the probability that an individual\nin this population group will kill a zombie when encountering one. Z is the zombie population"
     , main="Population During a Zombie Attack\nWith a well armed human population a zombie apocalypse is easily prevented")
for (i in seq(0,90,10)) {
  lines(get(paste("nhumans",i,sep="")))
  text(nrounds.completed+1, get(paste("nhumans",i,sep=""))[nrounds.completed], i)
}
lines(nzombies, lwd=3)
  text(nrounds.completed+1, nzombies[nrounds.completed], "Z")

# However, this equal proportion of highely effective zombie killers to very ineffective zombie killers in unrepresentational of typical zombie movies or games.

# I will instead rerun the simulation with a larger percentage of low ability humans.
htypes = seq(0,90,10)
freq = 10:1

# .... using same code as above but with new human population proportions

nrounds.completed = length(nhumans0)

plot(c(1,nrounds.completed), c(0,max(nhumans0,nzombies)), type="n",
     ylab="Population", xlab="Round\n*Note: Even the best trained individual will be overwhelmed eventually\nif there are too many easy zombie victims. Z is the zombie population"
     , main="Population During a Zombie Attack\nA poorly armed population is ill-equiped to survive a zombie attack")
for (i in seq(0,90,10)) {
  lines(get(paste("nhumans",i,sep="")))
  text(nrounds.completed+1, get(paste("nhumans",i,sep=""))[nrounds.completed], i)
}
lines(nzombies, lwd=3)
  text(nrounds.completed+1, nzombies[nrounds.completed], "Z")

# Let's try one more variant with still more weak humans but a little better ratios.

htypes = seq(0,90,10)
freq = seq(5,1, length.out=10)

# .... using same code as above but with new human population proportions

nrounds.completed = length(nhumans0)

plot(c(1,nrounds.completed), c(0,max(nhumans0,nzombies)), type="n",
     ylab="Population", xlab="Round\n*Note: In this scenario only the top 10 to 20% most effective zombie killers survive."
     , main="Population During a Zombie Attack-When the population of dangerous humans\n is sufficiently large, it is possible humanity survives, just barely")
     
for (i in seq(0,90,10)) {
  lines(get(paste("nhumans",i,sep="")))
  text(nrounds.completed+1, get(paste("nhumans",i,sep=""))[nrounds.completed], i)
}
lines(nzombies, lwd=3)
  text(nrounds.completed+1, nzombies[nrounds.completed], "Z")

# Overall conclusion? Mandating zombie defense classes is the only way to be certain humanity will survive.
  