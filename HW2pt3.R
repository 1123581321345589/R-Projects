setwd("C:/Users/dunnn2/Desktop/R Working Directory")
data = read.csv("insects_data.csv") 


antLength = data$Antenna_Length

Ant_data = data[order(data[,3]),]

avgs = c ()

for( i in 1:(length(antLength)-1) ){
  avgs[i] = (antLength[i] + antLength[i+1])/2
}


# for each element in the avgs array{
#   -count the entries less than that element
#   -count the number of class 1s and class 2s
#   -count the entries greater than that element by subtracting the ones
#    that are less than it from the total number of entries
#   -count the number of class 1s and class 2s
# }

ginis = c ()

c1abv = 0
c1bel = 0
c2abv = 0
c2bel = 0
greater = 0
lesser = 0
great_leaf_gini = 0
less_leaf_gini = 0
min_gini = 1000
split = 0


# looping through the averages
for (j in 1:length(avgs)){
  # looping through the entries in the dataset to get counts for probability calculations
  for (k in 1:length(antLength)){
     # increments the counts of the elements greater and lesser than the element
     if(Ant_data[k, "Antenna_Length"] < avgs[j]){
       lesser = lesser + 1
       # increments the counts of entries above and below the average
       if(Ant_data[k, "c"] == 1){
         c1bel = c1bel + 1
       }
       else{
         c2bel = c2bel + 1
       }
     }
     else{
       greater = greater + 1
       if(Ant_data[k, "c"] == 1){
         c1abv = c1abv + 1
       }
       else{
         c2abv = c2abv + 1
       }
     }

  }
  great_leaf_gini = 1 - (c1abv/greater)**2 - (c2abv/greater)**2
  less_leaf_gini = 1 - (c1bel/lesser)**2 - (c2bel/lesser)**2

  ginis[j] = (lesser/(greater+lesser))*less_leaf_gini + (greater/(greater+lesser))*great_leaf_gini
  
  if (ginis[j] < min_gini){
    min_gini = ginis[j]
    split = avgs[j]
  }
  
  #resetting counts for next
  c1abv = 0
  c1bel = 0
  c2abv = 0
  c2bel = 0
  greater = 0
  lesser = 0
  great_leaf_gini = 0
  less_leaf_gini = 0
}

gini_index = min(ginis)


print(avgs)
print(Ant_data)
print(min_gini)
print(split)

print("If the length of the antenna is less than 6.7, there is an 83.33% or 5/6 probability the observation is class 1, if it is greater than that there is a 100% or 4/4 probability that the observation is class 2")
