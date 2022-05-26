

#### Loading required packages.####
library(ape)
library(phytools)


# Here we pull in the functions that we have written
source("functions.R") 

# 1
#### Generating tree, importance and data values ####
taxaN <- 5
tree <- rcoal(taxaN)
plot(tree)
### inputs for the table
imp.vals <- sample(x=c(0,1), size=taxaN , replace=T, prob=c(0.8,0.2))
data.vals <- sample(x=c(0,1), size=taxaN , replace=T, prob=c(0.8,0.2))
species.names <- tree$tip.label ### these are the species names 

df <- data.frame( species.names,  imp.vals,  data.vals )

#### Using the two functions to get synthetic values ####
# Get syn.imp.vals
syn.imp.table <- SeqDef(tree=tree, table=df)
# Get syn.dat.vals
syn.dat.table <- SeqDef2(tree = tree, table = df)

# Adding 0.01 to each of the syn.dat.vals so that zeros won't skew
# the final numeric as much. 

syn.val.frame <- data.frame(syn.imp.table$syn.imp.vals, 
                            syn.dat.table$syn.dat.vals)

# finding the distance from the origin to each syn.point 
for(i in 1:dim(syn.val.frame)[1]){
  #Perform calculations and append to df
  syn.val.frame$combined.vals[i] <- 
    sqrt(syn.val.frame$syn.imp.table.syn.imp.vals[i]^2 + 
           syn.val.frame$syn.dat.table.syn.dat.vals[i]^2)
}


dim(syn.val.frame)
# Lines 39:51 is me trying to write a loop that ensures the larger 
# of the synthetic values is always in the numerator
#for(i in 1:length(syn.val.frame$syn.imp.table.syn.imp.vals)){
  #focal.tip <- syn.val.frame$syn.imp.table.syn.imp.vals[i] 
 # z <- c()  
 # for(j in 1:length(syn.val.frame$syn.dat.table.syn.dat.vals)){
   # cur.tip <- syn.val.frame$syn.dat.table.syn.dat.vals[j]
  # if(focal.tip >= cur.tip) {
   #   z <- c( i / j )}
   # else{
    #  z <- c( j / i)
  
   # }
  #}
#}

# NORMALIZE the final outputs below ?

# sum.dat <- (sum.dat$syn.imp.vals - min(sum.dat$syn.imp.vals)) /
#  (max(sum.dat$syn.imp.vals) -min(sum.dat$syn.imp.vals))

# Making a final df that has all of the values used for this data-set. 
final.df <- data.frame(species.names,
                        imp.vals,
                        data.vals ,syn.dat.table$syn.dat.vals, 
                        syn.imp.table$syn.imp.vals, combined.vals)

#### ADJUSTING COLUMN NAMES of final.df ####

names(final.df)[names(final.df) == "syn.imp.vals"]<- "Final Values"

names(final.df)[names(final.df) == "syn.dat.table.syn.dat.vals"]<-"syn.dat.vals"

names(final.df)[names(final.df) == "syn.imp.table.syn.imp.vals"]<-"syn.imp.vals"




