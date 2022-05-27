

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
imp.vals <- sample(x=c(0,0.8,0.5,1), size=taxaN , replace=T, prob=c(0.5,0.2,0.2,0.1))
data.vals <- sample(x=c(0,0.8,0.5,1), size=taxaN , replace=T, prob=c(0.5,0.2,0.2,0.1))
species.names <- tree$tip.label ### these are the species names 

df <- data.frame( species.names,  imp.vals,  data.vals )

#### Using the two functions to get synthetic values ####
# Get syn.imp.vals
syn.imp.table <- SeqDef(tree=tree, table=df)
# Get syn.dat.vals
syn.dat.table <- SeqDef2(tree = tree, table = df)

syn.val.frame <- data.frame(syn.imp.table$syn.imp.vals, 
                            syn.dat.table$syn.dat.vals)

  #Finding the distance from the origin to each syn.point 
for(i in 1:dim(syn.val.frame)[1]){
  #Perform calculations and append to df
  syn.val.frame$combined.vals[i] <- 
    sqrt(syn.val.frame$syn.imp.table.syn.imp.vals[i]^2 + 
           syn.val.frame$syn.dat.table.syn.dat.vals[i]^2)
}


# dim(syn.val.frame)



# Normalization Function Below 
# combined.vals <- (syn.val.frame$combined.vals - 
# min(syn.val.frame$combined.vals)) /
#  (max(syn.val.frame$combined.vals) -min(syn.val.frame$combined.vals))

# Making a final df that has all of the values used for this data-set. 
final.df <- data.frame(species.names,
                        imp.vals,
                        data.vals ,syn.dat.table$syn.dat.vals, 
                        syn.imp.table$syn.imp.vals, syn.val.frame$combined.vals)

#### ADJUSTING COLUMN NAMES of final.df ####

names(final.df)[names(final.df) == "syn.dat.table.syn.dat.vals"]<-"syn.dat.vals"

names(final.df)[names(final.df) == "syn.imp.table.syn.imp.vals"]<-"syn.imp.vals"








