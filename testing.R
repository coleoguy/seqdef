
###getting the required packages (step 1), and loading them. 
library(ape)
library(phytools)

# Here we pull in the functions that we have written
source("functions.R")
#play with values using the mean function. 



#### random tree data 

tree <- rcoal(50)

### inputs for the table
imp.vals <- sample(c(0,1), 50, replace=T, prob=c(.9,.1))
data.vals <- sample(50)
species.names <- tree$tip.label ### these are the species names 
df <- data.frame(species.names, imp.vals, data.vals)

SeqDef(tree = tree, table = df)

plot(tree)
tiplabels()



# Stress testing the stat-model 

# 1
tree <- rcoal(25)

### inputs for the table
imp.vals <- sample(c(0,1), 25, replace=T, prob=c(.8,.2))
data.vals <- sample(25)
species.names <- tree$tip.label ### these are the species names 


df <- data.frame(species.names, imp.vals, data.vals)


SeqDef(tree = tree, table = df)

plot(tree, tip.color = "black")


#2
tree <- rcoal(50)

### inputs for the table
imp.vals <- sample(c(0,0.5,0.8,1), 50, replace=T, prob=c(0.8,0.05,0.05,0.1))
data.vals <- sample(50)
species.names <- tree$tip.label ### these are the species names 


df <- data.frame(species.names, imp.vals, data.vals)


SeqDef(tree = tree, table = df)
tree$tip.label <- df2$syn.imp.vals
plot(tree, tip.color = "blue", cex=0.5, node.color = "black", 
      main="Tree With Synthetic Importance Values")












