
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


#2-------------------------------------------------------------------
taxaN <- 10
tree <- rcoal(taxaN)
plot(tree)
### inputs for the table
imp.vals <- sample(x=c(0,1), size=taxaN , replace=T, prob=c(0.8,0.2))
data.vals <- sample(x=c(0,1), size=taxaN , replace=T, prob=c(0.8,0.2))
species.names <- tree$tip.label ### these are the species names 


df <- data.frame(species.names, imp.vals, data.vals)

# to get syn.imp.vals
syn.imp.table <- SeqDef(tree=tree, table=df)
# to get syn.dat.vals
syn.dat.table <- SeqDef2(tree = tree, table = df)


final.df1 <- data.frame(syn.dat.table$syn.dat.vals,syn.imp.table$syn.imp.vals)

avg.dat <- (final.df1[1] + final.df1[2])/2
product.dat <- final.df1[1] * final.df1[2]

final.df2 <- data.frame(syn.dat.table$syn.dat.vals, syn.imp.table$syn.imp.vals, 
                        avg.dat, product.dat)

names(final.df2)[names(final.df2) == "syn.dat.table.syn.dat.vals"] <- "avg.dat"

names(final.df2)[names(final.df2) == "syn.dat.table.syn.dat.vals.1"] <- "products"


plot(tree, tip.color = "blue", cex=0.95, node.color = "black", 
      main="Tree With Synthetic Importance Values")














