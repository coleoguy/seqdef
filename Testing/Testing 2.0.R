
# Loading in packages
library(ape)
library(phytools)
# Creating a phylo object
tree <- pbtree(n= 10, scale=1)
plot(tree)

# Generaring sample data for both 'importance' and 'data'
imp.vals <- sample( c(0,1), size= length(tree$tip.label),
                    replace= T,prob=c(.8,.2))

data.vals <- sample( c(0,1), size= length(tree$tip.label),
                     replace= T,prob=c(.8,.2))
# Combining the synthesized values into a single data frame. 
df <- data.frame(tree$tip.label,  imp.vals, data.vals)

SeqDef(tree, df, data.col = 2, invert = T, scale = T )
                    
plot.seqdef(results)
