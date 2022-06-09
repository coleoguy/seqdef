

#### Loading required packages.####
library(ape)
library(phytools)


# Here we pull in the functions that we have written
source("functions.R") 

#### Generating tree, importance and data values ####
taxaN <- 10
tree <- pbtree(n=taxaN)
plot(tree, cex= 1, main= "Tree", offset = 1, adj= 2)
edgelabels(text=round(tree$edge.length,3),frame="none", col = "red", cex=0.8)

# inputs for the table
imp <- sample(x=c(0,1), size=taxaN , replace=T, prob=c(0.8,0.2))
data <- sample(x=c(0,1), size=taxaN , replace=T, prob=c(0.8,0.2))

??edgelabels
??offset

df <- data.frame(tree$tip.label,  imp,  data)
colnames(df)[1] <- "species"
# rm(imp,data, taxaN)
#### Done generating example data ######


#### Using the two functions to get synthetic values ####

results <- matrix(NA, 100, 2)
colnames(results) <- c("mean.raw","mean.syn")

for(i in 1:100){
  print(i)
  set.seed(i)
  taxaN <- 50
  tree <- pbtree(n=taxaN)
  imp <- runif(taxaN)
  df <- data.frame(tree$tip.label,  imp)
  colnames(df)[1] <- "species"
  synvals <- SeqDef(tree=tree, df=df, data.col=2, invert=T, scale=F)
  results[i,2] <- mean(synvals)
  results[i,1] <- mean(df[,2])
}

plot(tree)






# Get syn.imp.vals

final <- SeqDef(tree=tree, df=df, data.col=3, invert=T, scale= F)
final







PlotSeqDef(tree, df, 3)

syn.val.frame <- data.frame(syn.imp.table$syn.imp.vals, 
                            syn.dat.table$syn.dat.vals)


  #Finding the distance from the origin to each syn.point 
for(i in 1:dim(syn.val.frame)[1]){
  #Perform calculations and append to df
  syn.val.frame$combined.vals[i] <- 
    sqrt(syn.val.frame$syn.imp.table.syn.imp.vals^2 + 
           syn.val.frame$syn.dat.table.syn.dat.vals^2)
}






# Making a final df that has all of the values used for this data-set. 
final.df <- data.frame(species.names,
                        imp.vals,
                        data.vals ,syn.dat.table$syn.dat.vals, 
                        syn.imp.table$syn.imp.vals, syn.val.frame$combined.vals)

