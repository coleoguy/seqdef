# Making a function that will generate syn.imp.vals from a tree and a table. 

SeqDef <- function(tree, df, data.col, invert){
  if(length(tree$tip.label) != nrow(df)){
    stop("Tree and Data should have same Length")
  } 
  # The above line stops the code if the tree and data differ in length 
  td <- max(branching.times(tree))
  # Tree depth (td) is calculated above 
  for(i in 1:length(tree$tip.label)){
    focal.tip <- tree$tip.label[i] #here we look through our tip labels 
                                    #and make one called "focal tip"
    x <- c()  # x is an empty vector for use later
    for(j in 1:length(tree$tip.label)){
      cur.tip <- tree$tip.label[j]
      if(focal.tip != cur.tip) {
        x <- c(x, 
               (1 - ((fastDist(tree, focal.tip, cur.tip)) / 2) / td)* 
                 df[,data.col][df$species == cur.tip]) 
# lines 18:22 are the stat which takes 1 minus the pairwise distance
# between two differing tips and divides by tbl. 
# next, multiply this value by the importance value for this tip (species)
      }else{
        x <- c(x, df[,data.col][df$species == cur.tip])
      } # lines 26:27 are used in the event that the our focal tip and cur.tip
        # are in fact the same. when this occurs we just get back the imp.val. 
    }
    if(invert){
      df$syn.imp.vals[df$species == focal.tip] <- 1 - mean(x) 
    }else{
      df$syn.imp.vals[df$species == focal.tip] <- mean(x) 
    }
# line 32 stores the mean value of x that we got from earlier inside a 
# data.frame as "synthetic importance values" (syn.imp.vals) 
    # when the species name is 
# the same as the focal.tip we were comparing our cur.tip with. 
  }
    results <- (df$syn.imp.vals - min(df$syn.imp.vals)) /
      (max(df$syn.imp.vals) -min(df$syn.imp.vals))
  return(results)
}

# Lines 38:39 are used to normalize syn.imp.vals into values between 0 and 1.
# Line 40 returns our data frame with our newly calculated synthetic importance 
#values. 

# Incorporating the Data data
#given a tree with random amounts of sequence data (table), 
#and a tree (0 being no data, 
# and 1 being all the data)... we need to find a way to calculate which  
# species need to be sequenced more... and then the ----- of this value, 
# and our synthetic importance value will give overall sequence desirability. 


PlotSeqDef <- function(tree, df, data.col){
  tree$edge.length <- tree$edge.length/ max(branching.times(tree))
  plot(tree, cex=.7)
  tiplabels(text=df[, data.col], adj=-1.5, frame="none", col="red",cex=.7)
  tiplabels(text=as.character(df[, 4]), offset=-.1, cex=.7,frame="none", col="blue")
}




