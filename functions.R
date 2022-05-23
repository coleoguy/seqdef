##Making a function that will generate syn.imp.vals from a tree and a table. 

SeqDef <- function(tree, table){
  if(length(tree$tip.label) != nrow(table)){
    stop("tree and data should have same length")
  } # the above line stops the code if the tree and data differ in length 
  tbl <- sum(tree$edge.length)
  # total branch length (tbl) is calculated above 
  for(i in 1:length(tree$tip.label)){
    focal.tip <- tree$tip.label[i] #here we look through our tip labels 
                                    #and make one called "focal tip"
    x <- c()  # x is an empty vector for use later
    for(j in 1:length(tree$tip.label)){
      cur.tip <- tree$tip.label[j]
      if(focal.tip != cur.tip) {
        # lines 13:15 above ensure that our focal and current tip are not
          # the same two tips. this prevents arbitrary comparisons. 
        x <- c(x, 
               (1 - fastDist(tree, 
                             focal.tip, 
                             cur.tip) / tbl)* 
                 df$imp.vals[df$species.names == cur.tip]) 
# lines 18:22 are the stat which takes 1 minus the pairwise distance
# between two differing tips and divides by tbl. 
# next, multiply this value by the importance value for this tip (species)
      }else{
        x <- c(x, df$imp.vals[df$species.names == cur.tip])
      } # lines 26:27 are used in the event that the our focal tip and cur.tip
        # are in fact the same. when this occurs we just get back the imp.val. 
      
    }
    df$syn.imp.vals[df$species.names == focal.tip] <- mean(x) 
# line 32 stores the mean value of x that we got from earlier inside a 
# data.frame as "synthetic importance values" (syn.imp.vals) when the species name is 
# the same as the focal.tip we were comparing our cur.tip with. 
  }
  
  df$syn.imp.vals <- (df$syn.imp.vals - min(df$syn.imp.vals)) /
    (max(df$syn.imp.vals) -min(df$syn.imp.vals))
  return(df)
}

# Lines 38:39 are used to normalize syn.imp.vals into values between 0 and 1.
# Line 40 returns our data frame with our newly calculated synthetic importance 
#values. 



