# Making a function that will generate syn.imp.vals from a tree and a table. 

#### Sample Data ####
tree <- pbtree(n=5)
plot(tree)

imp <- c(0,0,0,0,1)
data <- c(0,0,0,0,1)

df <- data.frame(tree$tip.label,  imp,  data)
colnames(df)[1] <- "species" 


#### Functions.Faster.R ####

SeqDef <- function(tree, df, data.col, invert, scale){

    # The above line stops the code if the tree and data differ in length 
  if(length(tree$tip.label) != nrow(df)){
    stop("Tree and Data should have same Length")
  } 
  
  # Tree depth (td) is calculated above 
  td <- max(branching.times(tree))

    ## TODO
  # use cophenetic.phylo from APE to make a matrix of distances ∆
  # then divide the matrix by two to get the distance to the MRCA ∆
  # the divide the values by the tree depth that you calculate above ∆
  # then do 1 - the values in the matrix ∆
  
  dist.matrix <- cophenetic.phylo(tree)
  dist.prop <- 1- ((dist.matrix/2) / td)
  

  
  # At this point you should have a matrix that you can look up 
  # the values that you need below at the second TODO
  
  for(i in 1:length(tree$tip.label)){
    focal.tip <- tree$tip.label[i] 
    
    x <- c()  
    
    for(j in 1:length(tree$tip.label)){
      cur.tip <- tree$tip.label[j]
      # The first seven lines below will go away instead
      # of calculating every time, we will just look up the
      # values that we need for any particular pairing
      
      x <- c(x, dist.prop[row.names(dist.prop) == cur.tip, 
                          colnames(dist.prop) == focal.tip] 
             * df[ ,data.col][df[,1] == cur.tip])
    }
  }
      

       
    if(invert){
      df$syn.imp.vals[df$species == focal.tip] <- 1 - mean(x) 
    }else{
      df$syn.imp.vals[df$species == focal.tip] <- mean(x) 
    }
  
    if(scale){
      results <- (df$syn.imp.vals - min(df$syn.imp.vals)) /
      (max(df$syn.imp.vals) -min(df$syn.imp.vals))
    }else{
      results <- df$syn.imp.vals
    }
  return(results)
}

 

PlotSeqDef <- function(tree, df, data.col){
  tree$edge.length <- tree$edge.length/ max(branching.times(tree))
  plot(tree, cex=.7)
  tiplabels(text=df[, data.col], adj=-1.5, frame="none", col="red",cex=.7)
  tiplabels(text=as.character(df[ , 4]), offset=-.1, cex=.7,frame="none", col="blue")
}





