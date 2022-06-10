SeqDef <- function(tree, df, data.col, invert, scale){
  # The above line stops the code if the tree and data differ in length 
  if(length(tree$tip.label) != nrow(df)){
    stop("Tree and Data should have same Length")
  } 
  td <- max(branching.times(tree))
  dist.matrix <- cophenetic.phylo(tree)
  dist.prop <- 1 - ((dist.matrix/2) / td)
  synscores <- vector(length = length(tree$tip.label))
  names(synscores) <- tree$tip.label
  for(i in 1:length(tree$tip.label)){
    focal.tip <- tree$tip.label[i] 
    x <- vector(length = length(tree$tip.label))  
    for(j in 1:length(tree$tip.label)){
      cur.tip <- tree$tip.label[j]
      x[j] <- dist.prop[row.names(dist.prop) == cur.tip, 
                          colnames(dist.prop) == focal.tip] * 
              df[, data.col][df[, 1] == cur.tip]
    }
    if(invert){
      synscores[i] <- 1 - mean(x) 
    }else{
      synscores[i] <-  mean(x) 
    }
  }
  if(scale){
    synscores <- (synscores - min(synscores)) /
      (max(synscores) -min(synscores))
  }
  # prepare the results
  empscores <- df[, data.col]
  results <- list(tree, synscores, empscores)
  class(results) <- "seqdef"
  return(results)
}

plot.seqdef <- function(results){
  tree <- results[[1]]
  synscores <- results[[2]]
  empscores <- results[[3]]
  tree$edge.length <- tree$edge.length/ max(branching.times(tree))
  plot(tree, cex=.7)
  tiplabels(text=empscores, adj=-1.5, frame="none", col="red",cex=.7)
  tiplabels(text=synscores, offset=-.1, cex=.7,frame="none", col="blue")
}




