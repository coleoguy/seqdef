\name{SeqDef}
\alias{PlotSeqDef}
\title{Calculate and Visualize seqdef statistic}
\usage{
SeqDef(tree, df, data.col, invert, scale)

PlotSeqDef(tree, df, data.col)

}
\description{
SeqDef allows for analysis of phylogenetic sequencing data in combination with
other "importance" parameters of interest. This function allows for a more 
holistic view of which taxa on a given phylogeny need to be focused upon. SeqDef 
accomplishes this by calculating the average of the distance between species,
divided by the branch length, multiplied by the given data column value. 
}

\arguments{
  \item{tree}{a phylo object}
  \item{df}{a dataframe consisting of tiplabels (species), and numeric values 
                  corresponding to importance and sequencing data}
  \item{data.col}{a numeric value indicating the 
                    column of df to use in calculations}
  \item{invert}{ if true, this argument will subtract the average of 'x' from 1}
  \item{scale}{if true, this argument will scale the values 
                      of x to between 0 and 1}
}


\examples{
Howdy ∆
tree <- pbtree(n=taxaN)
imp <- c(0,0,0,0,1)
data <- c(0,0,0,0,1)
df <- data.frame(tree$tip.label,  imp,  data)
colnames(df)[1] <- "species"
results <- matrix(NA, 100, 2)
colnames(results) <- c("mean.raw","mean.syn")
synvals <- SeqDef(tree=tree, df=df, data.col=2, invert=T, scale=F)

}



