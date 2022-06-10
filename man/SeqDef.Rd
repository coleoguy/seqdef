\name{SeqDef}
\alias{PlotSeqDef}
\title{Calculate and Visualize seqdef statistic}
\usage{
SeqDef(tree, df, data.col, invert, scale)

PlotSeqDef(results)

}
\description{
SeqDef allows for analysis of data distributions and importance accounting for phylogeny. This function allows for a more holistic view of which taxa on a given phylogeny need to be focused upon. SeqDef accomplishes this by combining the data distribution with the covariance among tips based on an ultrametric phylogeny. 
}

\arguments{
  \item{tree}{a phylo object}
  \item{df}{a dataframe the first column should contain the species on the phylogeny additional columns should be numeric and contain the data of interest (e.g. the availability of sequencing data or pest status.)}
  \item{data.col}{a numeric value indicating the 
                    column of df to use in calculations}
  \item{invert}{if true high values in the data.col indicate importance}
  \item{scale}{if true, this argument will scale the synthetic scores between 0 and 1}
  \item{results}{an object of class seqdef returned by the seqdef function}
}


\examples{
tree <- pbtree(n=taxaN)
imp <- c(0,0,0,0,1)
data <- c(0,0,0,0,1)
df <- data.frame(tree$tip.label,  imp,  data)
colnames(df)[1] <- "species"
synvals <- SeqDef(tree=tree, df=df, data.col=2, invert=T, scale=F)
plot(seqdef)
}



