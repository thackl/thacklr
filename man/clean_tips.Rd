% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phylo.R
\name{clean_tips}
\alias{clean_tips}
\title{Remove Missing Tips}
\usage{
clean_tips(tips, tree)
}
\arguments{
\item{tips}{vector of tip labels}

\item{tree}{phylo object}
}
\description{
Compare a set of tip labels to a tree, and only keep tips actually
present. Through and error if no tips are present.
}
