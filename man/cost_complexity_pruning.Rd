% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/costComplexityPruning.R
\name{cost_complexity_pruning}
\alias{cost_complexity_pruning}
\title{Cost-Complexity Pruning}
\usage{
cost_complexity_pruning(
  t,
  m = 10,
  lambda_min = 0,
  lambda_max = 1,
  lambda_step = 0.05,
  plot = FALSE,
  print_progress = FALSE
)
}
\arguments{
\item{t}{The original decision tree to be pruned.}

\item{m}{Number of partitions to create for cross-validation. Default is 10.}

\item{lambda_min}{Minimum value for the complexity parameter \eqn{\lambda}. Default is 0.}

\item{lambda_max}{Maximum value for the complexity parameter \eqn{\lambda}. Default is 1}

\item{lambda_step}{Step size for iterating over \eqn{\lambda} values. Default is 0.05.}

\item{plot}{Logical flag to indicate whether to plot the cross-validation error against \eqn{\lambda}. Default is FALSE.}

\item{print_progress}{Logical flag to indicate whether to print progress messages during execution. Default is FALSE.}
}
\value{
A pruned decision tree based on the optimal \eqn{\lambda} value.
}
\description{
This function performs cost-complexity pruning on a decision tree using cross-validation to select the optimal complexity parameter (\eqn{\lambda}).
}
\examples{
# Assuming `tree` is an initialized decision tree object
pruned_tree <- cost_complexity_pruning(tree, lambda_min=1, lambda_max=50, lambda_step=1, plot=TRUE)
}
