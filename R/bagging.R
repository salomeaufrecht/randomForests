#' Bagging for Decision Trees
#'
#' Implements bagging (Bootstrap Aggregating) for decision trees, supporting both regression and classification tasks.
#'
#' @export
#' @param t The original decision tree.
#' @param B The number of bootstrap samples to generate. Default is 10.
#' @param lambda The complexity parameter for pruning in the case of regression trees. Default is 1.
#' @return An ensemble model that combines the results of multiple bootstrapped decision trees.
#' @examples
#' # Assuming `tree` is an initialized decision tree object
#' ensemble_model <- bagging(tree, B = 20, lambda = 0.5)
bagging <- function(t, B = 10, lambda = 1) {
    if (t$type == "classification") return(bagging_classification(t, B))
    return(bagging_regression(t, B, lambda))
}

bagging_regression <- function(t, B, lambda) {
    t_star <- list(B)
    for (i in 1:B) {
        t_star[[i]] <- get_t_star(t, lambda)
    }

    f_bagg <- function(x) {
        pred_sum <- sum(sapply(t_star, function(tree) tree$f(x)))
        return(pred_sum / B)
    }
    return(f_bagg)
}

get_t_star <- function(t, lambda = 1) {
    sample_indices <- get_bootstrap_sample(t)
    t_star <- greedy(matrix(t$training_data_x[sample_indices, ]), matrix(t$training_data_y[sample_indices, ]))
    if (t$type == "classification") return(t_star)
    trees_risk_t <- get_pruning_sequence(t_star)
    t_star_pruned <- choose_tp_lambda(lambda, trees_risk_t$trees, trees_risk_t$risk)
    return(t_star_pruned)
}


get_bootstrap_sample <- function(t) {
    n <- length(t$training_data_x)
    sample <- sample(1:n, n, replace = TRUE)
}



bagging_classification <- function(t, B) {
    t_star <- vector("list", B)

    for (i in 1:B) {
        t_star[[i]] <- get_t_star(t)
    }

    f_bagg <- function(x) x # TODO


    return(f_bagg)
}
