#' Bagging for Decision Trees
#'
#' Implements bagging (Bootstrap Aggregating) for decision trees, supporting both regression and classification tasks.
#'
#' @export
#' @param t The original decision tree.
#' @param B The number of bootstrap samples to generate. Default is 10.
#' @param lambda The complexity parameter for pruning in the case of regression trees. Default is 0.
#' @return An ensemble model that combines the results of multiple bootstrapped decision trees.
#' @examples
#' # Assuming `tree` is an initialized decision tree object
#' ensemble_model <- bagging(tree, B = 20, lambda = 0.5)
bagging <- function(t, B = 10, lambda = 0, random_subset=FALSE) {
    if (t$type == "classification") return(bagging_classification(t, B, random_subset))
    return(bagging_regression(t, B, lambda, random_subset))
}

bagging_regression <- function(t, B, lambda, random_subset = FALSE) {
    t_star <- list(B)
    for (i in 1:B) {
        t_star[[i]] <- get_t_star(t, lambda)
    }

    f_bagg <- function(x) {
        pred_sum <- sum(sapply(t_star, function(tree) tree$decide(x)))
        return(pred_sum / B)
    }
    return(f_bagg)
}

get_t_star <- function(t, lambda = 0, random_subset = FALSE) {
    sample_indices <- get_bootstrap_sample(t)
    t_star <- greedy(matrix(t$training_data_x[sample_indices, ], ncol=t$d), matrix(t$training_data_y[sample_indices, ]), random_subset = random_subset)
    if (t$type == "classification") return(t_star)
    trees <- get_pruning_sequence(t_star)
    t_star_pruned <- choose_tp_lambda(lambda, trees)
    return(t_star_pruned)
}


get_bootstrap_sample <- function(t) {
    n <- nrow(matrix(t$training_data_x, ncol=t$d))
    sample <- sample(1:n, n, replace = TRUE)
}



bagging_classification <- function(t, B, random_subset = FALSE) {
    t_star <- vector("list", B)

    for (i in 1:B) {
        t_star[[i]] <- get_t_star(t)
    }

    f_bagg <- function(x) {
        ux <- unique(sapply(t_star, function(tree) tree$decide(x)))
        return(ux[which.max(tabulate(match(x, ux)))])
    }


    return(f_bagg)
}
