#' Random Forest
#' 
#' This function creates a decision function based on the random forest algorithm
#' @export
#' @param training_data_x Matrix of inputs
#' @param training_data_x Matrix of outputs (of given inputs)
#' @param classification_tree Should the tree be a classification tree?
#' @export decision_function Decision Function used to predict output based on a given input

random_forest = function(training_data_x, training_data_y, classification_tree=FALSE) 
{
    tree <- greedy(training_data_x, training_data_y, classification_tree = classification_tree, random_subset = TRUE)
    decision_function <- bagging(tree, random_subset = TRUE)
    
    return(decision_function)
    
}