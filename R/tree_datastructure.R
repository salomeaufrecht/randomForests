#' Tree data structure. Allows accessing elements via index,
#' getting child and parents elements, determining whether 
#' an element is a leaf and finding the depth of an element.
#' 
#' 
#' @export Tree
#' @param length integer, is rounded to the next power of 2
#' @param dimension integer, dimension of the input data
#' 
#' 


Tree <- setRefClass(
    "Tree",
    fields = list(
        data = "matrix",
        d = "numeric",
        type = "character",
        training_data_x = "matrix",
        training_data_y = "matrix"
    ),
    methods = list(
        initialize = function(training_data_x, training_data_y, classificationType = FALSE) {
            stopifnot("Training data does not match" = nrow(training_data_x) == nrow(training_data_y))
            length <- (2 ^ ceiling(log2(nrow(training_data_x)) )) 
            .self$data <- matrix(rep(NA_integer_, length*4), ncol=4)
            colnames(.self$data) <- c("index", "j", "s", "y")
            .self$data[1:length, 1] <- 1:length
            .self$type <- ifelse(classificationType, "classification", "regression")
            .self$d <- ncol(training_data_x)
            .self$training_data_x <- training_data_x
            .self$training_data_y <- training_data_y
        },
        
        depth = function(index) floor(log2(index)) + 1,
        
        exists = function(index) {
            if (length(index) > 1) return(.self$all_exist(index))
            if (index >= nrow(.self$data)) return(FALSE)
            return(any(!is.na(.self$data[index, 2:4])))
        },
        all_exist = function(indices) all(sapply(indices,.self$exists)),
        
        has_parent = function (index) ifelse(index==1, TRUE, !is.na(.self$data[floor(index * 0.5),2])),
        
        get_parent_index = function(index) return(floor(index*0.5)),
        
        get_parent = function(index) return(.self[.self$get_parent_index(index)]),
        
        extend = function(length_new) {
            max_length <- nrow(.self$data)
            if (length_new <= max_length) return()
            # TODO: Discuss if this warning is necessary
            warning("Max length exceeded. More space will be allocated.")
            length_new <- 2 * max_length + 1 # 2 ^ ceiling(log2(length_new))
            new_matrix <- matrix(rep(NA_integer_, 4*length_new), ncol=4)
            new_matrix[1:max_length, ] <- .self$data[1:max_length, ]
            new_matrix[1:length_new, 1] <- 1:length_new
            colnames(new_matrix) <-  c("index", "j", "s", "y")
            .self$data <- new_matrix
        },
        
        get_child_indices = function(index) {
            if (!.self$exists(index)) stop("Parent node does not exist")
            first_child_index <- index * 2
            indices <- NA_integer_
            if (.self$exists(first_child_index)) indices <- first_child_index
            if (.self$exists(first_child_index+1)) indices <- c(indices, first_child_index+1)
            if (all(is.na(indices))) warning("Node is leaf node")
            return(indices)
        },
        
        set_values = function(index, j=NA_integer_, s=NA_integer_, y=NA_integer_) {
            
        },
        
        get_children = function(index) {
            indices = .self$get_child_indices(index)
            .self[indices]
        },
        
        add_children = function(index, j1=NA_integer_, s1=NA_integer_, j2=NA_integer_, s2=NA_integer_, y1=NA_integer_, y2=NA_integer_) {
            stopifnot(all(sapply(c(j1, s1, j2, s2, y1, y2), is.numeric)))
            max_length <- nrow(.self$data)
            .self$extend(2*index + 1)
            .self$data[index*2, 2:4] <- c(j1, s1, y1)
            .self$data[index*2 + 1, 2:4] <- c(j2, s2, y2)
            return(c(index*2, index*2+1))
        },
        
        delete = function(index) {
            if (!.self$exists(index)) return()
            children = .self$get_children(index)
            .self$data[index, ] <- c(index,NA,NA)
            # Delete child nodes recursively
            if (!is.na(children[1])) .self$delete(children[1]$index)
            if (!is.na(children[2])) .self$delete(children[2]$index)
        },
        
        is_leaf = function(index) {
            if (!.self$exists(index)) stop("Node does not exist.")
            return(all(is.na(.self$get_child_indices(index))))
        },
        
        decide = function(X) {
            current_node <- 1
            while(!is_leaf(current_node)) {
                j <- .self$data[current_node, "j"]
                s <- .self$data[current_node, "s"]
                if(X[j] < s) {
                    current_node <- .self$get_child_indices(current_node)[1]
                } else {
                current_node <- .self$get_child_indices(current_node)[2]
                }
            }
            .self$data[current_node, "y"]
        },
        
        get_length = function() {
            return(sum(!is.na(.self$data[,2])))
        },
        
        plot_data = function() {
            X <- .self$training_data_x
            Y <- .self$training_data_y
            plot(X, Y, col='red')
            
            Y_hat <- sapply(X, \(x) .self$decide(c(x)))
            points(X, Y_hat)
        }
    )
)


#' Allow accessing the n-th element of the tree (equivalent to array representation)
#' getting child and parents elements, determining whether 
#' 
#' https://www.geeksforgeeks.org/array-representation-of-binary-heap/
#' 
#' @export `[.Tree`
#' @param tree tree
#' @param value index of element
#' 
#' 

`[.Tree` <- function(tree, value) {
    if (length(value) > 1) return(lapply(value, function(x) tree[x]))
    el <- tree$data[value,,drop=FALSE]
    if((length(el) == 1 && is.na(el[1])) || is.na(el[2])) return(NA)
    return(el)
}

#' Allows writing to the the n-th element of the tree.
#' Vectorised mass assignment possible.
#' Passing NA as a value will result in the deletion of a node and all its children.
#' 
#' @export `[<-.Tree`
#' @param tree tree
#' @param index index of element
#' @param value atomic vector of two real numbers (j, s)
#' 
#' 
`[<-.Tree` <- function(tree, index, value) {
    if (length(value) == 1 && is.na(value)) value = c(NA, NA)
    if (length(index) > 1) {
        if (!is.list(value)) stop("Mass assignment is only possible using lists of vectors.")
        sapply(1:length(index), function(x) tree[index[x]] <- value[[x]])
        return(tree)
    }
    if (!tree$has_parent(index)) stop("Parent does not exist!")
    if (!is.numeric(value) || !is.atomic(value) || length(value) != 2) {
        stop("Only atomic vectors of length three (j, s) are permitted as values.")
    }
    if (all(is.na(value))) {
        tree$delete(value)
        return(tree)
    }
    tree$extend(index)
    tree$data[index, 1:3] <- c(index,value)
    tree
}

setMethod(f = "show",
          signature = "Tree",
          definition = function(object) {
              cat("Tree\n")
              print(c(type=object$type,
                      nodes=sum(!is.na(object$data[,2])),
                      depth=object$d,
                      dimension=object$d),
              )
          }
)

#' Shows the type, length, depth and dimension of a tree. 
#' 
#' @export print.Tree
#' @param x tree
#' 
#' 
print.Tree <- function(x) show(x)
