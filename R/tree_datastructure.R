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
        training_data_y = "matrix",
        order_matrix = "matrix",
        inverse_order_matrix = "matrix",
        risk = "numeric"
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
            
            .self$order_matrix <- matrix(
                sapply(1:d, \(x) order(training_data_x[, x])),
                ncol = d
            )
            
            .self$inverse_order_matrix <- matrix(
                sapply(1:d, \(x) order(order_matrix[, x])),
                ncol = d
            )
        },
        
        depth = function(index) floor(log2(index)) + 1,
        
        copy = function(nodes=NULL){
            t <- Tree$new(.self$training_data_x, .self$training_data_y)
            t$type <- .self$type
            t$d <- .self$d
            if(is.null(nodes)){
                t$data <- .self$data
                t$risk <- .self$risk
            } else{
                t$risk <- numeric(0)
                if(length(nodes)==1){
                    t$data <- matrix(.self$data[nodes, ], 
                                                       ncol= 4, byrow=TRUE, 
                                                       dimnames = list(c(1),names(.self$data[nodes, ])))
                } else t$data <- .self$data[1:max(nodes), ]
                
            }
            return(t)
        },
        
        exists = function(index) {
            if (length(index) > 1) return(.self$all_exist(index))
            if (index > nrow(.self$data)) return(FALSE)
            return(any(!is.na(.self$data[index, 2:4])))
        },
        all_exist = function(indices) all(sapply(indices,.self$exists)),
        
        has_parent = function (index) ifelse(index==1, TRUE, !is.na(.self$data[floor(index * 0.5),2])),
        
        get_parent_index = function(index) return(floor(index*0.5)),
        
        get_parent = function(index) return(.self[.self$get_parent_index(index)]),
        
        extend = function(length_new) {
            max_length <- nrow(.self$data)
            if (length_new <= max_length) return()
            length_new <- 2 ^ ceiling(log2(length_new))
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
            return(indices)
        },
        
        set_values = function(index, j=NA_integer_, s=NA_integer_, y=NA_integer_, recalcRisk = TRUE) {
            .self$data[index,c('j', 's', 'y')] <- c( j, s, y)
            if(recalcRisk) .self$calc_risk(force=TRUE)
        }, 
        
        get_children = function(index) {
            indices = .self$get_child_indices(index)
            .self[indices]
        },
        
        add_children = function(index, j1=NA_integer_, s1=NA_integer_, j2=NA_integer_, s2=NA_integer_, y1=NA_integer_, y2=NA_integer_, recalcRisk = TRUE) {
            stopifnot(all(sapply(c(j1, s1, j2, s2, y1, y2), is.numeric)))
            max_length <- nrow(.self$data)
            .self$extend(2*index + 1)
            .self$data[index*2, 2:4] <- c(j1, s1, y1)
            .self$data[index*2 + 1, 2:4] <- c(j2, s2, y2)
            if(recalcRisk) .self$calc_risk(force=TRUE)
            return(c(index*2, index*2+1))
        },
        
        delete = function(index, recalcRisk = TRUE) {
            if (!.self$exists(index)) return()
            children = .self$get_children(index)
            .self$data[index, ] <- c(index,NA,NA)
            # Delete child nodes recursively
            if (!is.na(children[1])) .self$delete(children[1]$index)
            if (!is.na(children[2])) .self$delete(children[2]$index)
            if(recalcRisk) .self$calc_risk(force=TRUE)
        },
        
        #' test
        is_leaf = function(index) {
            if (!.self$exists(index)) stop("Node does not exist.")
            return(all(is.na(.self$get_child_indices(index))))
        },
        
        decide = function(x) {
            current_node <- 1
            while(!is_leaf(current_node)) {
                j <- .self$data[current_node, "j"]
                s <- .self$data[current_node, "s"]
                if(x[j] < s) {
                    current_node <- .self$get_child_indices(current_node)[1]
                } else {
                current_node <- .self$get_child_indices(current_node)[2]
                }
            }
            return(unname(.self$data[current_node, "y"]))
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
            plot_split_lines()
        },
        
        plot_split_lines = function(index=1, recursive=TRUE) {
            if (is.na(index) || !.self$exists(index)) return()
            split_x = .self$data[index, "s"]
            abline(v=split_x)
            if (!recursive) return()
            children <- get_child_indices(index)
            plot_split_lines(children[1])
            plot_split_lines(children[2])
        },

        
        get_leaf_indices = function(subtree=NULL){
            if(!is.null(subtree)) return(subtree[! subtree %in% .self$get_parent_index(subtree)])
            data_ <- .self$data
            nodes <- which(!is.na(data_[ ,'y']))
            if(is.na(data_[1, 'y'])) nodes <- c(1, nodes)
            leaves <- nodes[is.na(data_[nodes, 's'])]
            pot_leaves_with_s <- nodes[!is.na(data_[nodes, 's'])]
            for (pl in pot_leaves_with_s){
                if(all(is.na(.self$get_child_indices(pl)))) leaves <- c(leaves, pl)
            }
            return(as.numeric(leaves))
        },
        
        f = function(x){
            node=1
            while (!.self$is_leaf(node)) {
                children <- .self$get_child_indices(node)
                node <- ifelse(.self$data[node, 's'] > x[.self$data[node, 'j']], children[1], children[2])
            }
            return(unname(.self$data[node, 'y']))
        },
        
        calc_risk = function(x_mask=.self$get_x_mask(), force=FALSE, subtree=NULL){
            if(is.null(subtree) && !is.null(.self$risk) && !force && !identical(numeric(0), .self$risk)) return(.self$risk)
            risk_ <- 0
            if(is.null(subtree))leaves <- .self$get_leaf_indices()
            else leaves <- subtree[! subtree %in% .self$get_parent_index(subtree)]
            
            if (.self$type == "classification"){
                for(l in leaves){
                    risk_ <- risk_+ sum((.self$training_data_y[x_mask[[l]]] != .self$data[l, 'y']))
                }
            }
            else{
                for(l in leaves){
                    risk_ <- risk_+ sum((.self$training_data_y[x_mask[[l]]]-.self$data[l, 'y'])^2)
                }
            }
            risk_ <- risk_/length(.self$training_data_x)
            if(is.null(subtree)) .self$risk <- risk_
            return(risk_)
        },
        
        set_risk = function(risk){ .self$risk <- risk},
         
        get_x_mask = function(){
            x_mask <- list(0) #which x values 'go this way'
            x_mask[[1]] <- rep(TRUE, nrow(.self$training_data_x))
            parents <- .self$data[!is.na(.self$data[, 's']), 'index']
            parents <- parents[!parents %in% .self$get_leaf_indices()]
            for(p in parents){
                child_indices <- .self$get_child_indices(p)
                x_mask[[child_indices[1]]] <- .self$training_data_x < .self$data[p, 's'] & x_mask[[p]]
                x_mask[[child_indices[2]]] <- !x_mask[[child_indices[1]]] & x_mask[[p]]
            }
            return(x_mask)
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
    if((length(el) == 1 && is.na(el[1])) ) return(NA) #TODO || is.na(el[2]) removed 
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
              (object$get_leaf_indices()) |> unique() -> leaves
              depth <- max(object$depth(leaves))
              
              print(c(type=object$type,
                      nodes=sum(!is.na(object$data[,'y'])),
                      depth=depth,
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
