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
    type = "character"
  ),
  methods = list(
    initialize = function(length, dimension = 50, classificationType = FALSE) {
      length <- 2 ^ ceiling(log2(length))
        # 1. Entry = Index, 2. Entry = j, 3. Entry = s
      .self$data <- matrix(rep(NA, length*3), ncol=3)
      .self$data[1:length, 1] <- 1:length
      .self$type <- ifelse(classificationType, "classification", "regression")
      .self$d <- dimension
    },
    
    depth = function(index) floor(log2(index)) + 1,
    
    exists = function(index) {
      if (length(index) > 1) return(.self$all_exist(index))
      index <= nrow(.self$data) && (!is.na(.self$data[index, 2]) || !is.na(.self$data[index, 3]))
    },
    all_exist = function(indices) all(sapply(indices,.self$exists)),

    has_parent = function (index) ifelse(index==1, TRUE, !is.na(.self$data[floor(index * 0.5),2])),

    get_parent = function(index) .self[floor(index * 0.5)],

    extend = function(length_new) {
      max_length <- nrow(.self$data)
      if (length_new <= max_length) return()
      # TODO: Discuss if this warning is necessary
      # warning("Max length exceeded. More space will be allocated.")
      length_new <- 2 ^ ceiling(log2(length_new))
      new_matrix <- matrix(rep(NA, 3*length_new), ncol=3)
      new_matrix[1:max_length, ] <- .self$data[1:max_length, ]
      new_matrix[1:length_new, 1] <- 1:length_new
      .self$data <- new_matrix
    },
  
    get_child_indices = function(index) {
      if (!.self$exists(index)) stop("Parent node does not exist")
      first_child_index <- index * 2
      indices <- NA
      if (.self$exists(first_child_index)) indices <- first_child_index
      if (.self$exists(first_child_index+1)) indices <- c(indices, first_child_index+1)
      if (all(is.na(indices))) warning("Node is leaf node")
      return(indices)
    },
  
    get_children = function(index) {
        indices = .self$get_child_indices(index)
        .self[indices]
    },
    
    add_children = function(index, j1, s1, j2=NA, s2=NA) {
        stopifnot(all(sapply(c(j1, s1, j2, s2), is.numeric)))
        max_length <- nrow(.self$data)
        .self[index*2] <- c(j1, s1)
        if (!is.na(j2) || !is.na(s2)) {
            .self[index*2 + 1] <- c(j2, s2)
        }
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
        return(!.self$exists(index*2))
    },
    
    decide = function(index, training_data) {
        stopifnot("Index is out of bounds! " = .self$data[index, 2] <= length(training_data))
        if (index == 1) return(TRUE)
        return(training_data[.self$data[index, 2]] >= .self$data[index, 3]) 
    }
  )
)

#' Allows accessing the n-th element of the tree (equivalent to array representation)
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
    if (value > nrow(tree$data)) return(NA)
    el <- tree$data[value, ]
    if((length(el) == 1 && is.na(el)) || is.na(el[2])) return(NA)
    return(c(index=el[1], j=el[2], s=el[3]))
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
    stop("Only atomic vectors of length two (j, s) are permitted as values.")
  }
  if (all(is.na(value))) {
    tree$delete(value)
    return(tree)
  }
  tree$extend(index)
  tree$data[index, ] <- c(index,value)
  tree
}

setMethod(f = "show",
          signature = "Tree",
          definition = function(object) {
            cat("Tree\n")
            print(c(type=object$type,
                    nodes=sum(where(!is.na(object$data[,2]))),
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
