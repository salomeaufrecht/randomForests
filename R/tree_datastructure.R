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
    length = "numeric",
    d = "numeric",
    type = "character"
  ),
  methods = list(
    initialize = function(length=16, max_length = 1024, dimension = 50, classificationType = FALSE) {
      stopifnot(length >= 1)
        # 1. Entry = Index, 2. Entry = j, 3. Entry = s
      .self$data <- matrix(rep(NaN, max_length*3), ncol=3)
      .self$data[1:length, 1] <- 1:length
      .self$length <- length
      .self$type <- ifelse(classificationType, "classification", "regression")
      .self$d <- dimension
    },
    
    depth = function(index) floor(log2(index)) + 1,
    
    exists = function(index) {
        if(index <= nrow(.self$data) && !is.nan(.self$data[index,1])) {
            return(TRUE)
        }
        #warning("Node does not exist")
        return(FALSE)
    },
    
    get_children = function(index) {
        return(
            .self$data[.self$get_children_index(index), ]
        )
    },
    
    get_children_index = function(index) {
      .self$exists(index)
      first_child_index <- index*2
      children <- NULL
      if(.self$exists(first_child_index)) {
          children <- first_child_index
      }
      if(.self$exists(first_child_index+1)) {
          children <- c(children, first_child_index+1)
      }
      if(is.null(children)) warning("Node is leaf node")
      return(children)
    },
    
    add_children = function(index, j1, s1, j2=NaN, s2=NaN) {
        stopifnot(all(sapply(c(j1, s1, j2, s2), is.numeric)))
        max_length <- nrow(.self$data)
        if(index+2 > max_length) {
           warning("Matrix exceeds max length. More space will be allocated")
           new_matrix <- matrix(1:max_length*2, ncol=3)
           new_matrix[1:max_length, ] <- .self$data[1:max_length, ]
           max_length <- max_length*2
           .self$data <- new_matrix
        } 
        
        .self$data[index*2, ] <- c(index*2, j1, s1)
        .self$length = .self$length+1
        if(!(is.na(j2) && is.na(s2))) {
            .self$data[index*2 + 1, ] <- c(index*2 + 1, j2, s2)
            .self$length = .self$length+1
        }
        
    },
    get_parent = function(index) {
      .self$data[floor(index * 0.5)]
    },
    
    is_leaf = function(index) {
        stopifnot(.self$exists(index))
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
#' @export `[.Tree`
#' @param x tree
#' @param value index of element
#' 
#' 

`[.Tree` <- function(x, value) {
    stopifnot(x$exists(value))
    x$data[value, ]
}

setMethod(f = "show",
          signature = "Tree",
          definition = function(object) {
            cat("Tree\n")
            print(c(type=object$type,
                    length=object$length,
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

