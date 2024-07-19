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
    data = "data.frame",
    length = "numeric",
    j = "numeric",
    s = "numeric",
    d = "numeric",
    type = "character"
  ),
  methods = list(
    initialize = function(length = 1024, dimension = 50, classificationType = FALSE) {
      rounded_length <- 2 ^ ceiling(log2(length))
      if (rounded_length != length) {
        suppressWarnings(length <- rounded_length)
        warning(paste0("Length increased to ",length, " (the nearest power of two)"))
      }
      .self$length <- length
      .self$data <- data.frame(index=seq(1:length),j=numeric(length),s=numeric(length))
      .self$type <- ifelse(classificationType, "classification", "regression")
      .self$d <- dimension
    },
    depth = function(index) floor(log2(index)) + 1,
    is_leaf = function(index) depth(index) == log2(.self$length),
    get_children = function(index) {
      firstChildIndex = index*2
      .self$data[firstChildIndex:(firstChildIndex+1)]
    },
    get_parent = function(index) {
      .self$data[floor(index * 0.5)]
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
    if (value > x$length) stop("Out of bounds!")
    x$data[value, ]
}
setMethod(f = "show",
          signature = "Tree",
          definition = function(object) {
            cat("Tree\n")
            print(c(type=object$type,
                    length=object$length,
                    depth=log2(object$length),
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
print.Tree = function(x) show(x)
