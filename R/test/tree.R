source("R/tree_datastructure.R")

testTreeDatastructure = function() {
  cat("Testing Tree datastructure...\n")
  test <- Tree$new(length=10)
  element5 <- test[5]
  print(element5)
  cat(paste0("Index of element 5: ", test[5][1]))
  cat("\nShould be: 5\n\n")
  cat(paste0("Depth of element 3: ",test$depth(3)))
  cat("\nShould be: 2\n\n")
  cat(paste0("Is element 5 a leaf? ",test$is_leaf(5)))
  cat("\nShould be: FALSE\n\n")
  cat(paste0("Is element 6 a leaf? ",test$is_leaf(6)))
  cat("\nShould be: TRUE\n\n")
  cat("\nGive children to 6\n")
  test$add_children(6, 1, 1, 2, 2)
  cat("\nChildren of 6: \n")
  print(test$get_children(6))
}
testTreeDatastructure()

