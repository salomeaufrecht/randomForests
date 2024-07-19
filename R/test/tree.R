source("R/tree_datastructure.R")

testTreeDatastructure = function() {
  cat("Testing Tree datastructure...\n")
  test <- Tree$new(len=10)
  element5 <- test[5]
  print(test)
  print(element5)
  cat(paste0("Index of element 5: ", test[5]$index))
  cat("\nShould be: 5\n\n")
  cat(paste0("Depth of element 3: ",test$depth(3)))
  cat("\nShould be: 2\n\n")
  cat(paste0("Is element 5 a leaf? ",test$is_leaf(5)))
  cat("\nShould be: FALSE\n\n")
  cat(paste0("Is element 14 a leaf? ",test$is_leaf(14)))
  cat("\nShould be: TRUE\n\n")
}
testTreeDatastructure()

