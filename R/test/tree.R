source("R/tree_datastructure.R")

testTreeDatastructure = function() {
  cat("Testing Tree datastructure...\n")
  test <- Tree$new(as.matrix(iris[1:3]), as.matrix(iris[4]))

  
  print("Assign element 1 & 2. Element 1:")
  test[1] <- c(1,2.1)
  test[2] <- c(2,3)
  print(test[1])
  cat("\nShould be: 1, 5, 6\n\n")
  cat("Element at index: 5\n")
  print(test[5])
  cat("Should be: NA\n\n")

  cat("Test mass access. Print elements at index two through five:\n")
  print(test[c(2,3,5)])

  cat(paste0("\nTest mass assignment and automatic extension. Length before: ", test$get_length(),"\n"))
  test[c(3,4,5,6,7)] = rep(list(c(1,1)),5)
  cat("Inserted elements (should all have j=1 and s=1):\n")
  print(test[c(3,4,5,6,7)])
  cat(paste0("\nLength after: ",test$get_length(), "Should be: 7\n\n"))

  cat(paste0("Index of element 2: ", test[5][1]))
  cat("\nShould be: 5\n\n")
  cat(paste0("Depth of element 3: ",test$depth(3)))
  cat("\nShould be: 2\n\n")
  cat(paste0("Is element 3 a leaf? ",test$is_leaf(3)))
  cat("\nShould be: FALSE\n\n")
  cat(paste0("Is element 6 a leaf? ",test$is_leaf(6)))
  cat("\nShould be: TRUE\n\n")


}
testTreeDatastructure()

