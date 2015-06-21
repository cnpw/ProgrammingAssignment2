# 2 main functions:
# i)  makeCacheMatrix() function stores the inverse of a "special
#     matrix"
# ii) cacheSolve() function calls and retrieves the inverse of the
#     "special matrix" if it is stored in makeCacheMatrix(), if not
#     it will compute the inverse of the matrix and store it in
#     makeCacheMatrix()

# First function sets the "special matrix" and stores its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Functions to create and retrieve "special matrix"
  set_matrix <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  get_matrix <- function() x
  
  # Functions to store and retrieve the inverse
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  
  # Store all 4 functions in main function
  list(set_matrix = set_matrix, get_matrix = get_matrix, set_inv = set_inv, get_inv = get_inv)
}

# Second function calls and check if inverse is already stored
# in first function

cacheSolve <- function(x, ...) {
  
  # Return the inverse of "special matrix" if already stored in
  # makeCacheMatrix()
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If inverse is not stored, compute the inverse using the
  # solve() function and store in makeCacheMatrix() then call
  # the inverse of "special matrix"
  inv_data <- x$get_matrix()
  inv <- solve(inv_data, ...)
  x$set_inv(inv)
  inv
}

# Test with 2x2 matrix
mat <- matrix(c(2, -1, -1, 1), nrow = 2, ncol = 2)
a <- makeCacheMatrix(mat)
a$get_matrix()    # The "special matrix"
cacheSolve(a)     # Creates and stores inverse of "special matrix"
cacheSolve(a)     # Since inverse already stored, retrieve stored inverse
