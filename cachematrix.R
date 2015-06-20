# 2 main functions:
# i) makeCacheMatrix function stores the inverse of a matrix
# ii) cacheSolve function calls to see if the inverse is stored,
#     if not, it will find the inverse of the matrix stored
#     in the makeCacheMatrix function

# First function calls the matrix and stores its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Functions to create and retrieve created matrix
  set_matrix <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  get_matrix <- function() x
  
  # Functions to store and retrieve the stored inverse value
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  
  # Store all 4 functions in main function
  list(set_matrix = set_matrix, get_matrix = get_matrix, set_inv = set_inv, get_inv = get_inv)
}

# Second function calls and check if inverse is already stored
# in first function

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of matrix 'x'
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not returned, inverse is not stored thus finds
  # the inverse and store in first function then call inverse
  inv_data <- x$get_matrix()
  inv <- inverse_function(inv_data, ...)
  x$set_inv(inv)
  inv
}
