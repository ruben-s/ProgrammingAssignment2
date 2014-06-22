## 

## The makeCacheMatrix function 
#
# arguments: a matrix
# 
# return:
# - set function - to initialise the encapsulated matrix
# - get function - to get the encapsulated matrix
# - setinv function - to set the inverse of the matrix
# - getinv function - to get the inverse of the encapsulated matrix
#
# makeCacheMatrix encapsulates the matrix that is passed to it as an argument
# the matrix contained can only be retrieved by calling the get() function
# the matrix can not be adressed directly
# to change the encapsulated matrix one needs to call the set() function
# the getinv() and setinv() functions can be used to retieve and set another matrix
# called inv_matrix
# HOWEVER there is no checking that the inv_matrix and x are actually
# indeed inverses of each other

makeCacheMatrix <- function(x = matrix()) {

# initialise a variable that will hold the inverse matrix
  inv_matrix <- NULL
  
# the set() function to re-initialise the encapsulated matrix
# howver the function does not verify that the argument passed is
# a) actually a matrix
# b) is a square matrix (non-square matrices do not have an inverse)
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }

# the get() function to retrieve the encapsulated matric
  get <- function() x

# the setinv() function to set the inv_matrix variable
# however the function does not verify that the argument passed is
# a) actually a matrix
# b) is a square matrix (non-square matrices do not have an inverse)
# c) is actually the inverse of x
  setinv <- function(inverse) inv_matrix <<- inverse

# the getinv() function to return the inv_matrix
  getinv <- function() inv_matrix

# the return of the makeCacheMatrix which is a list containing the above defined functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## The cacheSolve function retrieves the inverse of the makeCacheMatrix object
#
# the cacheSolve function checks to see if the inverse of the makeCacheMatrix object exists
# if it exists this is returned
# if the inverse does not exist yet is gets calculated and stored in the makeCacheMatrix object

cacheSolve <- function(x, ...) {
# fetch the inv_matrix from the passed makeCacheMatrix object
  inv_matrix <- x$getinv()
# if the fetched inv_matrix is defined (i.e. it is not null) then return this fetched matrix
# and end the function
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
# if we end up here, the fetched inv_matrix is not yet defined
# so we fetch the original matrix with the get() function
  matrix <- x$get()
# the inverse of the original matrix is calculated
  inv_matrix <- solve(matrix)
# the calculated inverse matrix is stored in the makeCacheMatrix object 
# through the function setinv()
  x$setinv(inv_matrix)
# the inverse matrix is returned
  inv_matrix
}
