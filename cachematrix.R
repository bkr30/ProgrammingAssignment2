# Assignment-Cache the Inverse of a Matrix

# As per our course instructions for this assignment, 
# "matrix inversion is usually a costly computation 
# and there may be some benefit to caching the inverse 
# of a matrix rather than compute it repeatedly."

# Per course instructions, this function creates a 
# matrix object that can cache its inverse.

# We will use the makeCacheMatrix function to create 
# a list of functions that can cache the inverse of 
# the matrix object created.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL 
    set <- function(y) { 
      x <<- y 
      m <<- NULL 
    }
    get <- function() x
    setinvmatrix <- function(solve) m <<- solve
    getinvmatrix <- function() m
    list(set = set, get = get,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}

# cacheSolve is a function that computes the inverse 
# of the matrix returned by makeCacheMatrix.
# cacheSolve will retrieve the inverse from the cache 
# if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinvmatrix(m)
  m
}

# Thanks to all who posted on the Discussion Board.
# I pieced most of this together reading through all the posts. 