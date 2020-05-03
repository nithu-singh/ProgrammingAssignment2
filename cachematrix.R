## Below are a pair of functions that cache the inverse of a matrix

#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = Matrix()) {
  Mx_inv <- NULL #Here Mx_inv is the inverse of the square invertible matrix.
  set <- function(y) {
    x <<- y
    Mx_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) Mx_inv <<- inv
  getinv <- function() Mx_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  Mx_inv<- x$getinv() #check if inverse of a matrix already exists
  if(!is.null(Mx_inv)) { #if yes, use the cache data
    message("getting cached data")
    return(Mx_inv)
  }
  data <- x$get()
  Mx_inv <- solve(data, ...)
  x$setinv(Mx_inv)
  Mx_inv ## Return a matrix that is the inverse of 'x'
}
