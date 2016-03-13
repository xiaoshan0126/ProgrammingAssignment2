## The following two functions are written to cache the inverse of a matrix

## The first function, `makeCacheMatrix` creates a special "matrix" object 
## that can cache its inverse by 

## 1.  setting the value of the matrix
## 2.  getting the value of the matrix
## 3.  setting the value of the inverse of matrix
## 4.  getting the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function computes the inverse of 
## the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then 'cacheSolve' retrieves the inverse from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
