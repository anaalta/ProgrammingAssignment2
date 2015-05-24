##The first function, makeCacheMatrix creates a special "matrix", 
##which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the matrix inverse
##get the value of the matrix inverse
  
  makeCacheMatrix <- function(x = matrix()){
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(mean) m <<- mean
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
  }
##This function computes the inverse of "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
  cacheSolve <- function(x=matrix(), ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    matrix <- x$get()
    m<-solve(matrix, ...)    
    x$setmatrix(m)
    m       ## Return a matrix that is the inverse of 'x'
  }

