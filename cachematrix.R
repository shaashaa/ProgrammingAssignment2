## makeCacheMatrix function creates a list containing a function to 
## set the value of the matrix, get the value of the matrix,
## set the value of the matrix-inverse and get the value of the matrix-inverse
makeCacheMatrix <- function(x = matrix()){ 
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setmi <- function(solve) mi <<- solve
  getmi <- function() mi
  list(set = set, get = get,
       setmi = setmi,
       getmi = getmi)
}


## Cachesolve function computes the inverse of the special "matrix" returned 
## by above makeCacheMatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mi <- x$getmi()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setmi(mi)
  mi
}

#RESULTS##
#> d=matrix(1:4,2,2)
#> l<-makeCacheMatrix(d)
#> cacheSolve(l)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(l)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
