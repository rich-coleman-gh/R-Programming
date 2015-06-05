############################Matrix Inverse#############################
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y #set x to our input matrix
    m <<- NULL # the inverse has not been set yet
  }
  get <- function() x #returns input matrix
  setinverse <- function(solve) m <<- solve #define function that will call solve to get inverse of input matrix
  getinverse <- function() m #define function that will retrieve inverse inside of cache envirionment 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve<- function(x, ...) {
  m <- x$getinverse() #call on cache in matrix to see if inverse has been defined
  if(!is.null(m)) {
    message("getting cached data") #if inverse has been defined we stop computation and return cached value
    return(m)
  }
  data <- x$get() #inverse has not been defined so we retrive input matrix from environment in makeCacheMatrix
  m <- solve(data, ...) # get the inverse of input matrix
  x$setinverse(m) #set the inverse inside the makeCacheMatrix enviornment for future function calls
  m
}

#test
B = matrix(c(2, 4, 3, 1), nrow=2, ncol=2) 
solve(B)

cache <- makeCacheMatrix(B)
cacheSolve(cache)
cacheSolve(cache)