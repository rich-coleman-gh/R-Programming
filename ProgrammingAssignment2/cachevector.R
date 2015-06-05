############################Vector Mean#############################
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y #set x to our input vector
    m <<- NULL # the mean has not been set yet
  }
  get <- function() x #returns input vector
  setmean <- function(mean) m <<- mean #define function that will call solve to get mean of input vector
  getmean <- function() m #define function that will retrieve mean inside of cache envirionment 
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean<- function(x, ...) {
  m <- x$getmean() #call on cache in vector to see if mean has been defined
  if(!is.null(m)) {
    message("getting cached data") #if mean has been defined we stop computation and return cached value
    return(m)
  }
  data <- x$get() #mean has not been defined so we retrive input matrix from environment in makeVector
  m <- mean(data, ...) # get the mean of input vector
  x$setmean(m) #set the mean inside the makeVector enviornment for future function calls
  m
}

#test
b = c(2, 4, 3, 1)
mean(b)

cache <- makeVector(b)
cachemean(cache)
cachemean(cache)