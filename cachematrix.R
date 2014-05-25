## makeCacheMatrix creates a list that holds the already computed matrix - inverse matrix pairs.
## 

## makeCacheMatrix takes a matrix as an input and returns a list which contains the cached data if it already exists.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL ## sets cache empty in this environment
  set <- function(y) {  
    x <<- y
    m <<- NULL
  }  ## Function for putting the cache value into x
  get <- function() x ##Returns precalculated value
  setinverse <- function(solve) m <<- solve ##computes the inverse and puts it in m, the cache
  getinverse <- function() m ## returns newly computed value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## output holding data defined above in a list
  
}


## cacheSolve takes as input the output of makeCacheMatrix. It calculates the inverse matrix and stores it, so in case it needs to be recomputed it can be recalled from memory.

cacheSolve <- function(x, ...) {
  m <- x$getinverse() ##sets m as the getinverse value from the list (4th), that is filled if data is precalculated.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() ##if m is empty then retieve the original input matrix
  m <- solve(data, ...) ## m holds the inverse matrix
  x$setinverse(m) #store the result in x
  m ## output answer
}
