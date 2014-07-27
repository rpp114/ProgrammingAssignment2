## the makeCacheMatrix saves the input matrix in x 
## and the inverse as calculated by the cacheSovle function of it in m.  

## The cacheSovle function looks for the value
## stored in m.  If null then solves inverse(x) and stores in m.
## if !NULL returns value in m.  
## m becomes a universal value that can be called from the second function.


## makeCacheMatric - Takes the input matrix and stores the matrix in x 
## and the inverse in m.  This keeps m available for the cacheSolve function
## It allows the cacheSolve to find m when it has been previously set.  


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse ## What goes on Here??
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## Looks to see if the inverse has been calculated and returns
## the cached number if so, if not, calculates the inverse and stores it
## into the m variable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
