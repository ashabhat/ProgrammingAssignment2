##This program has 2 functions for inversing a matrix, using the concepts of lexical scoping 
##Function 1(makeCacheMatrix) - It does 4 things. Setting & getting the matrix. Setting & getting the matrix inverse
##Function 2(cacheSolve) - It calculates the matrix inverse using the library function solve()
##PS-It calculates the inverse only if for _that_ instance the inverse isn't already calculated. 
##If it is, it will get the inverse from the cache

## It sets & gets the matrix. It sets and gets the inverse(from the cache, if its there)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
  
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(mi) m <<- mi
  
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse(if it isnt already calculated) using the solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mtx_inv <- x$getinverse()
  
  if (!is.null(mtx_inv)){
    message("getting cached data")
    return(mtx_inv)
  }
  
  data <- x$get()
  
  mtx_inv <- solve(data, ...)
  
  x$setinverse(mtx_inv)
  
  mtx_inv
}
