#The functions created below help us for getting the inverse of a matrix that not necesarily is in the global environment.

## makeChachematrix creates a special matrix, which is really a list containing four functions (set,get,setInverse,getInverse).

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
          x <<- y
          inverse <<- NULL
    }
    get <- function(){
      x
    }
    setInverse <- function(inv){
      inverse <<- inv
    }
    getInverse <- function(){
      inverse
    }
    list(set = set, get = get, setInverse = setInverse, getInverse= getInverse)
}


## cacheSolve calculates the inverse of a special matrix, which was created at the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
          message("getting cached data")
          return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix,...)
    x$setInverse(inverse)
    inverse
}
