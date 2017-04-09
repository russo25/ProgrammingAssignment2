## These are 2 functions that cache the inverse of a matrix
## This function creates a special kind of object that cache's the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ##assign the inv function the value of NULL
    inv <- NULL
    ##sets the values of the vectors below
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    ##Now, get the value of the vector as per direction example
    get <- function() x
    ##set the Inverse by assigning a value of "matrix solution" then invert it-inv
    setInverse <- function(matrix_solution) inv <<- matrix_solution
    ##next, get the value of the function using inv
    getInverse <- function()inv
    ##create a list per the directions, part 1 is now done!
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function will compute the inverse of the special matrix computed above by the makeCacheMatrix function
##Follows example from assignment for mean
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        ##IF inv is NOT NULL then invert it 
        if(!is.null(inv)){
          ##create a message to notify user work is ongoing - not required but more human friendly
          message("retrieving the cached data for you")
          ##now, it returns a value that is inverted
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
