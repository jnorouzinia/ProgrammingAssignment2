## MakeCacheMatrix and cacheSolve functions reports the inverse of input matrix.
## If the inverted matrix is available since previous computations, it 
## does not repeat it and uses the cashed result.


## MakeCacheMatrix recieves a matrix and make closure for the function. If it 
## is the first time that you run the function, the output of "get" function 
## would be the input and output of "getinv" would be NULL. 


makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInv <- function(Inverse) Inv <<- Inverse
        getInv <- function() Inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## By running cacheSolve function,
## it tries to check if the inverse of matrix is available
## or not. If is not, it calculate it and will show the result, and if is, it
## prints "getting cached data" and then the inverse with repeating the 
## computation.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getInv()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        Inv <- solve(x$get())
        x$setInv(Inv)
        Inv
}