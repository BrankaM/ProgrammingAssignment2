
## This function creates a list containing a function to
## 1. set the matrix
## 2. get the matrix
## 3. set the invers of the matrix
## 4. get the invers of the matrix

makeCacheMatrix <- function(x=matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invMat) inv <<- invMat
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

## The following function calculate invers of matrix
## of the special "vector" created with the above function.
## It first checks to see if the invers matrix has already been calculated.
## If so, it gets the invers from the cache and skips the computation.
## Otherwise, it calculates the invers of the data and sets the value of the invers
## in the cache via the setinv function.

cacheSolve <- function(x,...){
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return (inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
