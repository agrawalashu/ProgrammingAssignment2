## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       ### Set Inver to Null ( to capture the value on Inver)
         inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        #### Return matrix with inverse of 'x'
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting inverse from cache !!!")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        inver
}
