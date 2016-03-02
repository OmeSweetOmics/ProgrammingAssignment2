## These functions reutrns the inverse of the input matrix. It saves time by
## caching the matrix and then creating functions to allow for the 
## cached matrix to be changed or retrieved; It also creates functions that  
## cache or retrieve the inverse of the matrix. 
 
## It saves time because it calculates the inverse of the matrix only once, if 
## it calculates it at all. 

## It only calculates the inverse when (and if) it actually needs to and then
## it caches it for future use so it will not need to calculate it again.

## Initially makeCacheMatrix caches the input matrix and sets the 
## inverse of the matrix equal to NULL. 
## It then defines four functions
##  1. to change the input matrix  
##  (also setting the stored value = NULL to erase any previously stored values) 
##  2. retrieve the input matrix 
##  3. to cache the inverse of the input matrix, and 
##  4. to retrieve the stored inverse of the input matrix. 
## It performs these functions using $set, $get, $setinv and $getinv, respectively.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
    }

## cacheSolve reutrns the inverse of the input matrix, however it saves time
## because it first checks if the inverse of a matrix has already been 
## cached.  If the inverse has already been cached it outputs the cached
## value.  If the inverse has not been cached then it computes the inverse 
## of the matrix and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}