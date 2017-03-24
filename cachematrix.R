## This function calculates and stores inverse matrix in cache.


##The makeCacheMatrix function is created to store the matrix on
## which you want to do calculation and store the inverse matrix if
## already calculated. 
## When you assign this function to a variable (e.g. Cache) using 
## Cache<-makeCacheMatrix(), Cache is a list that contains 4 child 
## functions. 
## You can use Cache$set() to save the original matrix of
## which you want to calculate the inverse.
## Cache$get() is used to examine whether Cache have saved any matrix
## or not. 
## Cache$setinverse() can be used to mannually set and 
## store the inverse matrix. 
## Cache$getinverse() is to retrieve the inverse matrix so
## that you don't need to calculate repeatedly. 
 
makeCacheMatrix <- function(x = matrix()) {
 	ivs <- NULL
    set <- function(y) {   
        x <<- y
        ivs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) ivs<<-inverse
    
    getinverse <- function() ivs
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## When you pass the list argument (e.g. Cache from the above example)
## to the cacheSolve function, The cacheSolve function first check if
## the inverse matrix have been calculated and stored in Cache. If
## yes, it gets the inverse matrix from Cache directly. Otherwise, it
## calculates the inverse matrix and save the inverse matrix to Cache.
## Finally it returns the inverse matrix. 

cacheSolve <- function(x, ...) {
    ivs <- x$getinverse()
    if(!is.null(ivs)) {
        message("getting cached data")
        return(ivs)
    }
    data <- x$get()
    ivs <- solve(data,...)
    x$setinverse(ivs)
    ivs
}

