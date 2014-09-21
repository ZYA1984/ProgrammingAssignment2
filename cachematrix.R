##The two functions are to caching the inverse of a matrix rather than computing it repeatedly. 


## This function is to create the methods associated with an object(S4). These methods will be used in the function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## m will store the inverse and it's reset to NULL every time makeCacheMatrix is called
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x   # this function returns the value of the original matrix
        setinverse <- function(inverse) m <<- inverse   #it will store the value using superassignment
        getinverse <- function() m   #this will return the cached value
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)  #This list consists of the created object.

}


## This function is to compute the inverse of a matrix only when the inverse has not been calculated.

cacheSolve <- function(x, ...) {    #the input is an object created by makeCacheMatrix 
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } # send the message to console if the inverse exists
        data <- x$get()   #Take the matrix if the inverse doesn't exist
        m <- solve(data) #Calculate the inverse
        x$setinverse(m) "Set the cache value for the inverse
        m
}



