## The first of the following two functions, makeCacheMatrix(), creates objects to hold   
## a matrix and its inverse. It returns a list of methods to set and get these objects. 
## The second function, cacheSolve(), checks to see if the inverse of the matrix has
## already been calculated. If yes, it retrieves the inverse matrix from the cache. If not,
## it calculates the inverse matrix and saves it in the cached object.


## This function returns a list of methods to set and get matrix as well as to
## cache the inverse of the matrix and retrieve the same.
makeCacheMatrix <- function(x = matrix()) {
         invMat <- NULL
         setMat <- function(mtrix) {
                 x <<- mtrix
                 invMat <<- NULL
         }
         getMat <- function() x
         setInvMat <- function(m) invMat <<- m
         getInvMat <- function() invMat
         list(setMat = setMat, getMat = getMat,
              setInvMat = setInvMat,
              getInvMat = getInvMat)
}


## This function retrieves the inverse of the matrix from cache
## if it has already been calculated. If not, it calculates and caches the inverse of the
## matrix and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		trix <- x$getInvMat()
        if(!is.null(trix)) {
                 message("getting cached data")
                 return(trix)
         }
         mtrix <- x$getMat()
         m <- solve(mtrix, ...)
         x$setInvMat(m)
         m
}
