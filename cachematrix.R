## R Programming Assignment 2: Lexical Scoping
## Contributor:     P Schutte
## Gethub username: seq99543
## Date:            22 February 2015

## Get or set the cached matrix or inverse matrix objects

makeCacheMatrix <- function(x = matrix()) {
    ## Sanitise the inverse matrix object
    i <- NULL
    
    ## Set the matrix object in the global environment
    set <- function(y){
        x <<- y
        i <<- NULL
     }
    
    ## Get the matrix object from the global environment
    get <- function() x
    
    ## Set the inverse matrix object in the global environment
    setinverse <- function(solve(x)) i <<- solve(x)
    
    ## Get the inverse matrix object from the global environment  
    getinverse <- function() i
    
    ##Return a list with set, get, setinverse and getinverse matrix objects used in the calling environment
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Return cached matrix or inverse matrix objects - if the inverse matrix object does not exist, create an inverse matrix object and store in global environment

cacheSolve <- function(x, ...) {
    ## Assign the inverse of the matrix to variable i
    i <- x$getinverse()
    
    ## Test if x$getinverse() returned a cached inverse matrix object
    ## If yes, print message "getting cached data" and return the cached inverse matrix object
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## If x$getinverse() did not return a cached inverse matrix object, continue
    ## Assign matrix object to variable named matrix 
    matrix <- x$get()
    
    ## Assign inverse matrix object to variable named i 
    i <- solve(matrix)
    
    ## Set the value of the inverse matrix object in the global environment 
    x$setinverse(i)
    
    ## Return the inverse matrix object
    i
}
