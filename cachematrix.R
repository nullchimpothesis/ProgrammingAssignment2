##PROGRAM---cachematrix.R------------------------------------------------------

##FUNCTION---makeCacheMatrix---------------------------------------------------
##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- matrix()        #initializes the inverse matrix
    set <- function(y) 
    {
      x <<- y              #caches the matrix
      inv <<- matrix()     #clears the cached inverse
    }
    get <- function()
    {
      x                    #return the matrix
    }
    setInverse <- function(inverse)
    {
      inv <<- inverse      #sets the inverse, called from cacheSolve
    }
    getInverse <- function()
    {
      inv                  #returns the cached inverse
    }
    #a list of the methods is returned
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}

##FUNCTION---cacheSolve--------------------------------------------------------
##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the 
##matrix has not changed), then the cacheSolve retrieves the inverse from the 
##cache instead of solving it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()     #get the cached inverse
        ##check the first cell of the matrix. If the cell is NA, is.numeric 
        ##will return false, and the inverse will be calculated. If is.numeric
        ##evaluates as true, the cached value will be returned.
        if(is.numeric(inv[1,1])) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()           #get the matrix        
        inv <- solve(data)        #calculate the inverse of the matrix
        x$setInverse(inv)         #set the cached value for inverse
        return(inv)               #return the inverse value
}
