## makeCacheMatrix function creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    #creates a special "vector", which is 
    #really a list containing a function to:
    
    # 1. set the value of the matrix
    # 2. get the value of the matrix
    # 3. set the value of the inverted matrix
    # 4. get the value of the inverted matrix
    
    inv <- NULL #set matrix not inverted in cache
    set <- function(y) {
        #the <<- operator is used to assign a value 
        #to an object in an environment that 
        #is different from the current environment
        x <<- y #change value of matrix in global environment
        inv <<- NULL #set matrix not inverted in global environment
    }
    
    get <- function() x #return matrix
    setinverse <- function(inverse) inv <<- inverse #set value for inverted matrix in global environment
    getinverse <- function() inv #return value (or NULL) for inverted matrix
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse) #return list of current parameters
}



## CacheSolve function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## first the uninverted matrix goes through makeCacheMatrix
    
    #assign the inverse from makeCacheMatrix (either value or NULL)
    inv <- x$getinverse() 
    
    #check if inv has a value, then retrieve data from cache
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv) #return inverted matrix from cache
    }
    
    
    data <- x$get() # otherwise get matrix to invert
    inv <- solve(data, ...) # invert matrix
    x$setinverse(inv) # cache inverted matrix
    inv #return inverted matrix
}
