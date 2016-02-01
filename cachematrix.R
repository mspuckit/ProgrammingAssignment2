## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse. Including creating a few functions to set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
        ##m will be the inverse matrix
        m <- NULL
        set<- function(y){
                x <<- y
                m <<- NULL
        }
        ##Method to Get the matrix
        get<- function() x
        
        ##Method to Set the inverse of the matrix
        setInverse <-function(inverse) m <<-inverse

        ##Method to Get the inverse of the matrix
        getInverse <-function() m
        
        ##Return a list of fuctions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of a "matrix" returned by makeCacheMatrix above.	
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should return the inverse from the cache.	

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a mextrix that is the inverse of x 
        m <- x$getInverse()
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ## this will get the matrix data
        data <- x$get()
        
        ##Use solve to calcuare the inverse of the matrix
        m<- solve(data)
        
        ##Set the inverse of hte matrix to x
        x$setInverse(m)
        
        ##return the inverse
        m
}
