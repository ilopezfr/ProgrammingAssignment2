## Calculating the inverse of a matrix is a costly computation. 
## There’s a benefit in catching the inverse of a matrix rather 
## than compute it repeatedly. 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## This is really a list containing a function to: 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {             #Define function name and argument
        s <- NULL                                       #set the variable s to NULL
        set <- function(y) {                            #Define the different functions
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve         #Define the funcion solve and assign it to the variable s
        getsolve <- function() s                        #get the s variable through getsolve function
        list(set = set, get = get,                      #list of functions available in makeCacheMatrix 
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve computes the inverse of the special “matrix”. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setsolve(s)
        s                                        ## Return a matrix that is the inverse of 'x'
}


       
