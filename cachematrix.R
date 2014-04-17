#This program keep in memory the result of an inverse of a matrix

#The function makeCacheMatrix create a special matrix and has 4 sub functions
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        #sub function 1 set: to set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #sub function 2 get: to get the value of the matrix
        get <- function() x
        
        #sub function 3 setsolve: to set the value of the inverse of the matrix
        setsolve <- function(solve) 
                m <<- solve
        
        #sub function 4 getsolve: to get the value of the inverse of the matrix
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

#The function cacheSolve calculates the inverse of a matrix created with the makeCacheMatrix
cacheSolve <- function(x, ...) {
        #If the inverse was calculated before, return the value  
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #Else calculate the inverse of the matrix and add it to the list
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

