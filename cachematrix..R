## This programming assignment intends to illustrate how Lexical Scoping 
## in R works. 
## The functions are implemented to handle invertible matrices.
## Variable m is declared uniquely in both functions and are allocated 
## separate addresses in memory. In function makeCacheMatrix()
## variable m is declared immediately and assigned the value NULL 
## using the standard assignment operator (<-). However, the “set” functions 
## defined within the containing makeCacheMatrix() function require the 
## special assignment operator (<–) to update the value of variable m; it 
## is important for variable m to be declared and initialised by 
## makeCacheMatrix(). Had functions set() and setmatrix() not used the 
## special assignment operator, these functions would have allocated memory 
## to store the value and labelled the address as m. The variables 
## named m would effectively be isolated and distinct variables.


## The function makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.
## It creates an environment with two variables, one for storing the 
## matrix and one for storing its inverse. 
## This environment also contains four functions for getting and setting 
## these variables. These four functions keep a reference to the 
## environment in which they are declared.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversematrix <- function(solve) m <<- solve
        getinversematrix <- function() m
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}


## The function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## It takes the list of four functions as an argument. 
## All it does is check to see if the inverse is already stored in the 
## environment created by makeCacheMatrix and if not calculate 
## the mean and store it using a getInverse() function. 
## cacheSolve does not create the parent environment of makeCacheMatrix. 
## In fact, all of this can be done without cacheSolve by building the 
## cache check and inverse calculation into a getInverse() function directly.

cacheSolve <- function(x, ...) {
        m <- x$getinversematrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversematrix(m)
        m
}


amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()         # Returns original matrix
cacheSolve(amatrix)   # Computes, caches, and returns matrix inverse
amatrix$getinverse()  # Returns matrix inverse
cacheSolve(amatrix)   # Returns cached matrix inverse using previously 
			    # computed matrix inverse
                      # getting cached data
amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
amatrix$get()         # Returns matrix
amatrix$getinverse()  # Returns matrix inverse
