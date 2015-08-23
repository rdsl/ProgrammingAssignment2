# The functions make use of R's lexical scoping to cache the solution of a
# matrix inversion operation to save computation time.
# The <<- operator assigns to variables in the environment of the parent
# function, not the child's, thus used to save and preserve state.
#
# Testing code:
#               source('cachematrix.R')
#               m1 <- matrix(runif(9), 3, 3)
#               c1 <- makeCacheMatrix(m1)
#               cacheSolve(c1)
#               cacheSolve(c1)
#               m1 *%* cacheSolve(c1)
#
#  The second call to cachesolve will retrieve cached data.
#  The final call returns the identity matrix proving that cacheSolve returns
#  the inverse of m1.


makeCacheMatrix <- function(a = matrix()) {
        # Generates a closure in which to cache the inverse of the matrix
        # passed on at function call.
        #
        # Args:
        #       a: A matrix whose inverse will be cached.
        #
        # Returns:
        #       A list of functions: set, get, setinv, getinv
        #       which can be used by cacheSolve to set and retrieve the inverse
        #       of the matrix 'a'.

        m <- NULL
        set <- function(b) {
                a <<- b
                m <<- NULL
        }
        get <- function() a
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(c, ...) {
        # Return a matrix that is the inverse of 'a'.
        #
        # Args:
        #       A list returned by the function makeCacheMatrix
        #
        # Returns:
        #       The inverse matrix cached by 'c', if existing, of the matrix 'a'
        #  enclosed in the environment of 'c' by makeCacheMatrix. Or the newly
        # computed inverse when the function is called for the first time with
        # 'c', or when 'c' is changed with 'c$set(b)'. The inverse is then
        #  cached in 'c'.
        
        m <- c$getinv()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- c$get()
        m <- solve(data, ...)
        c$setinv(m)
        m
}
