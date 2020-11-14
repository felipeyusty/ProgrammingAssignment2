## Write a short comment describing this function
## makeCacheMatrix() creates a special "matrix", which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        mat <- NULL
        setmat <- function(y) {
                x <<- y
                mat <<- NULL
        }
        
        getmat <- function() x
        setinvertible <- function(solve) mat <<- solve
        getinvertible <- function() mat
        list(setmat = setmat, getmat = getmat,
             setinvertible = setinvertible,
             getinvertible = getinvertible)
}


## Write a short comment describing this function
## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        mat <- x$getinvertible()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        
        data <- x$getmat()
        mat <- solve(data, ...)
        x$setinvertible(mat)
        mat
}

## Test
my_matrix <- matrix(rnorm(4),2,2)
matrix_obj <- makeCacheMatrix(my_matrix)
cacheSolve(matrix_obj)