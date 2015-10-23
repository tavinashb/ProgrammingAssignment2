## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() - function creates the list of following functions -
## 1. set() - set the value of matrix  
## 2. get() - return the value of matrix
## 3. setinverse() - set the inverse value of matrix
## 4. getinverse() - return the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() {
        x
    } 
    
    setinverse <- function(matrix_inverse){
        i <<- matrix_inverse
    } 
    
    getinverse <- function() {
        i
    } 
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve() - function accepts input matrix then checks if the inverse of 
## matrix exists in cache or not. If inverse exists in cache then this function 
## return cached matrix inverse and skips the inverse computation. 
## If inverse of matrix does not exist in cache then this function computes and 
## store the inverse of matrix in cache.

cacheSolve <- function(x, ...) {
        
    i <- x$getinverse() 
    
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    matrix_data <- x$get()
    i <- solve(matrix_data)
    x$setinverse(i)
    i
}

# sample run -
# > m <- makeCacheMatrix(rbind(c(4,7), c(2,6)))
# > cacheSolve(m)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4    
