## Here are 2 functions, makeCacheMatrix and cacheSolve, they will be used to
## execute the computation of inverse matrix with cache. We can compute the inverse
## matrix before we use it, when we need it, if we find that there is the cached 
## inverse matrix, we will get the inverse matrix from our cache, which is quickly

## A basic assumption is that the matrix you want to be operated on is always invertible

## The first function, makeCacheMatrix, will set a pattern to store the inverse matrix 
## m is the result we want and cache
## The out put is a list

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function(){
        x
    }
    
    setinv <- function(inv){
        m <<- inv
    } 
    
    getinv <- function(){
        m
    } 
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second function, cacheSolve, is a function to execute check and computation
## First it will check whether the input parameter, x, has got its inverse, which is
## stored in the cache, if so, it will directly get the inverse and return it; if not,
## it will get the data, then use the solve function to get the inverse, then return it

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }else{
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
    }
}

## An example to check the result
a = matrix(data = c(2,0,0,2),2,2)
## ans gives the true inverse matrix
ans = solve(a)
cacheInv = makeCacheMatrix(a)
## The first call will compute the inverse matrix 
cacheSolve(cacheInv)
## The second call will use the inverse matrix in the cache while skip computation
## The two calls will both return the true matrix
cacheSolve(cacheInv)
