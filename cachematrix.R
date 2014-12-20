## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## Initalize the cached inverse to NULL
    m_inv<-NULL
    ## Fill the matrix with data and reset/initialize the cached inverse.
    set<-function(y){
        x<<-y
        m_inv<<-NULL
    }
    ## Get matrix data
    get<-function() x
    ## Set the (cached) inverse
    setinv<-function(solve) m_inv<<-solve
    ## Get the (cached) inverse
    getinv<-function() m_inv
    list(set=set, get=get,
        getinv=getinv,
        setinv=setinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get the cached inverse stored within the object
        m_inv<-x$getinv()
        ## Check if m_inv is NULL or not.
        ## If m_inv is not NULL, the inverse is currently cached, 
        ## just return it.
        if(!is.null(m_inv)) {
            message("getting cached data")
            return(m_inv)
        }
        ## If m_inv is NULL, get the matrix data
        data <- x$get()
        ## Calculate the inverse
        m_inv <- solve(data)
        ## Store the inverse in the cache
        x$setinv(m_inv)
        ## Return the inverse
        m_inv
}

