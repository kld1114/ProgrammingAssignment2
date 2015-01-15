##The makeCacheMatrix function defines additional functions and creates a 
##special matrix that can be used by the cacheSolve function to calculate and 
##store the inverse of a matrix for later recall.

##The makeCacheMatrix function establishes a matrix to use for storing the inverse 
##matrix data and also defines functions required by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL               #creates an object,s, with value NULL
        set<-function(y){       #function assigns x and s in different environment  
                x<<-y
                s<<-NULL 
        }
        get <- function() x                ##function stores original matrix data
        setinverse<-function(solve) s<<-solve  ##function stores inverse matrix data
        getinverse<-function() s        ##function retrieves cached matrix data
        list(set=set, get=get,     
             setinverse=setinverse, 
             getinverse=getinverse)  ##Prints list showing function environments
}

## The cacheSolve function calculates and caches the inverse of a matrix or 
## retrieves a previously cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s<-x$getinverse()               ##Retrieves cached data for x, if existing 
        if(!is.null(s)){                        ##if s doesn't have a NULL value
                message("getting cached data")  ##print this message  
                return (s)                      ##print the cached inverse data                      
        }
        data<-x$get()           ##If s is NULL, retrieve the original matrix data 
        s<-solve(data,...)      ##Calculate the inverse of x 
        x$setinverse(s)         ##Cache the inverse for future reference
        s                       ##Print the inverse matrix results
}
