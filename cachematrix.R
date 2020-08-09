## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## set the value of the matrix, get the value of the matrix ,set the value of the inverse, get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inver<-NULL
        ## set the value of the matrix
        set<-function(y){
                x<<-y
                inver<<-NULL                
        }
        
        ##get the value of the matrix
        get<-function() x
        
        ##set the value of the inverse
        setInverse<-function(inverse) inver <<-inverse
        
        ##get the value of the inverse
        getInverse<-function() inver
        list(set=set,get=get, setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" created by above function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver<-x$getInverse()
        if(!is.null(inver)){
                message("getting cached data")
                return(inver)
        }
        matri<-x$get()
        ##calculate inverse of matric
        inver<-solve(matri,...)
        ## set inverse to the object
        x$setInverse(inver)
        ## return inverse
        inver
}
