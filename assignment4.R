makeVector <- function(x = numeric()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmean <- function(mean) m <<- mean
	getmean <- function() m
	list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cachemean <- function(x, ...) {
	m <- x$getmean()
	if(!is.null(m)) {
			message("getting cached data")
			return(m)
	}
	data <- x$get()
	m <- mean(data, ...)
	x$setmean(m)
	m
}

makeCacheMatrix <- function(x = matrix()) {
    inversedData <- NULL
    set <- function(y) {
        x <<- y
        inversedData <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inversedData <<- inverse
    getinverse <- function() inversedData
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
	inversedData = x$getinverse()
	
	if (!is.null(inversedData)){
		message("getting cached data")
		return(inversedData)
	}
	
	mat.data = x$get()
	inversedData = solve(mat.data, ...)

	x$setinverse(inversedData)
	
	return(inversedData)
}

testCache = function(x){
	mcm = makeCacheMatrix(x)
	
	#First time call
	start.time = Sys.time()
	cacheSolve(mcm)
	dur = Sys.time() - start.time
	print(dur)
	
	#Second time call
	start.time = Sys.time()
	cacheSolve(mcm)
	dur = Sys.time() - start.time
	print(dur)
	
	#3rd time call
	start.time = Sys.time()
	cacheSolve(mcm)
	dur = Sys.time() - start.time
	print(dur)

	#4th time call
	start.time = Sys.time()
	cacheSolve(mcm)
	dur = Sys.time() - start.time
	print(dur)

	#5th time call
	start.time = Sys.time()
	cacheSolve(mcm)
	dur = Sys.time() - start.time
	print(dur)		
}