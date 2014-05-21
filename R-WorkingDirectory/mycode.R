mynavalues <- function() {
	print("====== Removing the NA elements from my data set")
	x <- c(1,2,3,NA,6,7,NA,9)
	bad <- is.na(x)     #TRUE are the real na ones
	print(x)
	print(bad)
	print(x[!bad])
	print("===== Find elements where they are not BOTH missing, USING complete.cases")
	x <- c( 1,  2,  3, NA,6,  7,  NA, 9)
	y <- c("a","b",NA,"c",NA,"f","g","t")
	good <- complete.cases(x,y)  #TRUE are the good ones
	print(good)
	print(x[good])
	print(y[good])
}

mysubset <- function() {
	x <- c("a","b","c","d","a")
	print(x)
	print(x[1])
	print(x[4])
	print(x[1:3])
	print("=====building logical index")
	print(x[x > "b"])
	print("=====building logical index in another way")
	u <- x>"b"
	print(u)
	print(x[u])
	print("=====now a matrix; remember with matrix it is always rows then columns")
	x <- matrix(1:6, 2, 3)
	print(x)
	print(x[1,2]) #first row second column
	print("======return a matrix not just a vector with a number")
	print(x[1,2, drop = FALSE])
	print(x[2,2])
	print(x[2,])
	print("======return a matrix, with row, not just a vector with a number")
	print(x[2,, drop = FALSE])
	print(x[,3])
	print("======return a matrix, with column, not just a vector with a number")
	print(x[,3, drop = FALSE])

	print("============================== now a list")
	x <- list(foo = 2:6, bar = 0.6, baz = c("hello","Hi"))
	print(x[1])  # same as x["foo"], returns a list
	print("===")
	print(x[[1]]) #same as x$foo, returns the sequence
	print("===")
	print(x["bar"]) # same as x[2] returns a list
	print("===")
	print(x$bar)    # same as x[[2]] returns the sequence
	print("another way to do it, ONLY using double brackets")
	name <- "bar"
	print(x[[name]])
	print("====== extracting multiple elements from a list has one way")
	print(x[c(1,3)])
	print("====== same command using double brackets return one element")
	print(x[[c(1,3)]])
	print(x[[c(3,2)]])

	print("============================== now a list INSIDE a list")
	x <- list(a = list(10,12,14), b = c(3.14, 2.81))
	print(x[[c(1,3)]]) # same as x[[1]][[3]]
	print(x[[c(2,1)]])
}

mydataframe <- function() {
	x <- data.frame(empid = 1:10, employed = c(T,T,F,T,T,F,T,T,F,T))
	print(x)
	print("=======show number of rows and columns")
	print(nrow(x))
	print(ncol(x))
}

myfactor <- function() {
	# factors used to represent categorical data using factor() function
	# manipulated using modeling functions lm() and glm() functions
	x <- factor(c("yes", "no","yes", "yes", "no","yes","unspecified"), 
						levels=c("yes","no","unspecified"))
	print(x)
	print("============show the table")
	print(table(x))
	print("============show class")
	print(class(x))
	print("============show UNclass")
	print(unclass(x))
}

mylist <- function() {
	# Create a list which consists of differnt vectors
	x <- list(1, "a", TRUE, 1+4i)
	print(x)
	print("======= show list with names")
	x <- list(ali = 1, samy = "a", final = TRUE, complicate = 1+4i)
	print(x)
	print(names(x))
	print("===========show class")
	print(class(x))
	print("============show UNclass")
	print(unclass(x))
}

mymatrix <- function() {
	#creates a matrix with number of rows and columns
	m <- matrix(0:7, nrow=2, ncol=4)
	print(m)
	print("=======assign names to matrix")
	dimnames(m) <- list(c("r1","r2"), c("c1","c2","c3","c4"))
	print(m)
	print("=======show dimention of matrix")
	print(dim(m))
	print("=======show attribute of matrix")
	print(attributes(m))
	# matrix from vector
	print("========Create matrix from integer vector")
	m <- 1:10
	print(m)
	dim(m) <- c(2,5)
	print(m)
	m <- c("a", "b", "c", "d")
	print(m)
	dim(m) <- c(2,2)
	print(m)
	# column binding cbind and row binding cbind
	# common way as well
	print("====== column and row bindings")
	x <- 3:5
	y <- 9:11
	z <- cbind(x,y)
	print("column binding")
	print(z)
	z <- rbind(x,y)
	print("row binding")
	print(z)
}

myvectors <- function() {
	#c() to create vector objects
	print("following using c() function to create vector object")
	x <- c(1.45, 6.89, 102.455) # numeric
	print(class(x)) 
	print(x)
	print("=====================")
	print("show x with names")
	names(x) <- c("simple", "medium", "large")
	print(x)
	print(names(x))
	print("=====================")
	x <- c(TRUE, FALSE) #logical
	print(class(x)) 
	print(x)
	x <- c("type", "the vector") #character
	print(class(x)) 
	print(x)
	x <- 0:9 #Integer
	print(class(x)) 
	print(x)
	#explicit coercion from one class to another
	print("============Start explicit coercion")
	print(as.numeric(x))  
	print(as.logical(x))
	print(as.character(x))
	print(as.complex(x))
	print("============end of explicit coercion")
	x <- c(1+0i, 2+4i) #complex vector
	print(class(x)) 
	print(x)
	print("following using vector() function")
	y <- vector("numeric", length = 10)
	print(class(y)) 
	print(y)
}

myfunction <- function() {
	x <- rnorm(100)
	print(mean(x))
	y <- 5:50
	print(y)
}

noise <- function(x) {
	y <- rnorm(length(x))
	print(y)
	z <- x + y
	print(z)
	a <- 1/0  #Infinity number type
	print(a)
	print(is.na(a))
	print("False means that it is applicable number")
	a <- 0/0  #Not a Number type
	print(a)
	print(is.nan(a))
	print(is.na(a))
	b <- 1L
	print(b - 0.95)
}

randomfunction <- function(y) {
	rnorm(y)
}
