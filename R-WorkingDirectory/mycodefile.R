myoutsideworld <- function () {
	# using file connection
	print("====== File Connection")
	con1 <- file("DataR.txt", "r")
	data <- read.table(con1)
	print(data)
	# using gzfile, bzfile connection
	print("====== gz Connection")
	con2 <- gzfile("DataRZ.gz")
	x <- readLines(con2, 2)
	print(x)
	# using url connection
	print("====== URL Connection")
	con3 <- url("http://www.jhsph.edu", "r")
	x <- readLines(con3)
	print(head(x))
	# Close open connections
	close(con1)
	close(con2)
	close(con3)
}

myreadfile <- function () {
	x <- "DataR.txt"
	y <- 3
	s <- ":"
	c <- c("integer", "character")
	data <- read.table(x, header=TRUE, sep=s, nrows=y, comment.char="#",
					skip=0, colClasses=c, stringsAsFactors=TRUE)
	print(data)
	print("======== show initial attibutes")
	print(attributes(data))
	print("======== sum nrow + ncol ")
	print(nrow(data) + ncol(data))
	print("======== change column and row names")
	names(data) <- c("FirstCol", "SecondCol")  #columns names using just names(dataframe)
	row.names(data) <- c("er", "cn", "ls")
	print(attributes(data))
	print("==== Get column classes")
	classes <- sapply(data, class)
	print(classes)
	print("*** to calculate the number of lines in a file you can use [wc] a Unix tool")
	print("e.g. type in git [wc filename] then it shows")
	print("#of lines, #of words, filesize & filename")
	print( "Note that the last line is not counted ***")
}

writedputdump <- function() {
	# dgut can only be used on a single R object
	zp <- data.frame(empid = 1:10, employed = c(T,T,F,T,T,F,T,T,F,T))
	dput(zp, file = "zp.R")
	# dump can be used on multiple R objects
	x <- "foo"
	y <- data.frame(a = 1:2, b = c("s", "x"))
	dump(c("x", "y") , file = "zd.R") # now two R objects are stored in the file

}

readdgetsource <- function() {
	# dget is used for dput
	print("====== dget the list")
	zg <- dget("zp.R")
	print(zg)
	# source is used for dump
	print("====== source the dump")
	print("****** Note that this is justlike opening R file having functions")
	source("zd.R") # now we have two R objects available x & y
}
