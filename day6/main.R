library(data.table)

a <- strsplit(readLines("./day6/data/input.txt"), "")[[1]]
y <- shift(x, n=-1, fill = NA)
z <- shift(x, n=-2, fill = NA)
w <- shift(x, n=-3, fill = NA)

min(which(!(a == y | a == z | a == w | y == z | y == w | z == w))) + 3

# Part 2

mat <- setDT(shift(a, n=0:-13, fill = NA))
rez <- apply(mat, 1, function(x){
  if(length(unique(x)) == length(x)) return(TRUE)
  FALSE
})

min(which(rez)) + 13
