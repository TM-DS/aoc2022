library(data.table)

x <- fread("./day1/data/input.txt")[, grp := as.integer(NA)]
i <- 1
for(j in 1:nrow(x)){
  if(is.na(x$V1[j])){
    i <- i + 1
    next
  }
  x$grp[j] <- i
}
x <- x[!is.na(V1),]
xg <- x[, .(s = sum(V1)), by=.(grp)][order(s, decreasing = T), ]
# Most calories
xg[1, s]
# Top 3
sum(xg[1:3, s])
