x <- fread("./day4/data/input.txt", sep = ",", sep2 = "-", header = F)

m1 <- tstrsplit(x$V1, "-", type.convert = T)
m2 <- tstrsplit(x$V2, "-", type.convert = T)

rez <- cbind(setDT(m1), setDT(m2))
colnames(rez) <- c("min1", "max1", "min2", "max2")

# Q1
rez[, contained := fifelse((min2 >= min1 & max2 <= max1) | (min1 >= min2 & max1 <= max2), TRUE, FALSE)]
sum(rez[, contained])

# Q2
rez[, overlap := fifelse((min1 <= max2 & max1 >= max2) | (min2 <= max1 & max2 >= max1), TRUE, FALSE)]
sum(rez[, overlap])

