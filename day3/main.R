l <- c(letters, LETTERS)
n <- 1:length(l)
names(n) <- l



x <- readLines("./day3/data/input.txt")
y <- nchar(x)

#Creation of two vectors from half size
fp <- substr(x, 1, y/2)
sp <- substr(x, y/2 + 1, nchar(x))


# Split as Char vector
fp <- strsplit(fp, "")
sp <-strsplit(sp, "")

# Mar char to position in alphabet
fp <- lapply(fp, \(x) sapply(x, \(y)  n[which(y == names(n))]))
sp <- lapply(sp, \(x) sapply(x, \(y)  n[which(y == names(n))]))

# Function to extract the Char not in the other list
compare_lists_double <- \(x, y) unique(x[which(x %in% y)])

# result for Q1
sum(mapply(compare_lists_double , fp, sp))


# Part 2



# Separe the 3 series
fp <- x[which(1:length(x) %% 3 == 1)] 
sp <- x[which(1:length(x) %% 3 == 2)]
tp <- x[which(1:length(x) %% 3 == 0)]

# Split as Char vector
fp <- strsplit(fp, "")
sp <- strsplit(sp, "")
tp <- strsplit(tp, "")

fp <- lapply(fp, \(x) sapply(x, \(y)  n[which(y == names(n))]))
sp <- lapply(sp, \(x) sapply(x, \(y)  n[which(y == names(n))]))
tp <- lapply(tp, \(x) sapply(x, \(y)  n[which(y == names(n))]))


compare_lists_triple <- \(x,y,z) {
  st <- unique(x[which(x %in% y)])
  unique(st[which(st %in% z)])
}

sum(mapply(compare_lists_triple , fp, sp, tp))

