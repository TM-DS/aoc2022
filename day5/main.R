library(data.table)
library(stringr)



a <- readLines("./day5/data/input.txt")

move_first <- which.max(substr(a,1,4)=='move')
letters_id <- which(strsplit(a[move_first-2], '', fixed=T)[[1]] != ' ')
lists <- lapply(letters_id, \(x) substr(a[1:(move_first-3)], x, x))
lists <- lapply(lists, \(x) x[x != " "])


for(i in move_first:length(a)){
   move <- as.integer(str_match_all(a[i], "move (\\d+) from (\\d+) to (\\d+)")[[1]][1, 2:4])
  
   if(length(lists[[move[2]]]) >0) lists[[move[3]]] <- c(lists[[move[2]]][move[1]:1], lists[[move[3]]])
   

   if(move[1] >= length(lists[[move[2]]])){
     lists[[move[2]]] <- character()
   }else{
     lists[[move[2]]] <- lists[[move[2]]][(move[1] +1):length(lists[[move[2]]])]
   }
}
paste0(sapply(lists, \(x) x[1]), collapse = "")

# Part 2
lists <- lapply(letters_id, \(x) substr(a[1:(move_first-3)], x, x))
lists <- lapply(lists, \(x) x[x != " "])


for(i in move_first:length(a)){
  move <- as.integer(str_match_all(a[i], "move (\\d+) from (\\d+) to (\\d+)")[[1]][1, 2:4])
  
  if(length(lists[[move[2]]]) >0) lists[[move[3]]] <- c(lists[[move[2]]][1:move[1]], lists[[move[3]]])
  
  
  if(move[1] >= length(lists[[move[2]]])){
    lists[[move[2]]] <- character()
  }else{
    lists[[move[2]]] <- lists[[move[2]]][(move[1] +1):length(lists[[move[2]]])]
  }
}
paste0(sapply(lists, \(x) x[1]), collapse = "")


