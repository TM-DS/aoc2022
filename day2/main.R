library(data.table)


# Q1
x <- fread("./day2/data/input.txt", header = F)[, N:= 1:.N]
x[, V2:= fifelse(V2=="X", "A", V2)]
x[, V2:= fifelse(V2=="Y", "B", V2)]
x[, V2:= fifelse(V2=="Z", "C", V2)]


match_fun <- function(x){
  if(x[1,1] == x[1,2]) return(3)
  if((x[1,1] == "A" & x[1,2] == "B")| (x[1,1] == "B" & x[1,2] == "C") | (x[1,1] == "C" & x[1,2] == "A")) return(6)
  return(0)
  
}

x[, rez := match_fun(.SD), by=.(N)]
x[, rez := rez + ifelse(V2 == "A", 1, ifelse(V2 == "B", 2, 3))]


sum(x[, rez])

# Q2
p <- c("A", "B", "C")

x <- fread("./day2/data/input.txt", header = F)[, N:= 1:.N][,rez := ifelse(V2 == "X", 0, ifelse(V2 == "Y", 3, 6))]

match_fun <- function(x){

  if(x[1,3] == 0){
    if(x[1,1] == "A") return("C")
    if(x[1,1] == "B") return("A")
    if(x[1,1] == "C") return("B")
    
  }
  if(x[1,3] == 3){
    if(x[1,1] == "A") return("A")
    if(x[1,1] == "B") return("B")
    if(x[1,1] == "C") return("C")
    
  }
  if(x[1,3] == 6){
    if(x[1,1] == "A") return("B")
    if(x[1,1] == "B") return("C")
    if(x[1,1] == "C") return("A")
  }
  
}
x[, play := match_fun(.SD), by=.(N)]
x[, rez2 := rez + ifelse(play == "A", 1, ifelse(play == "B", 2, 3))]


sum(x[, rez2])


