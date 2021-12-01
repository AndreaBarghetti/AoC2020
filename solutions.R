# Advent Of Code ####
library(tidyverse)


#Day 1 ####
#copy numbers to clipboard and read it
input1 <- readLines("Day1/input1.txt") %>% 
  as.numeric()

# * - part 1 - *####
solution1 <- input1 %>% 
  c(.,abs(. -2020)) %>% 
  extract(duplicated(.)) %>% 
  prod()

# or
solution1 <- combn(input1, 2, FUN = prod, simplify = TRUE)[which(combn(input1, 2, FUN = sum, simplify = TRUE)==2020)]

# * - part 2 - *####
solution2 <- combn(input1, 3, FUN = prod, simplify = TRUE)[which(combn(input1, 3, FUN = sum, simplify = TRUE)==2020)]

#
#Day 2 ####
input2 <- readLines("Day2/input2.txt")


# * - part 1 - *####
map_lgl(input2, function(x) {
  rule <- str_split(x,pattern = " ") %>% 
    unlist()
  
  min <- extract(rule, 1) %>%
    str_split("-", simplify = T) %>% min()
  max <- extract(rule, 1) %>%
    str_split("-", simplify = T) %>% max()
  letter <- extract(rule, 2) %>% str_remove(":")
  passw <- extract(rule, 3)
  str_count(passw, letter) %in% min:max
}) %>% unlist() %>% sum()

# * - part 2 - *####
map_lgl(input2, function(x) {
  rule <- str_split(x,pattern = " ") %>% 
    unlist()
  
  min <- extract(rule, 1) %>%
    str_split("-", simplify = T) %>% min()
  max <- extract(rule, 1) %>%
    str_split("-", simplify = T) %>% max()
  letter <- extract(rule, 2) %>% str_remove(":")
  passw <- extract(rule, 3)
  str_locate_all(passw, letter) %>% 
    data.frame() %>% 
    pull(start) %in% c(min, max) %>% 
    sum() == 1
}) %>% unlist() %>% sum()
#
# Day 3 ####
input3 <- readLines("Day3/input3.txt")

# * - part 1 - *####
slope <- str_replace_all(input3, c("\\."="0", "#"="1"))

width <- slope[1] %>% nchar()
length <- length(slope)
position <- list(row=1,col=1)
trees <- 0
direction <- list(right=3, down=1)

skyfun <- function(slope, direction, print=T) {
  
  width <- slope[1] %>% nchar()
  length <- length(slope)
  position <- list(row=1,col=1)
  trees <- 0
  
  while (position$row <= length) {
    hit <- slope[position$row] %>% str_sub(start = position$col, end = position$col) %>% as.integer()
    trees <- sum(trees, hit)
    if(print) {
      print(slope[position$row])
      print(c(rep(".", position$col-1), "^   ", trees) %>% paste(collapse = ""))
      }
    position$col <- position$col+direction$right
    position$row <- position$row+direction$down
    if (position$col >width) {position$col <- position$col - width}
    if (position$row >length) {
      return(trees)
      position <- list(row=1,col=1)
      trees <- 0
      break()
    }
  }
}

skyfun(slope, direction, print = T)

# * - part 2 - *####
directions <- list(list(right=1, down=1),
                   list(right=3, down=1),
                   list(right=5, down=1),
                   list(right=7, down=1),
                   list(right=1, down=2))

lapply(directions, function(direction) {
  skyfun(slope, direction, print=F)
}) %>% unlist() %>% prod()
#
# Day 4 ####
input4 <- readLines("Day4/input4.txt") %>% 
  lapply(function(x) {
  if (x=="") "NEXTPASSPT"
  else x
}) %>% paste(collapse = " ") %>%
  str_split("NEXTPASSPT") %>%
  unlist() %>% 
  str_remove_all("^ | $")


# * - part 1 - *####
fields <- c("byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:", "cid:")

names(fields) <- fields

scan <- lapply(input4, function(x) {
  passport <- x %>%
    str_detect(pattern = fields)
  names(passport) <- fields
  return(passport %>% 
           as.list() %>% 
           as_tibble())
})

scan_df <- scan %>% purrr::reduce(bind_rows)

passidx <- rowSums(scan_df %>% select(-`cid:`))==7
sum(passidx)


# * - part 2 - *####
scan <- lapply(input4, function(x) {
  passport <- x  %>% str_split(" ") %>% unlist()
  names(passport) <- passport %>% 
    str_remove(":.*")
  return(passport %>% 
           as.list() %>% 
           as_tibble())
})

scan_df <- scan %>% purrr::reduce(bind_rows)

checkpass <- scan_df %>%
  mutate_all(.funs = function(.) str_remove(., pattern = ".*:")) %>%
  mutate(byr = byr %>% as.numeric() %>% between(1920,2002),
         iyr = iyr %>% as.numeric() %>% between(2010,2020),
         eyr = eyr %>% as.numeric() %>% between(2020,2030),
         hgt = case_when(str_detect(hgt, "cm") ~ hgt %>% str_remove("cm") %>% as.numeric() %>% between(150,193),
                         str_detect(hgt, "in") ~ hgt %>% str_remove("in") %>% as.numeric() %>% between(59,76),
                         is.na(hgt) ~ F,
                         T~F),
         hcl = str_detect(hcl, pattern = "#[a-f0-9]{6}"),
         ecl = ecl %in% c("amb","blu","brn","gry", "grn","hzl","oth"),
         pid = pid %>% str_detect("^[0-9]{9}$"))


passidx2 <- checkpass %>%
  select(-cid) %>%
  rowSums(na.rm=T) == 7

sum(passidx2)
#
# Day 5 ####
input5 <- readLines("Day5/input5.txt")

# * - part 1 - *####
seats <- list(a=str_sub(input5, 1,7),
                b=str_sub(input5, 8,10))
  
seats$row <- seats$a %>% str_replace_all(c("F"="0", "B"="1")) %>%
                              strtoi(base=2)
seats$col <- seats$b %>% str_replace_all(c("L"="0", "R"="1")) %>%
  strtoi(base=2)

seats$id <- seats$row*8+seats$col

max(seats$id)

# * - part 2 - *####
all_seats <- min(seats$id):max(seats$id)

all_seats %>% extract(!all_seats %in% seats$id)

#
# Day 6 ####
input6 <- readLines("Day6/input6.txt") 

# * - part 1 - *####

answers <- input6 %>% 
  lapply(function(x) {
    if (x=="") "NEXT-GRP"
    else x
  }) %>% paste(collapse = " ") %>%
  str_split("NEXT-GRP") %>%
  unlist() %>% 
  str_remove_all("^ | $")

count_uniq <- sapply(answers, function(x) {
  x %>%
    str_remove_all(" ") %>% 
    str_split("") %>% 
    unlist() %>% 
    unique() %>%
    length()
})

sum(count_uniq)

# * - part 2 - *####
count_common <- sapply(answers, function(x) {
  x %>%
    str_split(" ") %>%
    unlist() %>%
    lapply(function(x) {
      str_split(x, "") %>% 
        unlist()
    }) %>%
    reduce(intersect) %>%
    unique() %>%
    length()
})

sum(count_common)
#
# proiblem 7 ####
input7 <- readLines("Day7/input7.txt")

# * - part 1 - *####
rules <- lapply(input7, function(line) {
  rule <- line %>%
    str_remove("\\.$") %>%
    str_split(" bags contain ") %>%
    unlist()
  names(rule) <- c("bag","inside")
  return(rule %>% as.list())
})

bags <- sapply(rules, function(x) {
  x$bag
})

insides <- sapply(rules, function(x) {
  x$inside
})

insides2 <- lapply(insides, function(x) {
  what <- x %>% str_split(", ") %>% unlist()
  howmany <- what %>% sapply(parse_number)
  if (is.na(howmany)) { howmany <- 0}
  names(howmany) <- what %>% str_remove_all(c("[0-9] | bags| bag"))
  return(howmany)
})

names(insides2) <- bags

bags_df <- lapply(bags, function(bag) {
  res <- insides2[[bag]] %>%
    as.list() %>% 
    as_tibble() %>%
    mutate(bag=bag)
  return(res)
}) %>% purrr::reduce(bind_rows) %>%
  select(bag, everything())

tidy_bags <- bags_df %>% 
  gather(contain, number, 2:ncol(bags_df)) %>%
  arrange(bag) %>%
  filter(!is.na(number))

lev1 <- tidy_bags %>%
  filter(contain=="shiny gold") %>%
  pull(bag) %>%
  unique()

possible_bags <- lev1
lev <- lev1

while(length(lev)>0) {
  lev <- tidy_bags %>%
    filter(contain %in% lev) %>%
    pull(bag) %>%
    unique()
  possible_bags <- c(possible_bags, lev)
  if(length(lev)==0) {break()}
}

length(possible_bags %>% unique())

# * - part 2 - *####
n <- 0

test <- tidy_bags %>%
  filter(bag=="shiny gold") %>%
  select(contain, number)

while(nrow(test)>0) {
  n <- n + sum(test$number)
  print(n)
  test <- test %>%
  rename(bag=contain) %>%
  left_join(tidy_bags, by="bag") %>%
  mutate(number=number.x*number.y) %>%
  select(contain, number) %>%
  filter(contain!="no other")
  print(test)}
n
#
# Day 8 ####
input8 <- readLines("Day8/input8.txt")

# * - part 1 - *####
code <- lapply(input8, function(x) {
  out <- x %>%
    str_split(" ")
})

instructions <- sapply(code, function(x) {x[[1]][1]})
arguments <- sapply(code, function(x) {x[[1]][2] %>% as.numeric()})

i <- 1
accumulator <- 0
ilist <- vector(mode = "numeric")

run_program <- function(instructions, arguments) {
  while(!i %in% ilist) {
    
    ilist <- append(ilist, i)
    instruction <- instructions[i]
    
    if(i > length(instructions)) {return(paste("terminated with accumulator = ", accumulator))}
    
    if(instruction == "acc") {
      accumulator <- accumulator + arguments[i]
      i <- i + 1
      next()
    }
    
    else if(instruction == "jmp") {
      i <- i + arguments[i]
      next()
    }
    
    else if(instruction == "nop") {
      i <- i+1
      next()
    }
  }
  return(paste("endless loop started with accumulator=", accumulator))
}

run_program(instructions, arguments)

# * - part 2 - *####
try_fix_at <- grep(instructions, pattern = "nop|jmp")

for(pos in try_fix_at) {
  new_instructions <- instructions
  new_instructions[pos] <- new_instructions[pos] %>%
    recode("nop"="jmp", "jmp"="nop")
  
  output <- run_program(new_instructions, arguments)

  if (str_detect(output, "terminated")) {
    print(paste(output))
    break()
    }
}

# Day 9 ####
input9 <- readLines("Day9/input9.txt")

# * - part 1 - *####
input9

numbers <- input9 %>% as.numeric()

for (i in seq_along(numbers)) {
  
  if (i <= 25) {next()}
  
  if (numbers[i] %in% (numbers[(i-25):(i-1)] %>% 
      combn(2) %>% 
      colSums())) {next()}
  
  else {
    target <- numbers[i]
    print(target)}
    }

# * - part 2 - *####
for (i in seq_along(numbers)) {
  for (j in 1:(length(numbers)-i)) {
    sum = sum(numbers[i:(j+i)])
    if (sum == target) {
      result <- numbers[i:(j+i)]
      print(sum(range(result)))
      stop("done!")}
  }
}

# Day 10 ####
input10 <- readLines("Day10/input10.txt")

# * - part 1 - *####
nums <- input10 %>% as.numeric()

diff <- tibble(a=c(0,nums) %>% sort()) %>%
  mutate(b=lead(a,1, default = max(a)+3),
         dif=b-a) %>%
  pull(dif) %>%
  table() %>% prod()

diff

# * - part 2 - *####
ns <- tibble(a=c(0,nums) %>% sort()) %>% 
  mutate(b=lead(a,1, default = max(a)+3),
         dif=b-a) %>%
  pull(dif) %>%
  paste0(collapse = "") %>%
  str_remove("[3]+$") %>%
  str_remove("^3") %>%
  str_split("[3]+") %>%
  sapply(function(x) (nchar(x)-1))

combos_fun <- function(vector) {
  sapply(vector, function(n) {
      if (n < 3) return( 2^n)
      else (y <- 2^n - sum(2^((n-3):0)))
  })
}

#options(digits=16)
combos_fun(ns) %>% purrr::reduce(prod)



# Day 11 ####
input11 <- readLines("Day11/input11.txt")

# * - part 1 - *####
floors <- sapply(input11, USE.NAMES = F, function(string) {
  str_split(string, "") %>%
    unlist() != "L"
}) %>% t()


occupied_chairs <- matrix(F, nrow = nrow(floors),ncol=ncol(floors))

positions <-  matrix(seq_along(floors), nrow=nrow(floors))

get_neighbour_matrix <- function(matrix, position) {
  nrow <- nrow(matrix)
  ncol <-  ncol(matrix)
  pos <- which(position==positions, 
               arr.ind=TRUE)
  row_start <- max(1, pos[1]-1)
  row_end <- min(pos[1]+1, nrow)
  col_start <- max(1, pos[2]-1)
  col_end <- min(pos[2]+1, ncol)
  
  return(matrix[row_start:row_end,col_start:col_end])
}

take_seat <- function(matrix, position) {
  neighbours <- sum(get_neighbour_matrix(matrix, position))
  if (neighbours == 0) return(TRUE)
  else if(neighbours > 4) return(FALSE)
  else return(matrix[position])
}

stable <- FALSE
i<-0
while(!stable) {
  #seating
  seating <- sapply(positions, function(position) {
    take_seat(occupied_chairs, position)
  }) %>% matrix(nrow=nrow(occupied_chairs))
  #empty_floor
  seating[floors] <- FALSE
  stable <- all(seating == occupied_chairs)
  occupied_chairs <- seating
  i <- i+1
  print(i)
}

sum(occupied_chairs)

to_print <- occupied_chairs %>%
  as.numeric() %>%
  recode("1"="#","0"="L") %>%
  matrix(nrow=nrow(occupied_chairs))
to_print[floors] <- "."
apply(to_print, 1, paste, collapse="") %>%
    paste(collapse="\n") %>% 
  cat()

# * - part 2 - *####

#add extra floor
input11 <- readLines("Day11/input11.txt")

floors <- sapply(input11, USE.NAMES = F, function(string) {
  str_split(string, "") %>%
    unlist() == "."
}) %>% t()

positions <-  matrix(seq_along(floors), nrow=nrow(floors))

chairs <- matrix("L", nrow = nrow(floors),ncol=ncol(floors))
chairs[floors] <- "_"

chair_positions <- positions[chairs=="L"]

stable <- FALSE
i<-0

#
take_seat2 <- function(matrix, position) {
  nrow <- nrow(matrix)
  ncol <-  ncol(matrix)
  pos <- which(position==positions, 
               arr.ind=TRUE)
  
  
  #N
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look[1] <- look[1]-1
    if (look[1]<1) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  N <- see
  #S
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look[1] <- look[1]+1
    if (look[1]>nrow) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  S <- see
  #W
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look[2] <- look[2]-1
    if (look[2]< 1) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  W <- see
  # E
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look[2] <- look[2]+1
    if (look[2]> ncol) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  E <- see
  #NW
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look <- look-1
    if (look[1]<1 | look[2]<1) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  NW <- see
  #NE
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look[1] <- look[1]-1
    look[2] <- look[2]+1
    if (look[1]<1 | look[2]>ncol) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  NE <- see
  
  #SE
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look <- look+1
    if (look[1]>nrow | look[2]>ncol) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  SE <- see
  
  #SW
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look[1] <- look[1] +1
    look[2] <- look[2] -1
    if (look[1]>nrow | look[2]<1) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  SW <- see
  
  visible <- c(N,NE,E,SE,S,SW,W,NW) == "#"
  
  if (sum(visible)==0) return("#")
  else if (sum(visible)>=5) return("L")
  else return(matrix[position])
}

while(!stable) {
  #seating
  seating <- sapply(chair_positions, function(position) {
    take_seat2(chairs, position)
  }) 
  
  chairs2 <- chairs
  chairs2[chair_positions] <- seating
  
  stable <- all(chairs2 == chairs)
  chairs <- chairs2
  i <- i+1
  if(F) {cat(i)
    
    cat("\n\n")
    
    chairs %>%
      apply(1, paste, collapse="") %>%
      paste(collapse="\n") %>% 
      cat()
    
    cat("\n\n")}
  
  if(F) {cat(paste(sum(chairs=="#"),"\n"))}
}

sum(chairs=="#")


# Day 12 ####
input12 <- readLines("Day12/input12.txt")

# * - part 1 - *####
actions <- sapply(input12, function(x) {
  dir <- x %>% str_split("", n = 2)
  dir[[1]][1]
})

values <- sapply(input12, function(x) {
  dir <- x %>% str_split("", n = 2)
  dir[[1]][2]
}) %>% as.numeric()

move <- function(direction, value) {
  if (direction=="N") {y <<- y + value}
  else if (direction=="S") {y <<- y - value}
  else if (direction=="W") {x <<- x - value}
  else if (direction=="E") {x <<- x + value}
}

turn <- function(direction, value) {
  if (direction=="R") {dir <<- dir + (value/90)}
  else if (direction=="L") {dir <<- dir - (value/90)}
}

x <- 0
y <- 0
dirs <- rep(c("N","E","S","W"), 100)
dir <- 50

for (i in seq_along(actions)) {
  if (actions[i] %in% c("N","E","S","W")) {
    move(actions[i], values[i])
    
    print(paste("move", actions[i], dirs[dir],  values[i]))
  }
  
  else if (actions[i] %in% c("L","R")) {
    turn(actions[i], values[i])
    
    print(paste("turn",  dirs[dir]))
  }
  
  else if (actions[i] == "F") {
    move(dirs[dir], values[i])
    
    print(paste("move", actions[i], dirs[dir], values[i]))
  }
  
  print(paste(x,y))
}
sum(abs(x), abs(y))


# * - part 2 - *####
waypoint <- list(x=10,y=1)
x <- 0
y <- 0

move2 <- function(waypoint, value) {
    x <<- x + waypoint$x*value
    y <<- y + waypoint$y*value
  }

move_waypoint <- function(direction, value) {
  x <- waypoint$x
  y <- waypoint$y
  
  if (direction=="N") {y <- y + value}
  else if (direction=="S") {y <- y - value}
  else if (direction=="W") {x <- x - value}
  else if (direction=="E") {x <- x + value}
  
  waypoint$x <<- x
  waypoint$y <<- y
}

turn2 <- function(direction, value) {
  
  x <- waypoint$x
  y <- waypoint$y
  
  turn <- value/90
  
  if (direction=="L") {turn <- 4 - turn}
   
  if (turn==1) {
    waypoint$x <<- y
    waypoint$y <<- -1*x
  }
  if (turn==2) {
    waypoint$x <<- -1*x
    waypoint$y <<- -1*y
  }
  if (turn==3) {
    waypoint$x <<- -1*y
    waypoint$y <<- x
  }
}

for (i in seq_along(values)) {
  print(input12[i])
  if (actions[i] %in% c("N","E","S","W")) {
    move_waypoint(actions[i], values[i])
    
    print(paste("move waypoint to ", paste(waypoint %>% unlist(), collapse = " ")))
  }
  
  else if (actions[i] %in% c("L","R")) {
    turn2(actions[i], values[i])
    
    print(paste("turn",actions[i],values[i], "waypoint:", paste(waypoint %>% unlist(), collapse = " ")))
  }
  
  else if (actions[i] == "F") {
    move2(waypoint, value = values[i])
    
    print(paste("move toward", paste(waypoint %>% unlist(), collapse = " "), values[i],"times" ))
  }
  
  print(paste(x,y))
}

sum(abs(x), abs(y))


# Day 13 ####
input13 <- read_lines("Day13/input13.txt")

# * - part 1 - *####
bus <- input13 %>% str_split(",") %>% unlist() %>% as.numeric()

wait <- (bus[-1] - (bus[1] %% bus[-1])) %>% min(na.rm = T)
getbus <- bus[-1][which(bus[-1] - (bus[1] %% bus[-1]) == wait)]

getbus*wait

# * - part 2 - *####
bus2 <- bus[-1]
delays <- 0:(length(bus2)-1)

bus3 <- bus2[!is.na(bus2)] %>% as.numeric()
delays2 <- delays[!is.na(bus2)]

lcm <- function(x, y) {
  # choose the greater number
  greater = max(x,y)
  i <- 1
  while(TRUE) {
    if((greater %% x == 0) && (greater %% y == 0)) {
      lcm = greater
      break
    }
    i <- i + 1
    greater = max(x,y) * i
  }
  return(lcm)
}

align <- function(x,y,delayx,delayy) {
  i <- x-delayx
  
  while(T) {
    if ((i+delayx)%%x == 0 & (i+delayy)%%y==0) {
      return(i)
      break
      }
    else {i <- i+x}
  }
}

# must covert to loop
i<-1
i<-i+1
delay <- align(bus3[1],bus3[i],delays2[1], delays2[i])
new <- lcm(bus3[1],bus3[i])
i<-i+1
delay <- align(new,bus3[i],new-delay,delays2[i])
new <- lcm(new,bus3[i])

# Day 14 ####
input14 <- read_lines("Day14/input14.txt")
library(magrittr)
library(binaryLogic)

# * - part 1 - *####

code <- list()
iter <- 0

for (i in input14) {
  if (str_detect(i, "mask")) {
    iter <<- iter+1
    mask <- i %>% str_remove("mask = ")
    code[[iter]]<- list(mask=mask, mem=c(), val=c())
  }
  else if (str_detect(i, "mem")) {
    code[[iter]]$mem <- append(code[[iter]]$mem, str_replace(i, ".*\\[(.*)\\].*", "\\1"))
    code[[iter]]$val <- append(code[[iter]]$val, str_remove(i, ".*= "))
  }
}

memories <- lapply(code, function(x) x$mem) %>% unlist()

bin2dec <- function(x)
{
  b <- as.numeric(unlist(strsplit(x, "")))
  pow <- 2 ^ ((length(b) - 1):0)
  sum(pow[b == 1])
}

dec2bin <- function(x) {
  x %>% 
    intToBits() %>% 
    as.numeric() %>% 
    paste(collapse = "")}

get_masked_val <- function(mask, vals) {
  mask <-  mask %>% str_split("") %>% unlist()
  sapply(vals,USE.NAMES = F, function(val) {
    cval <- val %>% 
      as.numeric() %>%
      as.binary(n = 36, size = 8) %>%
      as.character()
    out <- map2_chr(cval,mask, function(val, mask) {
      if (mask=="X") {return(val %>% as.character())}
      else {return(mask)}
    })
    out %>% 
      paste(collapse = "") %>%
      bin2dec()
  })
}

coded <- lapply(code, function(x) {
  x$assigned <- get_masked_val(mask = x$mask,
                 vals = x$val)
  x
})

assigned <- sapply(coded, function(x) {x$assigned}) %>% unlist()
names(assigned) <- memories

unimem <- list()

#for (m in unique(memories)) {unimem[[m]] <- 0}

for (i in seq_along(assigned)) {unimem[[memories[[i]]]]<-assigned[[i]]}
# options(digits = 16)
unimem %>% unlist() %>% sum()

# * - part 2 - *####
mask_mem <- function(mask, mems) {
  mask <-  mask %>% str_split("") %>% unlist()

  sapply(mems,USE.NAMES = F, function(mem) {
    cmem <- mem %>% 
      as.numeric() %>%
      as.binary(n = 36, size = 8) %>%
      as.character()
    out <- map2_chr(cmem,mask, function(mem, mask) {
      if (mask=="X") {return(".")}
      else if (mask=="0") {return(mem %>% as.character())}
      else if (mask=="1") {return("1")}
    })
    out %>% 
      paste(collapse = "")
  }) 
}

masked <- lapply(code, function(x) {
  x$to_mem <- mask_mem(mask = x$mask,
                               mems = x$mem)
  x
})

expand_mem <- function(my_mem) {
  while(any(str_detect(my_mem, "\\."))) {
    zero <- str_replace(my_mem, "\\.","0")
    one <- str_replace(my_mem, "\\.","1")
    my_mem <- c(zero, one)
  }
  return(my_mem)
}

addressed <- lapply(masked, function(x) {
  x$addresses <- x$to_mem %>% lapply(expand_mem)
  x
})

unimem <- list()

for (i in seq_along(addressed)) {
  for (j in seq_along(addressed[[i]]$val)) {
    unimem[addressed[[i]]$addresses[[j]]] <- addressed[[i]]$val[j]
  }
}

unimem %>% as.numeric() %>% sum()

# Day 15 ####
input15 <- c(20,0,1,11,6,3)

# * - part 1 - *####
init <- input15

i <- length(init)
while(i < 2020) {
  if (!init[i]  %in% init[-i]) {init[i+1] <- 0}
  else {init[i+1] <- match(init[i],rev(init[-i]))}
  i <- i+1
}
init[2020]

# * - part 2 - *####
init <- input15

memory_game <- function(init, rounds) {
  taken_number <- init[-length(init)]
  next_n <- init[length(init)]
  
  index_numbers <- rep(0, rounds)
  index_numbers[taken_number+1] <- seq_along(taken_number)
  
  i <- length(init)
  
  while (i < rounds) {
    
    is_new <- index_numbers[next_n+1]==0
    
    if(is_new) {
      index_numbers[next_n+1] <- i
      next_n <- 0
    }
    else {
      x <- i - index_numbers[next_n+1]
      index_numbers[next_n+1] <- i
      next_n <- x
    }
    i<-i+1
  }
  return(next_n)
}

memory_game(init = init,
            rounds=3e7)



# Day 16: Ticket Translation ####
input16 <- read_lines("Day16/input16.txt")

# * - part 1 - *####
rules <- input16[1:(grep(input16, pattern = "your ticket:")-2)]
my_ticket <- input16[grep(input16, pattern = "your ticket:")+1] %>%
  str_split(",") %>% unlist() %>% as.integer() 
all_tickets <- input16[-c(1:(grep(input16, pattern = "nearby tickets:")+1))] %>%
  str_split(",") %>% lapply(as.integer)

all_ranges <- str_match_all(rules, "[0-9]+-[0-9]+") %>% 
  unlist() %>% lapply(function(x) {c(str_match(x,"^[0-9]+") %>% as.integer(), 
                                     str_match(x,"[0-9]+$") %>% as.integer())})

all_values <- lapply(all_ranges, function(range) {c(range[1]:range[2])}) %>% unlist() %>% unique() %>% sort()

error_rate <- sum(sapply(all_tickets, function(ticket) {sum(ticket[!ticket %in% all_values])}))

# * - part 2 - *####
all_ranges2 <- str_match_all(rules, "[0-9]+-[0-9]+") %>% 
  lapply(function(x) { unlist(x) %>% lapply(function(x) {c(str_match(x,"^[0-9]+") %>% as.integer(), 
                                                           str_match(x,"[0-9]+$") %>% as.integer())})})

valid_tickets <- all_tickets[sapply(all_tickets, function(ticket) {all(ticket %in% all_values)})]

rules_names <- str_match(rules, "^(.*):")[,2]
names(all_ranges2) <- rules_names

possible <- 1:20

matrix <- sapply(seq_along(all_ranges2), function(f) {
  sapply(possible, function(i) {
    all(sapply(valid_tickets, function(x) x[i]) %>% between(all_ranges2[[f]][[1]][1],all_ranges2[[f]][[1]][2]) |
          sapply(valid_tickets, function(x) x[i]) %>% between(all_ranges2[[f]][[2]][1],all_ranges2[[f]][[2]][2]))
  })
}) 

df <- matrix %>% as_tibble()
colnames(df) <- c(rules_names)

#I could not code the sulution, I had to solve it like a sudoku
df %>%
  rownames_to_column(var = "f") %>% 
  mutate_all(.,as.integer) %>%
  gather(field, value, 2:21) %>%
  ggplot(aes(x=field, y=f))+
  geom_text(aes(label=value))+
  theme_bw() +
  scale_x_discrete(position = "top") + 
  scale_y_reverse(breaks = 1:20) +
  theme(axis.text.x = element_text(angle=90, hjust=0, vjust=.5))
  
results <- list("departure date" = 2,
     "departure time"=3,
     "departure location" =7,
     "departure track" = 13,
     "departure platform" = 15,
     "departure station"=17)

my_ticket[results %>% unlist()] %>% prod()

# Day 17: Conway Cubes ####
input17 <- read_lines("Day17/input17.txt")

cubes <- input17 %>%
  lapply(function(x) {
    x %>% 
      str_replace_all(c("\\."="0", "#"="1")) %>% 
      str_split("") %>%
      unlist() %>%
      as.integer()
  })

# * - part 1 - *####

tidy_space <- tibble(x=-12:12, y=-12:12, z=-12:12) %>%
  expand(x,y,z) %>%
  mutate(state=0)

# inizialize space:
for (y in seq_along(cubes)) {
  for (x in seq_along(cubes[[y]])) {
    tidy_space$state[tidy_space$x==x & 
               tidy_space$y==y &
               tidy_space$z==0] <- cubes[[y]][x]
  }
}


simulate_expansion <- function(tidy_space, cycles) {
  
  for (i in 1:cycles) {
    tidy_charge <- pmap(tidy_space %>%
                          filter(state==1), function(x,y,z,...){
                            near <- tibble(
                              x=c((x-1):(x+1)),
                              y=c((y-1):(y+1)),
                              z=c((z-1):(z+1))
                            ) %>% 
                              expand(x,y,z) %>%
                              mutate(charge=1)
                            return(near)
                          }) %>%
      reduce(bind_rows) %>%
      group_by(x,y,z) %>%
      summarise(charge=sum(charge))
    
    tidy_space <- left_join(tidy_space, tidy_charge) %>%
      mutate(charge= case_when(is.na(charge)~0, 
                               T~charge),
             state=case_when(state==1 & charge%in% c(3,4) ~ 1,
                             state==1 & !charge%in% c(3,4) ~ 0,
                             state==0 & charge==3 ~ 1,
                             state==0 & charge!=3 ~ 0)) %>%
      select(-charge)
  }
  return(sum(tidy_space$state))
}

simulate_expansion(tidy_space, 6)


# * - part 2 - *####
tidy_space <- tibble(x=-13:13, y=-13:13, z=-13:13, w=-13:13) %>%
  expand(x,y,z,w) %>%
  mutate(state=0)

# inizialize space:
for (y in seq_along(cubes)) {
  for (x in seq_along(cubes[[y]])) {
    tidy_space$state[tidy_space$x==x & 
                       tidy_space$y==y &
                       tidy_space$z==0 &
                       tidy_space$w==0] <- cubes[[y]][x]
  }
}

simulate_expansion <- function(tidy_space, cycles) {
  
  for (i in 1:cycles) {
    tidy_charge <- pmap(tidy_space %>%
                          filter(state==1), function(x,y,z,w,...){
                            near <- tibble(
                              x=c((x-1):(x+1)),
                              y=c((y-1):(y+1)),
                              z=c((z-1):(z+1)),
                              w=c((w-1):(w+1))
                            ) %>% 
                              expand(x,y,z,w) %>%
                              mutate(charge=1)
                            return(near)
                          }) %>%
      reduce(bind_rows) %>%
      group_by(x,y,z,w) %>%
      summarise(charge=sum(charge), `.groups`="drop")
    
    tidy_space <- left_join(tidy_space, 
                            tidy_charge, 
                            by=c("x","y","z","w")) %>%
      mutate(charge= case_when(is.na(charge)~0, 
                               T~charge),
             state=case_when(state==1 & charge%in% c(3,4) ~ 1,
                             state==1 & !charge%in% c(3,4) ~ 0,
                             state==0 & charge==3 ~ 1,
                             state==0 & charge!=3 ~ 0)) %>%
      select(-charge)
  }
  return(sum(tidy_space$state))
}

simulate_expansion(tidy_space, 6)

# Day 18: Operation Order ####
input18 <- read_lines("Day18/input18.txt")

math <- sapply(USE.NAMES = F, input18, str_remove_all, " ")


# * - part 1 - *####

# if there is a bracket extract it, solve it and replace it
# if there are no bracket, solve first 2 and replace results
# if there is only one number left, return result
string = "1+1*4"
solve_first2 <- function(string) {
  paste0(string %>% str_match("[0-9]+[\\+\\*][0-9]+") %>% pluck(1) %>% parse(text = .) %>% eval(),
         string %>% str_remove("[0-9]+[\\+\\*][0-9]+"))
}
solve_first2(string)

solve_all <- function(string) {
  while( str_detect(string, "[\\+\\*]")) {
    string <- solve_first2(string)
  }
  return(string)
}

solve_all(string)

string = "(2*3)+(1*4+(2*3))"
extract_bracket <- function(string) {
  (string %>% str_match(.,"\\(([0-9\\*\\+]+)\\)"))[[2]]
}
extract_bracket(string)

extract_inner_bracket <- function(string) {
  while( str_detect(string, "[\\(\\)]")) {
    string <- extract_bracket(string)
  }
  return(string)
}
extract_inner_bracket(string)

solve_inner_bracket <- function(string) {
  string %>% str_replace_all(
    paste0("\\(",
           extract_inner_bracket(string) %>%
             str_replace_all(c("\\*" = "\\\\*", 
                               "\\+" = "\\\\+")),
           "\\)"),
    solve_all(extract_inner_bracket(string))
  )
}

solve_inner_bracket(string)

solve_math <- function(string) {
  while(str_detect(string, "[\\(\\)]")) {
    string <- solve_inner_bracket(string)
  }
  return(string %>% solve_all)
}

solve_math(string)

# options(digits=16)
sapply(math, solve_math) %>% as.numeric() %>% sum()


# * - part 2 - *####
string = "1+1*4+1"
string = "2*4+1"

solve_first_add <- function(string) {
  string %>% str_replace(
    str_match(string, "[0-9]+[\\+][0-9]+") %>% pluck(1)%>%
      str_replace("\\+","\\\\+"),
    str_match(string, "[0-9]+[\\+][0-9]+") %>% pluck(1) %>% 
      parse(text = .) %>% eval() %>% as.character())
}
solve_first_add(string)

solve_all <- function(string) {
  while( str_detect(string, "[\\+]")) {
    string <- solve_first_add(string)
  }
  while(str_detect(string, "[\\*]")) {
    string <- solve_first2(string)
  }
  return(string)
}

sapply(math, solve_math) %>% as.numeric() %>% sum()


# Day 19: Monster Messages ####
input19 <- read_lines("Day19/input19.txt")

# * - part 1 - *####
rules <- input19[str_detect(input19, ":")] %>%
  lapply(function(x) str_split(x, ":") %>% unlist() %>%
           str_remove_all("\""))
messages <- input19[!str_detect(input19, ":")][-1]

all_rules <- lapply(rules, function(x) x[[2]])
names(all_rules) <- lapply(rules, function(x) x[[1]])


collected <- c("0")

found <-"0"

while(length(found!=0)) {
  found <- all_rules[found] %>% 
    sapply(function(x) {
      str_extract_all(x, "[0-9]+") 
    }) %>%
    purrr::reduce(c) %>%
    unique()
  collected <<- c(collected, found) %>% unique()
  
}

needed_rules <- all_rules[collected] %>% 
  lapply(str_replace_all, "([0-9ab]+)","(\\1)") %>% 
  lapply(str_replace_all, "\\|",")|(") %>% 
  lapply(function(x) {str_c("((",x,"))")}) %>% 
  lapply(str_remove_all, " ")

main_rules2 <- needed_rules[sapply(needed_rules, str_detect, "[ab]")]
subrules2 <- needed_rules[!sapply(needed_rules, str_detect, "[ab]")]

decoded_rules <- list()
main_rules2
subrules2



decnames<-c()
while(length(main_rules2)!=0){
  
  iwalk(main_rules2, function(rulevalue, ruleN) {
    decnames <<- c(decnames, ruleN)
    decoded_rules[ruleN] <<- rulevalue
    main_rules2[ruleN] <<- NULL
    subrules2<<-lapply(subrules2, function(value) {
      str_replace_all(value, str_c("\\(",ruleN,"\\)"),  rulevalue)
    })
    main_rules2 <<- c(main_rules2, subrules2[!subrules2 %>%
                              sapply(function(x) {
                                str_detect(x, "[0-9]")
                              })])
    subrules2[!(subrules2 %>% sapply(str_detect, "[0-9]"))] <<- NULL
    
  })
}

decnames
main_rules2
subrules2
decoded_rules

decoded_rules <- decoded_rules %>% 
  lapply(str_remove_all," ") %>% 
  lapply(str_replace_all,"\\(([ab]+)\\)","\\1") %>% 
  lapply(str_replace_all,"\\(([ab]+)\\)","\\1")

all_rules[["0"]]
decoded_rules[["0"]]
decoded_rules[["0"]] == str_c("((",decoded_rules[["8"]],decoded_rules[["11"]],"))")

rule0 <- str_c("^",decoded_rules[["0"]],"$")

sum(sapply(messages, str_detect, rule0))

# * - part 2 - *####
all_rules[["8"]] <- " 42 | 42 8"
all_rules[["8"]] <- " 42+"

all_rules[["11"]] <- " 42 31 | 42 11 31"
all_rules[["11"]] <- " 42 31 | 42 42 31 31 | 42 42 42 31 31 31 | 42 42 42 42 31 31 31 31"

# and then the same... 

# Day 20: Jurassic Jigsaw ####
input20 <- read_lines("Day20/input20.txt")
# * - part 1 - *####
library(R6)

tile_names <- input20 %>% str_extract("[0-9]+") %>% na.omit() %>% as.character()
tiles <- list()
for (name in tile_names) {
  tiles[[name]] <- matrix(nrow = 10,ncol = 10, data = rep("",100))
}

input20clean <- input20[input20!=""]

for (row in input20clean) {
  if(str_detect(row, "Tile")) {
    irow <- 1
    tile_name <- str_extract(row, "[0-9]+")
  } else {
    tiles[[tile_name]][irow,] <- row %>% 
      # str_replace_all("\\.","0") %>% 
      # str_replace_all("#","1") %>%  
      str_split("") %>% sapply(as.character)
    irow <- irow + 1
  }
}

get_edges <- function(tile){
  up <- tile[1,] %>% 
    #as.character() %>% 
    str_c(collapse = "")
  right <- tile[,10] %>% 
    #as.character() %>% 
    str_c(collapse = "") 
  down <- tile[10,] %>% 
    #as.character() %>% 
    str_c(collapse = "") %>% 
    stringi::stri_reverse()
  left <- tile[,1] %>% 
    #as.character() %>% 
    str_c(collapse = "") %>% 
    stringi::stri_reverse()
  return(list(up=up, right=right, down=down, left=left))
}


# define frame object
# where to put the tiles
# used R6 just for fun

Frame <- R6Class(
  classname = "Frame",
  public =  list(
    
    matrix=NA,
    
    rotate = function() {
      self$matrix <- t(apply(self$matrix, 2, rev))
      invisible(self)
    },
    
    flip = function() {
      self$matrix <- self$matrix [c(dim(self$matrix)[1]:1),]
      invisible(self) 
    },
  
    initialize = function() {
      self$matrix <- matrix(rep(c("empty"),144), nrow=12)
    },
    
    print = function(...) {
      cat(paste0(self$matrix[1,1],"-",
                 self$matrix[1,dim(self$matrix)[2]],"\n",
                 self$matrix[dim(self$matrix)[1],1],"-",
                 self$matrix[dim(self$matrix)[1],dim(self$matrix)[2]]),"\n")
      invisible(self)
    }
    
  ))

the_frame <- Frame$new()

the_frame$matrix
the_frame

# Define the Tile object
# used R6 just for fun

Tile <- R6Class("Tile", 
                list(edges = list(),
                     
                     ID = NA_character_,
                     
                     matrix = NA_integer_,
                     
                     available = T,
                     position = NA_integer_,
                     
                     side = 1, # change when flip
                     
                     orientation=1, # change when rotate or flip
                     
                     allow_flip = T,
                     allow_rotation = T,
                     
                     possible_edges = NA_character_,
                     
                     rotate = function() {
                       
                       if(self$allow_rotation) {
                         self$orientation <- self$orientation + 1
                         if(self$orientation ==5) {self$orientation<-1}
                         
                         self$matrix <- t(apply(self$matrix, 2, rev))
                         
                         self$edges <- get_edges(self$matrix)
                       }
                       
                       invisible(self)
                     },
                     
                     flip = function() {
                       
                       if(self$allow_flip) {
                         self$side <- if_else(self$side==1,-1,1)
                         
                         self$orientation <- case_when(self$orientation==3 ~1,
                                                       self$orientation==1 ~3,
                                                       T~ self$orientation)
                         
                         self$matrix <- self$matrix [c(dim(self$matrix)[1]:1),]
                         
                         self$edges <- get_edges(self$matrix)
                       }
                       invisible(self)  
                       
                     },
                     
                     print = function(...) {
                       cat("\nTile:", self$ID,".",self$orientation,".",self$side, "\n",sep = "")
                       sapply(1:dim(self$matrix)[1], function(i) {
                         self$matrix[i,] %>% str_c(collapse = "")
                       }) %>% 
                         str_c(collapse = "\n") %>% 
                         cat()
                       invisible(self)
                     },
                     
                     list_possible_edges = function() {
                       self$possible_edges <- self$edges %>% unlist()
                       for (i in 1:3) {
                         self$possible_edges <- c(self$possible_edges, self$rotate()$edges %>% unlist())
                       }
                       self$flip()
                       self$possible_edges <- c(self$possible_edges, self$edges %>% unlist())
                       for (i in 1:3) {
                         self$possible_edges <- c(self$possible_edges, self$rotate()$edges %>% unlist())
                       }
                       
                       self$flip()
                       
                       self$possible_edges <- self$possible_edges %>% unique()
                       return(self$possible_edges)
                     },
                     
                     initialize = function(position=NA_integer_, matrix, ID) {
                       self$position <- position
                       self$matrix <- matrix
                       self$edges <- get_edges(self$matrix)
                       stopifnot(names(self$edges) == c("up","right","down","left"))
                       self$ID <- ID
                       self$possible_edges <- self$list_possible_edges()
                       
                     }
                )
)

Tiles <- imap(tiles, function(tile, id) {
  Tile$new(
    matrix = tile,
    ID = id
  )
})

testTile <- Tiles[[2]]

testTile
testTile$edges
testTile$list_possible_edges()

get_all_possible_edges <- function() {
  lapply(Tiles, function(x) {
    if (x$available) x$possible_edges
  }) %>%
    purrr::reduce(c)
}

all_possible_edges <- get_all_possible_edges()

#seems like the same edge is never present more than twice
all_possible_edges %>% table() %>% table()

#I will use this amount of matching edges
sqrt(length(Tiles))*(sqrt(length(Tiles))-1)*2

# given an edge, find the tile(s) that can be attached to it
find_tiles_by_edge <- function(Tiles, edge) {
  map(Tiles, function(tile) {
    if (tile$available & stringi::stri_reverse(edge) %in% tile$possible_edges) return(tile$ID) 
  }) %>% unlist()
}

find_tiles_by_edge(Tiles, "#..###...#")
find_tiles_by_edge(Tiles, testTile$edges[["up"]]) %>% setdiff(testTile$ID)
find_tiles_by_edge(Tiles, testTile$edges[["down"]]) %>% setdiff(testTile$ID)


# for each tile and orientation, I can count how many matches I can find
# all non edge tiles in the middle must have 4 matches
# boarder tiles must have 3 or more
# corner must have 2 or moe

# for a tile, count how many edges can be matched by available edges
count_matches <- function(tile) {
  map(tile$edges, function(edge) {
    find_tiles_by_edge(Tiles, edge) %>% setdiff(tile$ID)
  }) %>% unlist() %>% unique() %>% length()
} 

count_matches(testTile)
map_dbl(Tiles, count_matches)

is_central_tile <- function(tile) {
 count_matches(tile) == 4
}

is_central_tile(testTile)

# this must be >= 100! #or >=1 for test input
sum(sapply(Tiles, is_central_tile))

# find tiles with only 2,3,4 matching edges
count_of_matches <- sapply(Tiles, function(tile) {
  count_matches(tile)
})
count_of_matches

# and those are the corners, edge tiles, and middle tiles
corner_tiles <- Tiles[(count_of_matches==2)[count_of_matches==2] %>% names()]
edge_tiles <- Tiles[(count_of_matches==2)[count_of_matches==3] %>% names()]

find_directions <- function(tile) {
  map(tile$edges, function(x) {
    r <- find_tiles_by_edge(Tiles, x) %>% setdiff(tile$ID)
    if (length(r) >0) {r}
  }
  ) %>% unlist() %>% names()
}

find_directions(testTile)

add_position <- function(direction) {
  recode(direction, 
         "up"="-1,0",
         "down"="1,0",
         "left"="0,-1",
         "right"="0,1") %>% 
    str_split(",", simplify = T) %>% 
    as.integer()
}

add_position("right")

# pick the first corner and put it in the right orientation and position
corner1 <- corner_tiles[[1]]

corner1$position <- (lapply(find_directions(corner1), add_position) %>% 
                       purrr::reduce(`+`) ==1 )%>% 
  ifelse(1,dim(the_frame$matrix)[1])

corner1$available <- F
corner1$allow_flip <- F
corner1$allow_rotation <- F

the_frame$matrix[corner1$position[1],corner1$position[2]] <- corner1$ID

# Add tile to frame
# 1 pick a tile on the frame
# pick a direction where there are available tiles
# find the tile and put it in the right position on the frame
# update tile position, allow roate etc
add_tile <- function(tile) {
  
  dir <- find_directions(tile)[1]
  
  found <- tryCatch(
    error = function(condition) {
      stop("no tile found in this direction", call. = T)
    },
    expr = {Tiles[[tile$edges[dir] %>% find_tiles_by_edge(Tiles, .) %>% setdiff(tile$ID)]]}
  )
  try <- 0
  while(found$edges[dir] %>% stringi::stri_reverse() != tile$edges[dir]) {
    try <- try+1
    found$rotate()
    if (try==4) {found$flip()}
    if (try >9) {stop("deh!")}
  }
  found$rotate()$rotate()
  found$position <- tile$position+add_position(dir)
  found$available <- F
  the_frame$matrix[found$position[1],found$position[2]] <- found$ID
  found
}

last_tile <- corner1
while(sum(the_frame$matrix=="empty")>0) {
  last_tile <- add_tile(last_tile)
}

# OF FUCKING YEAH
as.integer(c(the_frame$matrix[1,1],
the_frame$matrix[12,1],
the_frame$matrix[1,12],
the_frame$matrix[12,12])) %>% prod() %>% format(scientific=F)



# * - part 2 - *####
sea_monster <- read_lines("Day20/seamonster.txt") %>% str_replace_all(" ",".") 

sea_monsterL <- list(hidden = sea_monster,
                     regex = sea_monster %>% 
                       map_chr(str_replace_all,"[#]","[#]"),
                     found =sea_monster %>% 
                       map_chr(str_replace_all,"[#O]","O")
)

# crazy regex but I got it worked
#there must be an easy way
#must substitute A and B with str_locate indexes later
sea_monsterLx <- list(hidden = sea_monster,
                    regex = sea_monster %>% 
                      map_chr(str_replace_all,"[#]","[#]")%>% 
                      map_chr(function(x) {
                        pat <- str_replace_all(x,"(\\.+)","(\\1)")
                        str_c("^(.{A})",pat,"(.{B})$")}),
                    found =sea_monster %>% 
                      map_chr(str_replace_all,"[#O]","O") %>% 
                      map_chr(function(x) {
                        pat <- x
                        i <- 0
                        for (i in 1:str_count(pat,"\\.+")) {
                          pat <- str_replace(pat, "\\.+",str_c("\\\\",as.character(i+1)))
                          i<-i
                        }
                        str_c("\\1",pat,"\\",i+2)
                      })
)

cat(str_c(sea_monsterL$hidden,collapse = "\n"))

#create the image
image_matrix <- lapply(1:dim(the_frame$matrix)[1], function(i) {
  lapply(Tiles[the_frame$matrix[i,]], function(tile) {tile$matrix[2:9,2:9]}) %>% 
    purrr::reduce(cbind)
}) %>% purrr::reduce(rbind) 

rotate_matrix <- function(matrix) {
  t(apply(matrix, 2, rev))
}
flip_matrix <- function(matrix) {
  matrix [c(dim(matrix)[1]:1),]
}

print_image <- function(matrix) {
  matrix %>% 
    apply(1, str_c, collapse="")
}

image <- print_image(image_matrix)

# for testing
testimage_matrix <- read_lines("Day20/input20test2.txt") %>% 
  lapply(str_split,"",simplify=T) %>% 
  purrr::reduce(rbind)
testimage_matrix <- flip_matrix(testimage_matrix)
testimage_matrix <- rotate_matrix(testimage_matrix)
testimage <- print_image(testimage_matrix)
testimage2 <- print_image(cbind(testimage_matrix,testimage_matrix))


detect_monsters <- function(image=testimage2) {
  walk(2:(length(image)-1), function(line) {
    locate2all <- str_locate_all(image[line], sea_monsterL$regex[2])
    
    locate2 <- locate2all[[1]]
    
    #for (i in )
    if (length(locate2)==0) {return(F)} else {locate2 <- locate2[1,]}
    
    if (length(locate2)==2) {
      locate1 <- str_detect(str_sub(image[line-1],locate2[1],locate2[2]), sea_monsterL$regex[1])
      locate3 <- str_detect(str_sub(image[line+1],locate2[1],locate2[2]), sea_monsterL$regex[3])
        
        if (locate1&locate3) {
          # add replace # with 0 part
          image[line-1] <<- str_replace(image[line-1], 
                      sea_monsterLx$regex[1] %>% 
                        str_replace("A", as.character(locate2[1]-1)) %>% 
                        str_replace("B", as.character(nchar(image[1])-locate2[2])),
                      sea_monsterLx$found[1])
          image[line] <<- str_replace(image[line], 
                                       sea_monsterLx$regex[2] %>% 
                                         str_replace("A", as.character(locate2[1]-1)) %>% 
                                         str_replace("B", as.character(nchar(image[1])-locate2[2])),
                                       sea_monsterLx$found[2])
          image[line+1] <<- str_replace(image[line+1], 
                                     sea_monsterLx$regex[3] %>% 
                                       str_replace("A", as.character(locate2[1]-1)) %>% 
                                       str_replace("B", as.character(nchar(image[1])-locate2[2])),
                                     sea_monsterLx$found[3])
          
          #return(T)
          } else return(F)
 
    }else return(F)
  }) #%>% sum() 
  return(image)
}

detect_monsters(sea_monsterL$hidden)
detect_monsters(sea_monsterL$found)
detect_monsters(testimage)
detect_monsters(testimage2) #this must be fixed with str_locate_all
testimage2 <- detect_monsters(testimage2)
detect_monsters(testimage2)

sum(detect_monsters(testimage) %>% str_count("#"))

image_matrix <- rotate_matrix(image_matrix)
image_matrix <- flip_matrix(image_matrix)
image <-  print_image(image_matrix)

image <- detect_monsters(image)

sum(detect_monsters(image) %>% str_count("#"))

# need to add autorotate and auto iteretae detect monster a few times detect
# done interactively to get the solution

# this was CRAZY!

# Day 21: Allergen Assessment ####
input21 <- read_lines("Day21/input21.txt")
# * - part 1 - *####
# * - part 2 - *####

# Day 22: Crab Combat ####
input22 <- read_lines("Day22/input22.txt")

# * - part 1 - *####
deck1 <- input22[2:26] %>% as.numeric()
deck2 <- input22[29:53] %>% as.numeric()

play_game <- function(deck1, deck2) {
  while(length(deck1)!=0 & length(deck2)!=0) {
  
    c1 <- deck1[1]
    c2 <- deck2[1]
    get <- c(c1, c2) %>% sort(decreasing = T)
    deck1 <- deck1[-1]
    deck2 <- deck2[-1]
    
    if (c1>c2) {deck1 <- append(deck1, get)}
    else if (c1<c2) {deck2 <- append(deck2, get)}
    else {stop("there was a tie!")}
  }
  winner <- c(deck1, deck2)
  return(winner)
}
end <- play_game(deck1, deck2)
sum(end*seq_along(end) %>% sort(decreasing = T))

# * - part 2 - *####
deck1 <- c(9,2,6,3,1)
deck2 <- c(5,8,4,7,10)

deck1 <- c(2,43,19)
deck2 <- c(3,2,29,14)

maxdepth <- 0
depth <- 0

play_rgame <- function(deck1, deck2, subgame=F) {
  
  if (!exists("i")) {i <- 0}
  
  if (!exists("past_decks1")) {past_decks1 <- list()}
  if (!exists("past_decks2")) {past_decks2 <- list()}
  
  while(length(deck1)!=0 & length(deck2)!=0) {
    
    i<-i+1
    
    cat(paste("round",i, "\n"))
    cat("deck1:", deck1, "\ndeck2:",deck2, "\n")
    
    if (any(map2_lgl(past_decks1,past_decks2, function(d1,d2) identical(d1,deck1) & identical(d2,deck2)))) {
      win <- "deck1"
      assign("win", "deck1", pos = parent.frame(n = 1))
      print("looped")
      break
    }
    
    past_decks1[[length(past_decks1)+1]] <- deck1
    past_decks2[[length(past_decks2)+1]] <- deck2
    
    c1 <- deck1[1]
    c2 <- deck2[1]
    
    if (c1>c2) {
      win <- "deck1"
      }
    else if (c2>c1) {
      win <- "deck2"
    }
    
    else {stop("tie?")}
    
    cat(paste("t0:", win, "\n"))
    
    if (c1<=(length(deck1)-1) & c2 <= (length(deck2)-1)) {
      # subgame
      assign("depth", depth+1, envir = .GlobalEnv)
      assign("maxdepth", max(depth, maxdepth), envir = .GlobalEnv)
      play_rgame(deck1[2:(c1+1)], deck2[2:(c2+1)], subgame = T)
    }
    
    cat(paste("t1:", win, "\n"))
    
    deck1 <- deck1[-1]
    deck2 <- deck2[-1]
  
    if (win=="deck2") {
      get <- c(c2, c1)
      deck2 <- append(deck2, get)
    }
    
   else if (win=="deck1") {
      get <- c(c1, c2)
      deck1 <- append(deck1, get)
    }
    
    else {stop("no winner ?")}
    
  }
  
  if (length(deck1)==0) {
    cat("deck1 is 0 \n")
    assign("win", "deck2", pos = parent.frame(n = 1))
    }
  
  if (length(deck2)==0) {
    cat("deck2 is 0 \n")
    assign("win", "deck1", pos = parent.frame(n = 1))
    }
 
  print(paste(win, "won"))
  
  assign("depth", depth-1, envir = .GlobalEnv)
  
  if (!subgame) {
    winner <- c(deck1, deck2)
    return(winner)
  }
}

maxdepth
depth

end <- play_rgame(deck1, deck2)
sum(end*seq_along(end) %>% sort(decreasing = T))

# Day 23: Crab Cups ####
input23 <- c(1,2,3,4,8,7,5,9,6)


# * - part 1 - *####
cups <- input23

play_cups <- function(cups, rounds) {
  
  if (!exists("current")) {current<- cups[1]}
  size <- length(cups)
  
  for (i in 1:rounds) {
    print(i)
    pos <- which(current==cups)
    expand <- rep(cups, 2)
    next_cups <- expand[(pos+1):(pos+3)]
    left <- cups[!cups %in% next_cups]
    new_cup <- max(left[left < current])
    if (new_cup == -Inf) {new_cup <- max(left)}
    new_order <- c(left[1:which(left==new_cup)], next_cups, left[(which(left==new_cup)+1):length(left)])
    # fix for when new cup is the last one
    if (any(is.na(new_order))) {
      new_order <- c(left[1:which(left==new_cup)], next_cups)
      }
    current <- rep(new_order,2)[(which(new_order==current))+1]
    cups <- new_order
  }
  return(cups)
}

play_cups(cups = cups, rounds = 100)

# * - part 2 - *####
cups2 <- c(cups, 10:1000000)

play_cups2 <- function(cups, rounds) {}
play_cups2(cups2 = cups, rounds = 1e7)

# Day 24: Lobby Layout ####
# * - part 1 - *####
# * - part 2 - *####

# Day 25: Combo Breaker ####
# * - part 1 - *####
# * - part 2 - *####