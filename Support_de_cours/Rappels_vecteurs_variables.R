install.packages("swirl")
library(swirl)
library(tidyr)
library(dplyr
install_course("The R Programming Environment")
x <- 1 
print(x)
msg <- "hello"
x <- 11:30
x
x <- 1L
x <- c(0.5, 2, 0.6)
x <- c(FALSE,2)
x <- c("a", "b", "c")
x <- vector("numeric",length=10)
y <- c(1.7, "a")   ## character
y <- c(TRUE, 2)    ## numeric
y <- c("a", TRUE)  ## character
x <- 1:6
cclass(x)
as.numeric(x)
class(x)
x
as.logical(x)
as.character(x)
m <- matrix(1:10,nrow = 2, ncol = 5)
dim(m)
attributes(m)
m <- 1:10
dim(m) <- c(2,5)
dim(m) <- cbind(2,5) ## Pareil que c remplit par colonne 


x <- 1:3
y <- 10:12
cbind(x, y)
rbind(x, y) 

x <- list(1,"n",TRUE)
x <- vector("list",5)

x <- factor(c("yes", "yes", "no", "yes", "no"), levels=c("yes","no")) 
table(x)

unclass(x)  

x <- data.frame(foo=1:4,bar=c(T,T,F,F))



VADeaths %>%
  tbl_df() %>%
  mutate(age = row.names(VADeaths)) %>%
  gather(key, death_rate, -age) %>%
  separate(key, c("urban", "gender"), sep = " ") %>%
  mutate(age = factor(age), urban = factor(urban), gender = factor(gender))

#reading tabular data

logs <- read_csv("data/2016-07-19.csv.gz", n_max = 10)

Parsed with column specification:
  cols(
    date = col_date(format = ""),
    time = col_time(format = ""),
    size = col_integer(),
    r_version = col_character(),
    r_arch = col_character(),
    r_os = col_character(),
    package = col_character(),
    version = col_character(),
    country = col_character(),
    ip_id = col_integer()
  )

logs <- read_csv("data/2016-07-20.csv.gz", col_types = "ccicccccci", n_max = 10)

