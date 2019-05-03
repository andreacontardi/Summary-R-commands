###############################################Count the Digit#####################################
#Take an integer n (n >= 0) and a digit d (0 <= d <= 9) as an integer. Square all numbers k (0 <= k <= n) between 0 and n. 
#Count the numbers of digits d used in the writing of all the k**2. Call nb_dig (or nbDig or ...) the function taking n and d
#as parameters and returning this count.

library(stringr)
options("scipen"=100, "digits"=15)

nbDig <- function(n, d) {
  a <- 1:n
  for (i in a)
  {a[i] <- i**2}
  if (n>0) {sum(str_count(as.character(c(0,a)), as.character(d)))}
  else {sum(0==d)}
}


####################################Consecutive strings###########################################
#You are given an array strarr of strings and an integer k. 
#Your task is to return the first longest string consisting of k consecutive strings taken in the array.

longestConsec <- function (strarr, k) {
  l=length(strarr)
  str <- 1:(l-k+1)
  if (l == 0 | k > l | k <= 0) {""}
  else {
    for (i in 1:(l-k+1)) {
      str[i] <- paste(strarr[i:(i+k-1)], collapse = "") #consecutive strings k words
    }
    str <- as.data.frame(t(rbind(str, nchar(str)))) #number of characters
    str <- str[order(as.numeric(as.character(str[,2])), decreasing = T),] #order by number of characters
    print(as.character(str[1,1])) #print the first one
  }
}


#################################Are they the "same"?#####################################
#Given two arrays a and b write a function comp(a, b) (compSame(a, b) in Clojure) that checks whether the two arrays 
#have the "same" elements, with the same multiplicities. "Same" means, here, that the elements in b are the elements in a 
#squared, regardless of the order.

comp <- function(a1, a2) {
  if (length(a1)<length(a2) | length(a1)>length(a2) | class(a1)!="numeric" | class(a2)!="numeric") {FALSE}
  else {
    l=length(a2)
    somma=1:l
    for (i in 1:l) {
      somma[i]=ifelse(sum(a2[i]==a1**2)>0,1,0)
    }
    if (sum(somma)==l & sum(a2)==sum(a1**2)) {TRUE} 
    else {FALSE}
  }
}


###################################Printer Errors###################################
#In a factory a printer prints labels for boxes. For one kind of boxes the printer has to use colors which, for the sake of simplicity, 
#are named with letters from a to m.
#The colors used by the printer are recorded in a control string. For example a "good" control string would be aaabbbbhaijjjm 
#meaning that the printer used three times color a, four times color b, one time color h then one time color a...
#Sometimes there are problems: lack of colors, technical malfunction and a "bad" control string is produced e.g. aaaxbbbbyyhwawiwjjjwwm with letters 
#not from a to m.
#You have to write a function printer_error which given a string will output the error rate of the printer as a string 
#representing a rational whose numerator is the number of errors and the denominator the length of the control string. 
#Don't reduce this fraction to a simpler expression.

printerError <- function(s) {
  l <- nchar(s)
  letter <- rep(0,l)
  for (i in 1:l) {
    letter[i] <- ifelse(substr(s, i, i) %in% letters[1:13],0,1)
  }
  paste(c(sum(letter), l), collapse = "/")
}