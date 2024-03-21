library(tidyverse)

#-------------------------------------------------------------------------------

# Arguments: alpha inputFile(CSV) outputFile(RDATA)
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 3) {
  stop("RTLF requires exactly 3 arguments: <alpha> <src file (CSV)> <output file (RDATA)>")
}

alpha <- as.numeric(args[1])
if (is.na(alpha) || alpha >= 1) {
  stop("Alpha should be a decimal number < 1")
}
alphaPerDecile = alpha / 9

inputFile <- toString(args[2], width = NULL)
outputFile <- toString(args[3], width = NULL)
data <- read.csv(file = inputFile)
namedData <- data %>% mutate(V1 = recode(V1, "X" = "1"))  %>%  mutate(V1 = recode(V1, "Y" = "2"))
n <- min(length((namedData %>% select(V1, V2) %>% filter(V1 == "1"))$V2), length((namedData %>% select(V1, V2) %>% filter(V1 == "2"))$V2))
B <- 10000

#-------------------------------------------------------------------------------

autotest <- function(data, n, B) {
  bb1 <- data %>% select(V1, V2) %>% filter(V1 == "1")
  bb2 <- data %>% select(V1, V2) %>% filter(V1 == "2")
  q1 <- replicate(B, bootstrap1(as.numeric(bb1$V2), n))
  q2 <- replicate(B, bootstrap1(as.numeric(bb2$V2), n))
  maxq1 <- apply(q1, 1, quantile, probs = 1 - alphaPerDecile, type = 2)
  maxq2 <- apply(q2, 1, quantile, probs = 1 - alphaPerDecile, type = 2)
  maxqs <- matrix(c(maxq1, maxq2), nrow = 9, ncol = 2)
  qmax <- apply(maxqs, 1, max)
  t <- test(as.numeric(bb1$V2), as.numeric(bb2$V2))
  dec <- rep(0, 9)
  for (l in 1:9) {
    if (t[l] > qmax[l]) {
      dec[l] <- dec[l] + 1
    }
  }
  return(list(dec, t, qmax, maxq1, maxq2))
}

test <- function(td1, td2) {
  q1 <- quantile(td1, probs = seq(0.1, 0.9, 0.1), names = FALSE, type = 2)
  q2 <- quantile(td2, probs = seq(0.1, 0.9, 0.1), names = FALSE, type = 2)
  t1 <- abs(q1 - q2)
  return(t1)
}

bootstrap1 <- function(dat, n) {
  x1 <- sample(dat, n, replace = TRUE)
  x2 <- sample(dat, n, replace = TRUE)
  return(test(x1, x2))
}

#-------------------------------------------------------------------------------

output <- autotest(namedData, n, B)
save(output, file = outputFile)
if(max(output[[1]]) > 0) {
  indices <- which(output[[1]] > 0)
  cli_message <- paste0("Test determined difference for the following quantile indices: ", paste(indices, collapse=", "), ".")
  print(cli_message)
  quit(status = 11) # test determined difference
}else{
  print("Test determined no difference.")
  quit(status = 10) # test determined no difference
}
