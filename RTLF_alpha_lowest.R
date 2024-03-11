library(tidyverse)

#-------------------------------------------------------------------------------

# Arguments: inputFile(CSV) outputFile(RDATA)
args <- commandArgs(trailingOnly = TRUE)
inputFile <- toString(args[1], width = NULL)
outputFile <- toString(args[2], width = NULL)
data <- read.csv(file = inputFile)
namedData <- data %>% mutate(V1 = recode(V1, "BASELINE" = "1"))  %>%  mutate(V1 = recode(V1, "MODIFIED" = "2"))
n <- min(length((namedData %>% select(V1, V2) %>% filter(V1 == "1"))$V2), length((namedData %>% select(V1, V2) %>% filter(V1 == "2"))$V2))
B <- 10000

#-------------------------------------------------------------------------------

autotest <- function(data, n, B) {
  bb1 <- data %>% select(V1, V2) %>% filter(V1 == "1")
  bb2 <- data %>% select(V1, V2) %>% filter(V1 == "2")
  q1 <- replicate(B, bootstrap1(as.numeric(bb1$V2), n))
  q2 <- replicate(B, bootstrap1(as.numeric(bb2$V2), n))
  maxq1 <- apply(q1, 1, quantile, probs = 1, type = 2)
  maxq2 <- apply(q2, 1, quantile, probs = 1, type = 2)
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