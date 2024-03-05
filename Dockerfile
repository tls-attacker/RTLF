FROM alpine:3.19.1

# Dependencies
RUN apk add R R-dev R-doc g++ libxml2-dev
RUN R -e "options(repos = c(CRAN = 'http://cran.rstudio.com/')); install.packages(c('tidyverse'), dependencies=TRUE);"

# Setup
ADD RTLF.R /RTLF.R
ENTRYPOINT ["Rscript", "/RTLF.R"]