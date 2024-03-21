FROM alpine:3.19.1

# Dependencies
RUN apk add R R-dev R-doc g++ libxml2-dev fontconfig-dev harfbuzz-dev fribidi-dev freetype-dev libpng-dev tiff-dev libjpeg
RUN R -e "options(repos = c(CRAN = 'http://cran.rstudio.com/')); install.packages(c('tidyverse'), dependencies=TRUE, Ncpus = 6);"

# RTLF Scripts 
ADD RTLF.R /RTLF.R

# Entrypoint
ENTRYPOINT ["Rscript", "/RTLF.R"]
