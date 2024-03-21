FROM alpine:3.19.1

# Dependencies
RUN apk add R R-dev R-doc g++ libxml2-dev fontconfig-dev harfbuzz-dev fribidi-dev freetype-dev libpng-dev tiff-dev libjpeg
RUN R -e "options(repos = c(CRAN = 'http://cran.rstudio.com/')); install.packages(c('tidyverse'), dependencies=TRUE, Ncpus = 6);"

# RTLF Scripts 
ADD RTLF_alpha_lowest.R /RTLF_alpha_lowest.R
ADD RTLF_alpha_0_9.R /RTLF_alpha_0_9.R
ADD RTLF_alpha_9.R /RTLF_alpha_9.R
ADD RTLF_alpha_18.R /RTLF_alpha_18.R

# Entrypoint
ENTRYPOINT ["Rscript", "/RTLF_alpha_9.R"]
