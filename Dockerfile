FROM alpine:3.19.1

# Dependencies
RUN apk add R R-dev R-doc g++ libxml2-dev fontconfig-dev harfbuzz-dev fribidi-dev freetype-dev libpng-dev tiff-dev libjpeg
RUN R -e "options(repos = c(CRAN = 'http://cran.rstudio.com/')); install.packages(c('tidyverse', 'optparse', 'jsonlite', 'crayon'), dependencies=TRUE, Ncpus = 6);"

# Set up work directory
WORKDIR /app

# Copy RTLF Scripts 
COPY rtlf.R /app/rtlf.R
RUN chmod +x /app/rtlf.R

# Create data directory
RUN mkdir -p /data
VOLUME /data
WORKDIR /data

# Entrypoint
ENTRYPOINT ["/app/rtlf.R"]