# RTLF - R-Time-Leak-Finder

[TODO - Introduce RTLF]

**Please note:**  RTLF is a research tool intended for developers, pentesters, administrators and researchers. There is no GUI.

Dependencies
--
To use RTLF, it is necessary to have *R* (https://www.r-project.org/) and the *tidyverse* (https://www.tidyverse.org/) library installed.
Running

Usage
--
In order to run RTLF, you need to execute the *R* file:
```
Rscript RTLF.R
```

Docker
--
We provide a Dockerfile, allowing you to run RTLF directly:
```
docker build -t rtlf .
docker run rtlf <input_file> <output_file>
```

Input File
--
[TODO - Introduce BASELINE and MODIFIED]

The input file must have a specific format. The input should be structured as follows: 
```
V1,V2
BASELINE,494602
BASELINE,481100
MODIFIED,531296
MODIFIED,539770
...
```

Output File
--
[TODO - Explain out file in more detail]

RTLF outputs a *RDATA* file. It contains a list with five entries.  
