# RTLF - R-Time-Leak-Finder

RTLF is a new tool to statistically evaluate timing measurements with a type-1 error bounded by an input parameter $\alpha$. Details on RTLF can be found on our [paper](https://www.usenix.org/conference/usenixsecurity24/presentation/dunsche), which we published at USENIX Security 2024.

**Please note:**  RTLF is a research tool intended for developers, pentesters, administrators and researchers. There is no GUI.

Dependencies
--
To use RTLF, it is necessary to have *R* (https://www.r-project.org/) and the *tidyverse* (https://www.tidyverse.org/) library installed.

Usage
--
In order to run RTLF, you need to execute the *R* file:
```
Rscript <script_name> <input_file> <output_file>
```

Docker
--
We provide a Dockerfile, allowing you to run RTLF directly:
```
docker build -t rtlf .
docker run -v <source>:<target> rtlf <input_file> <output_file>
```

**Please note:**  The Dockerfile sets the version with $\alpha=0.09$ as the entry point. If you want to use other versions, adapt the entry point.

Input File - Requirements
--
The input file must have be a *CSV* that follows to a specific format. The file should be structured as follows: 
```
V1,V2
BASELINE,494602
BASELINE,481100
MODIFIED,531296
MODIFIED,539770
...
```
*BASELINE* describes the first series of measurements, and *MODIFIED* represents the second series. Both are compared to each other.

Output File - Format
--
The output file is an *RDATA* file that contains a list with five entries. If the first row contains a *1*, RTLF has detected a difference in the measurement series. The output looks like this, for example:
```
[[1]]
[1] 0 0 0 0 0 0 0 0 0
[[2]]
[1] 12 10 14 18 26 34 12 32 54
[[3]]
[1]  34.0  25.0  24.0  26.0  32.5  48.0  72.0 197.0 139.0
[[4]]
[1]  32  25  20  24  32  46  72 197 139
[[5]]
[1]  34.0  22.0  24.0  26.0  32.5  48.0  58.0 178.0 119.0
```