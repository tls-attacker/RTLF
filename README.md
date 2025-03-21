# RTLF - R-Time-Leak-Finder

RTLF is a statistical tool to evaluate timing measurements with a type-1 error bounded by an input parameter α. Details on RTLF can be found in our [paper](https://www.usenix.org/conference/usenixsecurity24/presentation/dunsche), published at USENIX Security 2024.

## Quick Start

```bash
# Run with default options (α=0.09)
./rtlf.R -i your_data.csv

# Run with minimal output and custom settings
./rtlf.R -i your_data.csv -a 0.05 -o results.json -q

# Get help with all available options
./rtlf.R --help
```

## Installation

### Prerequisites

RTLF requires:
- R (https://www.r-project.org/)
- The following R packages: tidyverse, optparse, jsonlite

#### Installing R
```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install r-base r-base-dev

# Fedora/RHEL/CentOS
sudo dnf install R

# macOS (using Homebrew)
brew install r

# Windows
# Download and install from https://cran.r-project.org/bin/windows/base/
```

#### Installing required R packages:
```r
install.packages(c("tidyverse", "optparse", "jsonlite"))
```

### Docker

For convenience, you can use our Docker image:

```bash
# Build the Docker image
docker build -t rtlf .

# Run with Docker (mounting current directory)
docker run -v $(pwd):/data rtlf -i /data/your_data.csv -o /data/results.json
```

## Input Formats

RTLF supports multiple input formats and automatically detects the separator (comma or semicolon):

### Classic Format
CSV with two columns: first column contains 'X' and 'Y' labels, second column contains the measurements:

**Using comma separator:**
```
V1,V2
X,494602
X,481100
Y,531296
Y,539770
...
```

**Using semicolon separator:**
```
V1;V2
X;494602
X;481100
Y;531296
Y;539770
...
```

### Column Format
CSV where each column represents a different measurement series:

**Using comma separator:**
```
Series1,Series2
494602,531296
481100,539770
...
```

**Using semicolon separator:**
```
Series1;Series2
494602;531296
481100;539770
...
```

### Single-Row Format
CSV where each row contains a single measurement with type and value:

**Using comma separator:**
```
Type,Value
X,494602
X,481100
X,485321
Y,531296
Y,539770
Y,535802
...
```

**Using semicolon separator:**
```
Type;Value
X;494602
X;481100
X;485321
Y;531296
Y;539770
Y;535802
...
```

## Command Line Options

```
Usage: rtlf.R [options]

Options:
  -a, --alpha=ALPHA       Threshold for type-1 error rate (e.g., 0.09 for 9% threshold) [default: 0.09]
  -i, --input=INPUT       Input CSV file or directory with timing measurements
  -o, --output=OUTPUT     Output file name or directory (supports .RDATA, .json, or .csv extension)
  -p, --pattern=PATTERN   File pattern to match when input is a directory (e.g., "*.csv") [default: *.csv]
  -r, --recursive         Recursively process subdirectories when input is a directory
  -q, --quiet             Suppress detailed analysis results
  -t, --threads=THREADS   Number of parallel threads to use when processing multiple files (0 = auto-detect) [default: 0]
  -h, --help              Show this help message and exit

Note: Headers and series names (X/Y or other values) are automatically detected from the input file.
The format is also automatically detected based on the input file structure.
```

## Output Formats

RTLF supports multiple output formats based on the file extension:

- `.RDATA`: R data format (compatible with original RTLF)
- `.json`: JSON format (human readable and machine parsable)
- `.csv`: CSV format (compatible with spreadsheet applications)

### Output Interpretation

The output contains results for nine deciles (10%, 20%, ..., 90%):

- **Decision**: Binary indicator (0/1) of whether a statistically significant difference was detected
- **Difference**: Absolute difference between the deciles of the two measurement series
- **Threshold**: Maximum expected variance for each decile, based on bootstrap
- **ThresholdX/Y**: Expected variance within each series

### Basic Statistics

RTLF includes basic statistics about your measurement data:

- **Count**: Number of measurements for each series
- **Min/Max**: Minimum and maximum values in each series
- **Mean**: Arithmetic mean of measurements in each series
- **Median**: Median value of each series
- **StdDev**: Standard deviation of measurements in each series

These statistics are displayed in the console output when running RTLF, and are also included in JSON and CSV outputs.

### Directory Processing

RTLF can process entire directories of measurement files:

- Specify a directory as the input (`-i path/to/directory/`)
- Optionally specify a pattern to match files (`-p "*.csv"`)
- Enable recursive processing with the `-r` flag
- Specify an output directory to store all results (`-o path/to/output/`)
- Utilize parallel processing with `-t` to speed up analysis of multiple files

When processing a directory, RTLF:
1. Maintains original filenames but adds the RTLF result suffix
2. Automatically uses parallel processing when multiple files are found (by default)
3. Generates a processing summary with:
   - Count of files processed
   - Number of files with differences detected
   - Number of files with no differences detected
   - Any errors encountered
4. Provides a detailed table of results for each file
5. Creates a JSON summary file with per-file results and overall statistics

#### Parallel Processing

RTLF automatically uses multiple CPU cores when processing directories:

- By default, RTLF auto-detects the number of available cores (`-t 0`)
- You can specify the exact number of cores to use with `-t N`
- For optimal system performance, the auto-detection uses (available cores - 1)
- For small numbers of files, RTLF will not use more cores than necessary

This parallel processing can significantly speed up batch analysis of large directories.

#### Directory Summary File

When processing a directory, RTLF generates a comprehensive JSON summary file that includes:

```json
{
  "metadata": {
    "timestamp": "2025-03-21 12:34:56",
    "alpha": 0.09,
    "input_directory": "examples/",
    "files_processed": 5,
    "files_with_errors": 0,
    "files_with_differences": 4,
    "files_with_no_differences": 1
  },
  "file_results": [
    {
      "file_name": "example-1.csv",
      "full_path": "/path/to/examples/example-1.csv",
      "result_code": "DIFFERENCE",
      "status": "success",
      "difference_detected": true
    },
    {
      "file_name": "example-2.csv",
      "full_path": "/path/to/examples/example-2.csv",
      "result_code": "NO_DIFFERENCE",
      "status": "success",
      "difference_detected": false
    }
  ]
}
```

The summary file is saved in the output directory with a timestamp in the filename.


### JSON Output Format

When saving as JSON (by specifying a filename with `.json` extension), RTLF produces a structured JSON object with metadata and detailed results:

```json
{
  "metadata": {
    "timestamp": "2025-03-21 10:15:30",
    "alpha": 0.09,
    "input_file": "examples/example-1.csv",
    "samples": 100,
    "bootstrap_iterations": 10000,
    "difference_detected": true,
    "significant_deciles": [30, 40, 50, 60],
    "exit_code": 11
  },
  "statistics": {
    "series_x": {
      "count": 30000,
      "min": 481000,
      "max": 495000,
      "mean": 488000,
      "median": 488000,
      "stdev": 4000
    },
    "series_y": {
      "count": 30000,
      "min": 530000,
      "max": 540000,
      "mean": 535000,
      "median": 535000,
      "stdev": 3000
    }
  },
  "results": [
    {
      "Decile": "10%",
      "Decision": 0,
      "Difference": 14,
      "Threshold": 22,
      "ThresholdX": 22,
      "ThresholdY": 22
    },
    ...
  ]
}
```

## Examples

```bash
# Analyze data with JSON result (detailed output by default)
./rtlf.R -i examples/example-1.csv -o results.json

# Analyze data with minimal output
./rtlf.R -i my_data.csv -q

# Output results in different formats
./rtlf.R -i examples/example-1.csv -o results.json  # JSON output
./rtlf.R -i examples/example-1.csv -o results.csv   # CSV output
./rtlf.R -i examples/example-1.csv -o results.RDATA # R data format

# Examples with different formats (format is auto-detected)
./rtlf.R -i examples/example-columns.csv
./rtlf.R -i examples/example-rows.csv
./rtlf.R -i examples/example-single-row.csv

# Examples with different separators (comma or semicolon)
./rtlf.R -i examples/example-1.csv          # Uses comma separator
./rtlf.R -i examples/example-semicolon.csv  # Uses semicolon separator

# Directory processing examples
./rtlf.R -i examples/                       # Process all CSV files in examples directory
./rtlf.R -i examples/ -o results/           # Process files from examples/, save results to results/
./rtlf.R -i examples/ -p "*.csv" -r         # Process all CSV files recursively
./rtlf.R -i examples/ -o results/ -q        # Batch process with quiet output
./rtlf.R -i examples/ -t 4                  # Process using exactly 4 CPU cores
./rtlf.R -i examples/ -t 0 -r               # Process recursively using auto-detected cores
```

## Testing

RTLF includes a comprehensive test script that validates all functionality:

```bash
# Run the test suite (tests all features and parameter combinations)
./test_rtlf.sh
```

The test script:
1. Creates a temporary directory for test outputs
2. Tests all parameter combinations against example files
3. Validates the output formats
4. Checks for correct exit codes

This is useful for verifying that RTLF is working correctly after installation or updates.

## Exit Codes

- **1**: Error processing one or more files
- **10**: No statistically significant difference detected
- **11**: Statistically significant difference detected

When processing directories, the exit code is:
- **1** if any files had errors
- **11** if at least one file had a statistically significant difference
- **10** if all files were processed successfully with no differences detected
