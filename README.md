# FluoR

**Fluorescence-based DNA quantification and microplate analysis**

FluoR is an R/Shiny application for fluorescence-based DNA quantification and microplate analysis, featuring optional fluorescence decay correction based on pipetting order.

FluoR provides an interactive workflow for importing fluorescence measurements, defining microplate layouts, analysing standards and samples, correcting fluorescence decay, and calculating DNA concentrations.

## Features

- Automatic and manual import of fluorescence measurements from Excel files.
- Simultaneous analysis of multiple 96-well microplates.
- Automatic detection of standard layouts.
- Interactive assignment of standard and sample wells.
- Import and export of reusable microplate layouts.
- Configurable pipetting order.
- Optional multiplicative fluorescence decay correction.
- DNA quantification using two-point calibration.
- Interactive fluorescence maps and calibration plots.
- Summary statistics and quality control flags.
- Export of results, standards, calibration data and model parameters to Excel.
- Standalone Windows desktop application.

## Currently supported assays

- Qubit dsDNA High Sensitivity (HS).
- Qubit dsDNA Broad Range (BR).

## Currently supported plate readers

- BioTek Synergy H1.

## Installation

### Windows desktop application

1. Visit the [Releases](https://github.com/jjsanchezgil/FluoR/releases) page.
2. Download the Windows installer.
3. Run the installer.
4. Launch FluoR.

The standalone application includes the R runtime and required R packages. No separate R installation or internet connection is required to run FluoR after installation.

The current installer targets 64-bit Windows.

### Running from R

Alternatively, FluoR can be executed directly from its R source code.

Install the required R packages:

```r
install.packages(c(
  "shiny",
  "bslib",
  "readxl",
  "writexl",
  "dplyr",
  "ggplot2",
  "DT",
  "plotly",
  "scales"
))
```

Download the source code and extract it to a local directory. Set the working directory to the FluoR project root and run:

```r
shiny::runApp()
```

## Usage

### 1. Import

Select the appropriate DNA quantification assay and upload the Excel files containing fluorescence measurements. Configure the standards and sample data sources using automatic detection or manual configuration.

### 2. Standards

Review or assign the wells corresponding to the DNA standards. When fluorescence decay correction is enabled, both initial and final ST1 and ST2 standards are required.

### 3. Samples

Assign sample wells for each microplate. Layouts can be imported, exported or applied to multiple sample plates.

### 4. Pipetting order

Define the pipetting sequence across standards and sample plates to account for fluorescence changes associated with the order of assay preparation.

### 5. Data processing

Configure the standard, sample and final assay volumes. Inspect the fluorescence decay diagnostics and DNA calibration plot.

Fluorescence decay correction can be disabled when it is not required.

### 6. Results

Inspect the calculated DNA concentrations and quality control flags. Export the results as an Excel workbook containing:

- Results
- Standards
- Calibration
- Model parameters

## Fluorescence decay correction

FluoR implements an optional multiplicative fluorescence decay model using initial and final DNA standards.

The model estimates changes in fluorescence associated with pipetting order using a log-linear regression. Fluorescence measurements are subsequently corrected relative to the first pipetting step before calibration and DNA quantification.

When decay correction is disabled, quantification is performed directly using the initial standards and uncorrected fluorescence measurements.

## Citation

If you use FluoR in your research, please cite the software in any resulting scientific publication and include the version used.

Citation information is provided in the `CITATION.cff` file and through the **Cite this repository** option on GitHub.

## License

FluoR is distributed under the MIT License. See [LICENSE](LICENSE) for details.

## Feedback and contributions

Bug reports, suggestions and contributions are welcome through the [GitHub Issues](https://github.com/jjsanchezgil/FluoR/issues) page.

## Disclaimer

FluoR is research software. Users are responsible for validating the suitability of the calculations and results for their specific experimental conditions.
