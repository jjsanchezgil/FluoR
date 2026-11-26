# FluoR

**Fluorescence-based DNA quantification and microplate analysis**

![Version](https://img.shields.io/badge/version-0.1.0-blue)
![License](https://img.shields.io/badge/license-MIT-green)
![Platform](https://img.shields.io/badge/platform-Windows%20x64-blue)
![R](https://img.shields.io/badge/language-R-276DC3)
![Shiny](https://img.shields.io/badge/framework-Shiny-447099)

FluoR is an R/Shiny application for fluorescence-based DNA quantification and microplate analysis, featuring optional fluorescence decay correction based on pipetting order.

FluoR provides an interactive workflow for importing fluorescence measurements, defining microplate layouts, analysing standards and samples, correcting systematic changes in fluorescence, and estimating DNA concentrations.

The application is available both as an R/Shiny application and as a standalone Windows desktop application that does not require a separate R installation.

## Table of contents

- [Features](#features)
- [Current compatibility](#current-compatibility)
- [Installation](#installation)
- [User manual](#user-manual)
  - [Import](#1-import)
  - [Standards](#2-standards)
  - [Samples](#3-samples)
  - [Pipetting order](#4-pipetting-order)
  - [Data processing](#5-data-processing)
  - [Results](#6-results)
- [Quantification methodology](#quantification-methodology)
- [Example datasets](#example-datasets)
- [Development](#development)
- [Citation](#citation)
- [License](#license)
- [Feedback and contributions](#feedback-and-contributions)
- [Disclaimer](#disclaimer)

## Features

- Automatic and manual import of fluorescence measurements from Excel files.
- Simultaneous analysis of multiple 96-well microplates.
- Automatic detection of standard layouts.
- Interactive assignment of standard and sample wells.
- Import and export of reusable microplate layouts.
- Interactive visualisation of raw fluorescence measurements.
- Descriptive statistics for standards and samples.
- Automatic and manual reconstruction of pipetting order.
- Column-based and individual well-based pipetting sequences.
- Optional multiplicative fluorescence decay correction.
- Interactive fluorescence decay diagnostics.
- Two-point DNA calibration using replicate standard measurements.
- Background correction and DNA concentration estimation.
- Sample-level quality control flags.
- Export of results and model parameters to Excel.
- Standalone Windows distribution with a bundled R runtime.

## Current compatibility

FluoR is designed for fluorescence-based DNA quantification. The currently implemented assays, instruments and microplate formats are listed below.

### DNA quantification assays

| Assay | ST1 concentration (ng/µL) | ST2 concentration (ng/µL) |
|---|---:|---:|
| Qubit dsDNA High Sensitivity (HS) | 0 | 10 |
| Qubit dsDNA Broad Range (BR) | 0 | 100 |

### Microplate readers

- BioTek Synergy H1.

### Microplate formats

- 96-well microplates (8 rows × 12 columns).

### Input file formats

- Microsoft Excel (`.xlsx`).
- Microsoft Excel legacy format (`.xls`).

Fluorescence measurement files must contain well identifiers and corresponding fluorescence intensities. Manual import configuration allows users to specify the location of these data within a worksheet.

Compatibility with other assays, instruments and microplate formats may be extended in future versions.

## Installation

### Windows desktop application

The standalone Windows version is the recommended installation method for users who do not require access to the R source code.

**System requirements**

- 64-bit Windows operating system.
- Sufficient disk space for the application and bundled dependencies.

**Installation instructions**

1. Visit the [FluoR Releases page](https://github.com/jjsanchezgil/FluoR/releases/latest).
2. Download `FluoR-Setup-0.1.0.exe`.
3. Run the installer.
4. Launch FluoR from the desktop shortcut or Windows Start menu.

**No separate installation of R, RStudio or R packages is required.**

The installer contains the R runtime and all dependencies required to run FluoR. The application can be used offline.

The Windows installer is currently unsigned. Windows may therefore display a security warning when opening the installer.

Additional source archives available on the Releases page are provided for licensing and transparency purposes. They are not required to install or run FluoR.

### Running FluoR from R

FluoR can alternatively be executed directly from its source code.

#### Requirements

- R.
- The required R packages listed below.
- RStudio (optional).

#### Install R dependencies

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

#### Download the source code

Download the repository using the **Code → Download ZIP** option on GitHub and extract it to a local directory.

Alternatively, clone the repository:

```bash
git clone https://github.com/jjsanchezgil/FluoR.git
```

#### Launch FluoR

Set the R working directory to the FluoR project root, containing the `app.R` file and the `R/` directory.

Run:

```r
shiny::runApp()
```

Alternatively, open `app.R` in RStudio and click **Run App**.

## User manual

FluoR is organised into six main tabs that guide the user through the fluorescence quantification workflow.

| Tab | Purpose |
|---|---|
| Import | Load and configure fluorescence measurement files |
| Standards | Define and inspect DNA standards |
| Samples | Define and inspect sample microplates |
| Pipetting order | Reconstruct the assay preparation sequence |
| Data processing | Configure quantification and inspect diagnostic plots |
| Results | Review and export DNA concentration estimates |

### 1. Import

The Import tab is used to load fluorescence measurements and configure the data sources for standards and sample plates.

#### Select the assay

Use the **Qubit assay** selector in the sidebar to choose the appropriate assay:

- dsDNA High Sensitivity (HS).
- dsDNA Broad Range (BR).

The selected assay determines the reference DNA concentration of ST2.

#### Upload fluorescence measurements

Use **Upload Synergy H1 files** to upload one or multiple Excel files containing fluorescence measurements.

FluoR reads the worksheets contained in the uploaded files and identifies those containing compatible plate measurements.

Each compatible worksheet can be selected as an individual data source.

#### Configure the standards data source

Select the file and worksheet containing the DNA standards.

FluoR provides two import modes:

**Automatic detection**

FluoR searches the worksheet for a header row containing a well identifier column and a fluorescence measurement column.

**Manual configuration**

If automatic detection is unsuccessful, specify:

- Header row.
- Well column number.
- RFU column number.

Row and column numbers are counted from 1.

Well identifiers such as `A1` and `A01` are accepted and converted to a consistent format.

#### Configure sample plates

FluoR automatically creates sample plate sections for additional compatible worksheets.

Each sample plate can be configured independently using its file, worksheet and import settings.

Use **Add sample plates** to add further sample plates manually or **Remove** to exclude an unnecessary plate.

Before proceeding, verify that the correct worksheets have been assigned to the standards and sample plates.

### 2. Standards

The Standards tab is used to define the wells containing DNA standards and inspect their fluorescence measurements.

#### Fluorescence decay correction

The **Correct fluorescence decay** option determines whether fluorescence measurements are corrected according to pipetting order.

This option is enabled by default.

When correction is enabled, four standard categories are available:

| Category | Description |
|---|---|
| ST1 initial | Blank standard prepared at the beginning |
| ST2 initial | DNA standard prepared at the beginning |
| ST1 final | Blank standard prepared at the end |
| ST2 final | DNA standard prepared at the end |

When correction is disabled, only ST1 and ST2 initial standards are required.

For fluorescence decay correction, the experimental design must include initial and final standard measurements.

#### Automatic standard detection

FluoR attempts to identify standard layouts automatically from the fluorescence measurements.

The current detection algorithm searches for columns containing:

- ST1 replicates in rows A–D.
- ST2 replicates in rows E–H.
- A sufficiently large fluorescence difference between ST1 and ST2.
- Relatively consistent ST2 replicate measurements.

When fluorescence decay correction is enabled, two recognisable standard columns are required for automatic assignment.

The leftmost identified column is assigned to the initial standards and the rightmost identified column to the final standards.

When correction is disabled, only one standard column is needed.

If automatic detection is unsuccessful or ambiguous, standard wells can be assigned manually.

Automatically detected layouts should always be checked against the experimental design.

#### Manual standard assignment

Select a standard category and click or drag across the interactive microplate layout to assign wells.

Select **Unassigned** to exclude wells from the analysis.

Standards can be assigned to arbitrary well positions when manual configuration is used.

#### Raw fluorescence visualisation

The fluorescence map displays the measured fluorescence intensity of each well.

Hover over individual wells to inspect their RFU values.

Selected wells are highlighted to facilitate comparison between the assigned standard layout and the original measurements.

#### Standard statistics

FluoR calculates descriptive statistics separately for each standard category:

- Number of assigned wells.
- Number of measured wells.
- Number of missing measurements.
- Number of negative measurements.
- Mean fluorescence.
- Standard deviation.
- Coefficient of variation (CV).

These statistics facilitate the identification of inconsistent standard replicates and potential experimental problems.

#### Standard layout files

Standard layouts can be saved and reused in subsequent experiments.

Use:

- **Import layout from Excel** to load an existing layout.
- **Download layout** to export the current layout.
- **Clear all assignments** to reset the layout.

The supported standard layout identifiers include:

| Identifier | Category |
|---|---|
| `ST1_ini` | ST1 initial |
| `ST2_ini` | ST2 initial |
| `ST1_fin` | ST1 final |
| `ST2_fin` | ST2 final |
| Empty cell | Unassigned |

### 3. Samples

The Samples tab provides an independent interactive layout editor for each sample microplate.

#### Assign sample wells

Select **Sample** and click or drag across the microplate layout to assign wells.

Select **Unassigned** to exclude wells from quantification.

Use **Assign all** to assign all wells containing finite fluorescence measurements.

Use **Apply to all** to copy the current sample layout to all other sample plates.

This is particularly useful when multiple microplates share the same experimental layout.

#### Raw fluorescence visualisation

Each sample plate is displayed alongside its interactive layout.

The fluorescence map allows users to inspect individual fluorescence measurements and identify spatial patterns, missing measurements or unusual signals.

#### Sample statistics

For each sample plate, FluoR provides:

- Number of assigned wells.
- Number of measured wells.
- Number of missing measurements.
- Number of negative measurements.
- Mean fluorescence.
- Standard deviation.
- Coefficient of variation.

The selected well measurements are also displayed in a searchable table containing raw and log-transformed fluorescence values.

#### Sample layout files

Sample layouts can be imported from or exported to Excel.

A sample layout consists of an 8 × 12 grid representing the microplate.

Cells labelled `Sample` are included in the analysis, whereas empty cells are treated as unassigned wells.

Use **Download layout** to generate a reusable layout file.

### 4. Pipetting order

The Pipetting order tab reconstructs the sequence in which standards and samples were prepared.

This information is used to estimate and correct systematic changes in fluorescence associated with assay preparation order.

The ordering method should reflect the actual experimental pipetting procedure.

#### By column

This is the default ordering method.

All assigned wells belonging to the same column within a preparation group receive the same pipetting order.

This mode is appropriate when samples are prepared by column, for example using a multichannel pipette.

The default preparation sequence is:

1. Initial standards.
2. Sample plates, in their configured order.
3. Final standards.

Within each group, columns are ordered from left to right.

#### By well

Each assigned well receives an individual pipetting order.

Within each preparation group, wells are ordered by column and then by row (A–H).

This mode is appropriate when wells are prepared individually following this sequence.

#### Manual

Manual ordering allows users to reconstruct alternative preparation sequences.

Select the manual selection unit:

- Column.
- Well.

Click the corresponding assigned columns or wells in the order in which they were prepared.

Use **Undo last** to remove the most recently assigned step or **Reset order** to restart the sequence.

All assigned preparation units must receive an order when fluorescence decay correction is enabled.

#### Preparation sequence

The preparation sequence table displays the assigned pipetting order for standards and samples.

Review this table before proceeding to data processing.

Pipetting order represents preparation steps rather than directly measured elapsed time.

When fluorescence decay correction is disabled, pipetting order is not required for DNA quantification.

### 5. Data processing

The Data processing tab contains the quantification settings, fluorescence decay diagnostics and DNA calibration plot.

#### Quantification settings

| Parameter | Default value | Description |
|---|---:|---|
| Standard volume | 10 µL | Volume of DNA standard added to each standard assay well |
| Sample volume | 2 µL | Volume of DNA sample added to each sample assay well |
| Final assay volume | 200 µL | Total reaction volume in each assay well |

These values must correspond to the experimental protocol.

The standard and sample volumes are used to estimate the original DNA concentration of the samples.

The final assay volume is used to calculate and visualise DNA concentrations within the assay wells.

The current implementation assumes a common final assay volume across standards and samples.

#### Multiplicative fluorescence decay

When correction is enabled, FluoR estimates a multiplicative fluorescence change associated with pipetting order.

The following model parameters are displayed:

- Decay coefficient (γ).
- Estimated percentage change in fluorescence per pipetting step.
- Reference pipetting step.

Two diagnostic plots are provided:

**Before correction**

Displays the measured fluorescence of the standards against pipetting order, together with predictions from the multiplicative model.

**After correction**

Displays the corrected fluorescence measurements relative to the first pipetting step.

The plots should be inspected to assess whether the model provides a reasonable representation of the observed fluorescence changes.

#### DNA calibration in assay wells

The calibration plot displays the relationship between corrected fluorescence, DNA concentration and DNA amount within the final assay volume.

The plot includes:

- ST1 standard measurements.
- ST2 standard measurements.
- Sample measurements.
- The two-point calibration line.
- Extrapolation above ST2, when applicable.

The left vertical axis represents DNA concentration within the assay well (ng/µL).

The right vertical axis represents DNA amount within the assay well (ng).

Hover over individual observations to inspect their measurement and quantification information.

Measurements above ST2 should be interpreted with caution because they exceed the calibration interval.

### 6. Results

The Results tab displays the calculated DNA concentrations and associated quality control information for all assigned sample wells.

#### Results table

| Column | Description |
|---|---|
| Plate | Sample plate name |
| PlateID | Internal plate identifier |
| Well | Microplate well identifier |
| RFU | Original fluorescence measurement |
| log10 RFU | Base-10 logarithm of raw fluorescence |
| Order | Assigned pipetting order |
| Corrected RFU | Fluorescence after optional decay correction |
| log10 cRFU | Base-10 logarithm of corrected fluorescence |
| Background-corrected RFU | Fluorescence after background subtraction and application of the numerical lower bound |
| Assay concentration (ng/uL) | Standard-volume-equivalent DNA concentration |
| DNA concentration (ng/uL) | Estimated original sample DNA concentration |
| QC | Quality control flag |

When fluorescence decay correction is disabled, corrected RFU corresponds to the original RFU.

The column labelled `Assay concentration (ng/uL)` represents a concentration relative to the standard-volume calibration. It should not be interpreted as the actual DNA concentration within the final assay well.

The `DNA concentration (ng/uL)` column represents the estimated DNA concentration in the original sample.

#### Export results

Click **Export results** to generate an Excel workbook.

The exported file is named:

```text
FluoR_results_YYYY-MM-DD.xlsx
```

The workbook contains four worksheets:

| Worksheet | Content |
|---|---|
| Results | Sample measurements and estimated DNA concentrations |
| Standards | Standard measurements and corrected fluorescence |
| Calibration | Reference concentrations and calibration fluorescence values |
| Model parameters | Assay settings and quantification model parameters |

Exported model information includes the selected assay, fluorescence correction method, standard and sample volumes, decay coefficient, reference order, median standard fluorescence values and calibration slope.

## Quantification methodology

### Overview

FluoR estimates DNA concentrations using fluorescence measurements and DNA standards of known concentration.

The analytical workflow consists of:

1. Optional fluorescence decay correction.
2. Two-point calibration.
3. Background subtraction.
4. DNA concentration estimation.
5. Quality control.

### DNA standards

The currently implemented assays use two reference standards:

| Assay | ST1 (ng/µL) | ST2 (ng/µL) |
|---|---:|---:|
| dsDNA High Sensitivity | 0 | 10 |
| dsDNA Broad Range | 0 | 100 |

ST1 represents the background fluorescence of the assay, whereas ST2 contains a known DNA concentration.

When fluorescence decay correction is enabled, replicate measurements of both standards are included at the beginning and end of the assay preparation sequence.

### Multiplicative fluorescence decay correction

Fluorescence intensity may change systematically during assay preparation.

FluoR models these changes using a multiplicative relationship between fluorescence intensity and pipetting order.

A log-linear regression is fitted to the standard measurements:

$$
\ln(F_i)=\alpha+\beta C_i+\gamma P_i+\varepsilon_i
$$

where:

- $F_i$ is the measured fluorescence intensity (RFU).
- $C_i$ is the known DNA concentration of the standard.
- $P_i$ is the pipetting order.
- $\alpha$ is the intercept.
- $\beta$ is the standard concentration coefficient.
- $\gamma$ is the coefficient associated with pipetting order.
- $\varepsilon_i$ is the residual error.

The corresponding multiplicative model on the original fluorescence scale is:

$$
F_i=e^{\alpha+\beta C_i+\gamma P_i+\varepsilon_i}
$$

The coefficient $\gamma$ represents the estimated logarithmic change in fluorescence per preparation step.

The relative fluorescence change per step, expressed as a percentage, is:

$$
\Delta F(\%)=100(e^\gamma-1)
$$

Negative values of $\gamma$ indicate decreasing fluorescence along the preparation sequence, whereas positive values indicate increasing fluorescence.

Fluorescence measurements are corrected relative to a reference preparation step:

$$
F_i^{corr}=F_i e^{-\gamma(P_i-P_{ref})}
$$

The reference step is currently fixed at:

$$
P_{ref}=1
$$

Therefore:

$$
F_i^{corr}=F_i e^{-\gamma(P_i-1)}
$$

The same correction factor is applied to standards and samples according to their respective pipetting order.

When correction is disabled:

$$
F_i^{corr}=F_i
$$

Only the initial standards are used for calibration in this case.

### Two-point calibration

Calibration is performed using the median fluorescence intensities of ST1 and ST2.

The background fluorescence is defined as:

$$
F_0=\operatorname{median}(F_{ST1}^{corr})
$$

The fluorescence intensity corresponding to the known DNA concentration is defined as:

$$
F_2=\operatorname{median}(F_{ST2}^{corr})
$$

The calibration slope is calculated as:

$$
m=\frac{F_2-F_0}{C_2}
$$

where $C_2$ is the original DNA concentration of ST2.

When fluorescence decay correction is enabled, initial and final corrected standard measurements are combined to calculate the corresponding medians.

A positive calibration slope is required for DNA quantification.

### Background subtraction

The median ST1 fluorescence is subtracted from the corrected sample fluorescence.

The current implementation applies a numerical lower bound of 1 RFU:

$$
F_i^{bg}=\max(F_i^{corr}-F_0,1)
$$

where $F_i^{bg}$ represents the background-corrected fluorescence.

This lower bound is a computational convention and does not represent an experimentally determined limit of detection or quantification.

Consequently, samples with fluorescence at or below the blank may receive small positive DNA concentration estimates. These observations are identified by the corresponding quality control flag.

### DNA concentration estimation

The standard-volume-equivalent DNA concentration is calculated as:

$$
C_i^{equiv}=\frac{F_i^{bg}}{m}
$$

The original sample DNA concentration is subsequently calculated by accounting for differences between the volumes of standards and samples added to the assay:

$$
C_i^{DNA}=C_i^{equiv}\frac{V_{std}}{V_{sample}}
$$

Combining both equations:

$$
C_i^{DNA}=\frac{F_i^{bg}}{m}\frac{V_{std}}{V_{sample}}
$$

where:

- $C_i^{DNA}$ is the estimated original sample DNA concentration (ng/µL).
- $V_{std}$ is the standard volume added to the assay (µL).
- $V_{sample}$ is the sample volume added to the assay (µL).
- $m$ is the calibration slope.

### DNA amount and concentration within the assay well

The estimated DNA amount present in a sample assay well is:

$$
M_i=C_i^{DNA}V_{sample}
$$

where $M_i$ is expressed in ng.

The DNA concentration within the final assay well is:

$$
C_i^{well}=\frac{M_i}{V_{well}}
$$

where $V_{well}$ is the total assay volume.

The corresponding DNA amount for ST2 is:

$$
M_{ST2}=C_2V_{std}
$$

Its concentration within the final assay well is:

$$
C_{ST2}^{well}=\frac{C_2V_{std}}{V_{well}}
$$

These quantities are used to visualise the DNA calibration in terms of both concentration and DNA amount.

### Quality control

Each sample measurement is assigned a quality control flag.

| QC flag | Interpretation |
|---|---|
| Within calibration interval | Corrected fluorescence exceeds ST1 and does not exceed ST2 |
| At or below blank | Corrected fluorescence is equal to or below the ST1 median |
| Above ST2 | Corrected fluorescence exceeds the ST2 median |
| Missing RFU | Original fluorescence is missing or non-finite |
| Non-positive RFU | Original fluorescence is zero or negative |
| Invalid corrected RFU | Corrected fluorescence is non-finite |

Measurements with missing, non-positive or invalid fluorescence values are assigned missing DNA concentration estimates.

Measurements at or below the blank and above ST2 are retained but flagged for further inspection.

Quality control flags should be interpreted together with the experimental conditions and assay performance.

### Model assumptions and limitations

The current quantification methodology assumes:

- A linear relationship between DNA amount and fluorescence within the calibration interval.
- A common multiplicative fluorescence change across standards and samples.
- A log-linear relationship between fluorescence and pipetting order when decay correction is enabled.
- Comparable experimental conditions across all analysed microplates.
- A common final reaction volume across standards and samples.
- Accurate DNA standard concentrations and pipetted volumes.
- Representative and reliable standard measurements.

Pipetting order is used as a proxy for assay preparation progression rather than as a direct measurement of elapsed time.

The multiplicative model does not independently account for differences in preparation intervals, plate reading times or plate-specific effects.

The two-point calibration does not independently establish the linearity of the fluorescence response.

DNA concentration estimates above ST2 represent extrapolations beyond the calibration interval.

The numerical background correction threshold and quality control flags are not equivalent to experimentally validated limits of detection or quantification.

Users should evaluate whether the assumptions of the model are appropriate for their specific assay and experimental design.

### Reproducibility

For reproducible analyses, users should retain:

- Original fluorescence measurement files.
- Standard and sample plate layouts.
- Experimental pipetting sequence.
- Exported FluoR results.
- Assay type.
- Standard, sample and final assay volumes.
- Fluorescence decay correction settings.
- FluoR version.

The mathematical implementation is available in [`R/quantification.R`](R/quantification.R).

## Example datasets

FluoR can be tested using Excel worksheets containing well identifiers and fluorescence measurements.

A minimal fluorescence measurement worksheet should contain a structure similar to the following:

| Well | RFU |
|---|---:|
| A1 | 1020 |
| B1 | 985 |
| C1 | 1015 |
| D1 | 990 |
| E1 | 25400 |
| F1 | 25100 |
| G1 | 24900 |
| H1 | 25300 |

The values above are illustrative synthetic measurements, not experimental data.

For a complete analysis, provide a standards worksheet and at least one sample worksheet.

When fluorescence decay correction is enabled, both initial and final standard measurements must be available.

Reusable standard and sample layouts can be generated directly from FluoR using the corresponding **Download layout** buttons.

## Development

### Repository structure

```text
FluoR/
├── app.R
├── build_installer.R
├── R/
│   ├── pipetting_order.R
│   ├── quantification.R
│   ├── samples_editor.R
│   └── standards_editor.R
├── icon/
│   └── fluor-icon.ico
├── README.md
├── LICENSE
├── CITATION.cff
└── THIRD_PARTY_NOTICES.md
```

### Application modules

| File | Purpose |
|---|---|
| `app.R` | Main Shiny application, data import, visualisation and results export |
| `R/standards_editor.R` | Standard layout editor and automatic standard detection |
| `R/samples_editor.R` | Sample plate layout editor |
| `R/pipetting_order.R` | Automatic and manual reconstruction of pipetting order |
| `R/quantification.R` | Fluorescence correction, calibration and DNA quantification |
| `build_installer.R` | Windows desktop installer generation |

### Building the Windows installer

FluoR uses the `shinyelectron` package to distribute the Shiny application as a standalone Windows desktop application.

The Windows installer includes a portable R runtime, R package dependencies and an Electron-based desktop interface.

To build the installer, install the application dependencies and the additional build packages:

```r
install.packages(c(
  "shinyelectron",
  "yaml",
  "jsonlite",
  "processx"
))
```

Set the working directory to the FluoR project root.

Ensure that the required application files and `icon/fluor-icon.ico` are present.

Run:

```r
source("build_installer.R")
```

The build process requires internet access to obtain the necessary runtimes and build dependencies when they are not already available locally.

The build script creates the distribution outside the FluoR source directory.

The generated installer is located at:

```text
FluoR-build/
└── electron-app/
    └── dist/
        └── FluoR-Setup-0.1.0.exe
```

The current build configuration targets 64-bit Windows.

The main components used for the initial Windows release are:

| Component | Version |
|---|---|
| FluoR | 0.1.0 |
| R | 4.6.1 |
| shinyelectron | 0.2.1 |
| Electron | 41.10.7 |
| electron-builder | 26.18.0 |

The build script does not currently lock every dependency to its original release version. Rebuilding the application at a later date may therefore use newer dependencies.

## Citation

If you use FluoR in your research, please cite the software in any resulting scientific publication and include the version used.

### Suggested citation

Sánchez-Gil, J. J. (2026). *FluoR: Fluorescence-based DNA quantification and microplate analysis* (Version 0.1.0) [Computer software]. GitHub. https://github.com/jjsanchezgil/FluoR

Citation metadata is available in [`CITATION.cff`](CITATION.cff).

Use the **Cite this repository** option on GitHub to obtain the citation in APA or BibTeX format.

When reporting analyses performed with FluoR, users are encouraged to specify:

- Software version.
- DNA quantification assay.
- Standard and sample volumes.
- Final assay volume.
- Whether fluorescence decay correction was applied.
- Pipetting order method, when applicable.

## License

The original FluoR source code is distributed under the [MIT License](LICENSE).

The standalone Windows distribution incorporates third-party software that remains subject to its respective licenses.

See [`THIRD_PARTY_NOTICES.md`](THIRD_PARTY_NOTICES.md) for information about third-party components, licenses and source materials.

Additional source archives are available alongside the relevant Windows installer on the [Releases page](https://github.com/jjsanchezgil/FluoR/releases).

## Feedback and contributions

Bug reports, feature requests, suggestions and contributions are welcome.

Please use the [GitHub Issues](https://github.com/jjsanchezgil/FluoR/issues) page to report problems or suggest improvements.

When reporting an issue, please include relevant information such as:

- FluoR version.
- Operating system.
- Selected assay.
- A description of the problem.
- Steps to reproduce the issue.
- Relevant error messages.
- Example input files, where possible and appropriate.

## Disclaimer

FluoR is research software intended for fluorescence-based DNA quantification and microplate analysis.

Users are responsible for evaluating the suitability of the implemented methods for their experimental conditions and validating the resulting DNA concentration estimates.

The software is provided without warranty, as described in the MIT License.

---

**FluoR — Fluorescence-based DNA quantification and microplate analysis**
