
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DDcentutils

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

DDcentutils is a package created by the Ecosystem Modeling and Data
Consortium (EMDC) Team at Colorado State University (CSU) to facilitate
the use of the DayCent® model and the visualization of DayCent model run
results.

We created a Discussions forum on GitHub to facilitate Q&A about the
package and suggest ideas for further development:
<https://github.com/CSU-Soil-Carbon-Solution-Center/DDcentutils/discussions>.

You can also report bugs by creating an issue on the repository. For
more about creating an issue, please visit:
<https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/creating-an-issue>.

Please note that the package does not include access to the DayCent
model and its associated library files. DayCent is a CSU licensed
trademark and subject to the terms of use of the license agreement.
Users affiliated with a research institution can request access to the
model under a non-exclusive, non-commercial license on
<https://www.soilcarbonsolutionscenter.com/daycent>. Requests are
evaluated on a case-by-case basis and require acknowledging the
license’s terms of use and sharing basic information about the research
project. For more information about model access, please visit
<https://www.soilcarbonsolutionscenter.com/consortium>.

## Installation

You can install the development version of DDcentutils from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("CSU-Soil-Carbon-Solution-Center/DDcentutils")
```

## Directory structure

This package enforces a directory structure to establish robust file
organization practices. The DayCent environment consists of executables
and input and output files. Inputs include the .100, .in, and .sch
files. To better describe how this package deals with inputs and
outputs, we segment the files into library, parameter, data, and output
file categories, as depicted in the figure below.

<figure>
<img src="man/figures/daycent_environment.png"
alt="Figure 1. The DayCent environment." />
<figcaption aria-hidden="true">Figure 1. The DayCent
environment.</figcaption>
</figure>

The proposed categories can be described as:

- Executables: include the DayCent model and the List100 program to
  extract outputs from the binary files.
- Library files: include the library of parameters for events specified
  in the schedule file and are shared across all sites.
- Parameter files: describe user-defined site-specific parameters (obs:
  .dat and C14DATA are optional files for a DayCent run).
- Data files: describe user-defined soils and weather data for the site.
- Output files: outputs from a DayCent run.

Good file organization helps avoid duplicate files and prevents
accidental overwriting by keeping outputs clearly named and separated.
The current directory structure in this package is defined as:

<pre>        
{project}/                                      ## user defined name for the project
├── executables/                                ## stores the executables
├── 100libraryfiles/                            ## stores the library files
└── sites/
    └── {site_A}/                               ## user defined name for the site; stores parameter and data files
        ├── {site}.100                          ## user defined name for the site.100 file
        ├── {site}_{run}.sch                    ## user defined name for the site and DayCent run in the schedule file name
        ├── {site}.wth                          ## user defined name for the site weather file
        └── outputs/                            ## stores output files; outputs are organized in separate folders per run
            └── eq
            └── base
            └── {scen}                          ## can be more than one folder per scenario run
                └── {site}_{scen}_harvest.csv   ## output name defined based on site schedule file name
<pre>

The names of the project and site folders are user defined. More than
one site folder within *sites/* is allowed. The input files must follow
the pattern identified in the directory tree above, with name convention
for sites and scenarios defined by the user. Outputs are organized per
DayCent block run (equilibrium, baseline, scenario) within the
*outputs/* folder. More than one scenario folder within *outputs/* is
allowed.

Let’s use one of the most famous long-term Soil Organic Carbon (SOC)
research sites, located in Wooster, Ohio, to understand how the
directory structure works. Since 1962, this site has hosted comparison
trials of no-till and conventional tillage for corn and soybeans
(<https://soilfertility.osu.edu/research/long-term-tillage-plots>). In
this example, this is how the directory tree and file names could look
like:

<pre>            
{project}/
├── executables/
├── 100libraryfiles/
└── sites/
    └── <strong>Wooster</strong>/
        ├── <strong>wooster_site</strong>.100
        ├── <strong>wooster</strong>_<strong>cc_nt</strong>.sch
        ├── <strong>wooster</strong>_<strong>cc_ct</strong>.sch
        ├──  <strong>wooster</strong>.wth
        └── outputs/
            └── eq
            └── base
                └── <strong>base</strong>_harvest.csv
            └── <strong>cc_nt</strong>
                └── <strong>wooster</strong>_<strong>cc_nt</strong>_harvest.csv
            └── <strong>cc_ct</strong>
                └── <strong>wooster</strong>_<strong>cc_ct</strong>_harvest.csv
<pre>   

## How to use this package

This package is being developed to facilitate the use of the DayCent
model in three main aspects:

1.  Input file building and management
2.  Running DayCent
3.  Output visualization

### 1. Input file building and management

#### Setting up a new site

Asuming we are taking a global calibration approach to the fix.100
parameters and reusing crop and event .100 parameters, there are three
primary tasks when building a new site:

1)  Updating the site.100 file to represent the new location.

- This includes updating the latitude, longitude, cloud percentages, and
  monthly weather statistics.

2)  Define the soil layer parameters.

- For the equilibrium run, the package uses soil.in and site.par files
  to pass the parameterization.

3)  Build a weather file in the correct format from either station data
    or a gridded reanalysis product.

For any DayCent simulation, it is strongly recommended to start from an
existing site file set as a template and working on one file at a time.

#### Defining basic {site}.100 parameters

We need to change a series of parameters is the site.100, sitepar.in and
soils.in files.

In the {site}.100 file:

1)  First we can use the internal calculation of the program to find the
    weather statistics by changing “\*\*\* Climate parameters” line to
    “\*\*\* Climate statistics {site}.wth”.

    - This will use the weather file to recalculate the statistics
      rather than recalculating and replacing. These statistics are
      saved if an extended site.100 file is written.

2)  We need to change the latitude (SITLAT) and longitude (SITLNG)
    parameters.

    - The SITELNG parameter is not really used in the calculation, but
      the SITLAT (+/- 90 degrees) is critically important for estimating
      day length and and solar radiation if not provided in the weather
      file.

For this, we use the following function:

- **update_daycent_site_file**: Reads the file and replaces specific
  parameter values. The function description includes the supported
  parameter groups for update. The updated file is written to a new
  location.

#### Updating site parameters in the sitepar.in file

We also need to update some additional some parameters stored in the
sitepar.in file. Estimations of soil radiation and potential
evapotranspiration depend on the latitude but also the elevation and
cloud impacts on solar radiation. Several other parameters could be
changed (aspect, slope, etc…). These often have a small sensitivity to
SOC change, but novel sites may require careful evaluation of
controlling mechanisms. We can always change the site par.in file
manually or we can pull these data from other data sets and update them
directly.

This function uses R packages that pull data for sites from existing
APIs:

- **adjust_sitepar_srad_elev**: Retrieves and updates the solar
  radiation adjustment for cloud cover and transmission coefficient
  using the NASA POWER global data, and retrieves and updates the
  elevation, slope, and aspect using Mapzen terrain tiles.

#### Build Soil and Weather Data

With {site}.100 and sitepar files updated, we need to build the soil and
weather files for the site. As each simulation represents a point in
space, this point can be abstracted to a whole ecosystem or can
represent on soil core. It is easiest to treat each simulation as an
exact point.

We can provide direct observational data for soil and weather at that
point or, as more commonly done for large simulations, we can use
continuous modeled data from soil surveys, like SSURGO in the USA, or
gridded reanalysis weather products, such as Daymet or NASA POWER.

We have written functions to retrieve soils and weather data from
commonly used sources, such as SSURGO and Daymet, and have successfully
collaborated with community members to develop other functions for the
same purposes.

This package offers two options to update the *.wth* file and the output
for both will include the weather variables necessary for a DayCent run:
day of the month, month, maximum and minimum temperature (in degrees
Celsius), daily precipitation (cm), and shortwave radiation (W/m2).

- **getDaymetData**: retrieves the Daymet weather data of a specific
  site in **North America** based on its latitude and longitude using
  the *daymetr* package, including additional rows for day 366 in leap
  years.

- **getNASAPowerData**: retrieves the NASA POWER global weather data of
  a specific site based on its latitude and longitude using the
  *nasapower* package.

There are also two options to update the *soils.in* file and the output
for both is interpolated to fit the soil depth intervals for DayCent
when necessary.

- **convert_ssurgo_to_daycent**: converts SSURGO data into a
  DayCent-ready file format for specific locations. Data availability is
  restricted to the United States.

- **convert_soilgrids_to_daycent**: converts SoilGrids data into a
  DayCent-ready file format for specific locations. SoilGrids provides
  global data.

We welcome contributions of additional functions from other data
products and cross-valuation of data products!

#### Updating an existing schedule file

This package includes a simple function, **update_sch**, that reads an
existing DayCent schedule file ({site}\_{run}.sch) and replaces one or
more parameters (e.g., “Site file name”, “Starting year”) with new
values. This function will not write repeating parameters like schedule
events. It writes the modified file to a new output path.

### 2. Running DayCent
