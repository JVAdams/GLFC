GLFC
====

**GLFC** is a package of functions developed for the [Great Lakes Fishery Commission](http://www.glfc.org/)'s sea lamprey control program using the [R programming language](http://www.r-project.org/).

An example of how to use the functions in **GLFC** to estimate indices of adult sea lamprey abundance in the Great Lakes is given in this [vignette](https://rawgit.com/JVAdams/GLFC/master/vignettes/Adult-Index.html).

The package can be installed from within R.

	devtools::install_github("JVAdams/GLFC")
	library(GLFC)

You may need to download and install (as administrator, if using a PC) Rtools from [CRAN](http://cran.r-project.org/bin/windows/Rtools/), the Comprehensive R Archive Network, then run the following lines of code before submitting the code above:

	install.packages("devtools")
	devtools::find_rtools()

An alternative approach for Windows users is to download this 
[zip file](https://github.com/JVAdams/GLFC/raw/master/GLFC.zip)
and install the package from the R menu:
- Packages
- Install package(s) from local zip files...

- - -

_U.S. Geological Survey_ (USGS) Computer Program **GLFC** version 1.0.0.9000. 
Written by Jean V. Adams, [USGS - Great Lakes Science Center](http://www.glsc.usgs.gov/), Ann Arbor, Michigan, USA. 
Written in programming language R (R Core Team, 2016, www.R-project.org), version 3.3.0. 
Run on a PC with Intel(R) Core(TM) I7-4600m CPU, 2.90 GHz processor, 16.0 GB RAM, and Microsoft Windows 7 Enterprise operating system 2009 Service Pack 1. 
Source code is available from Jean V. Adams on [GitHub](https://github.com/JVAdams/GLFC), _jvadams (at) usgs (dot) gov_.

_Disclaimer:_ Although this program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the United States Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.
