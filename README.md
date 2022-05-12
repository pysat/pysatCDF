<div align="center">
        <img height="0" width="0px">
        <img width="20%" src="https://raw.githubusercontent.com/pysat/pysatCDF/main/docs/images/logo.png" alt="pysatCDF" title="pysatCDF"</img>
</div>

# pysatCDF
[![PyPI Package latest release](https://img.shields.io/pypi/v/pysatcdf.svg)](https://pypi.python.org/pypi/pysatcdf)
[![Build Status](https://github.com/pysat/pysatCDF/actions/workflows/main.yml/badge.svg)](https://github.com/pysat/pysatCDF/actions/workflows/main.yml/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/pysat/pysatCDF/badge.svg?branch=main)](https://coveralls.io/github/pysat/pysatCDF?branch=main)
[![DOI](https://zenodo.org/badge/51764432.svg)](https://zenodo.org/badge/latestdoi/51764432)

Self-contained Python reader for NASA CDF file format

Uses standard and extended Fortran CDF interfaces to load Common Data Format (CDF) files into Python.

# Example
```
import pysatCDF
with pysatCDF.CDF(filename) as cdf:

    # All variable data loaded into dictionary in .data
    cdf.data
    data = cdf.data[name]
    # Attributes dictionary
    cdf.meta
    attribute = cdf.meta[name][attr_name]

    # CDF variable information available by name
    cdf[name]

    # Data access similar to other packages
    data = cdf[name][...]
    attribute = cdf[name].attrs[attr_name]

    # Export data to pysat data and metadata format
    data, meta = cdf.to_pysat()
```

# Testing
pysatCDF has been tested on Mac OS X and Ubuntu 15.04. Support is included 
for building on windows via Windows Subsystem for Linux. 

# Motivation
Provide simple, robust access to CDF data in Python and simplify 
adding instruments to [pysat](https://github.com/pysat/pysat).

# Installation in POSIX compatible environments
Actual CDF loading is performed by the [NASA CDF libraries] (http://cdf.gsfc.nasa.gov/html/sw_and_docs.html) 
which are included with pysatCDF.

To install pysatCDF
```
git clone https://github.com/rstoneback/pysatCDF.git
cd pysatCDF
python setup.py install
```

# Installing pysatCDF in MacOS

MacOS does not ship with a Fortran compiler. One method for getting a suitable
build environment is to use brew.
```
brew install gcc
```

and then install pysatCDF as normal.

# Installing pysatCDF in Windows

Install the Windows Subsytem for Linux and proceed as per POSIX installation.

