# pysatCDF
Python reader for NASA CDF file format

Uses standard and extended Fortran CDF interfaces to load Common Data Format (CDF) files into Python.

# Example
```
import pysatCDF
cdf = pysatCDF.CDF(filename)

# All variable data loaded into dictionary in .data
cdf.data
# Attributes dictionary
cdf.meta

# Specific access by variable
data = cdf[var]
data = cdf.data[var]
info = cdf.meta[var]

# Export data to pysat data and metadata format
data, meta = cdf.to_pysat()
```

# Installation
Actual CDF loading is performed by the [NASA CDF libraries] (http://cdf.gsfc.nasa.gov/html/sw_and_docs.html) 
and must be installed before pysatCDF is installed.

To install pysatCDF
```
git clone https://github.com/rstoneback/pysatCDF.git
cd pysatCDF
python setup.py build install
```
Note this is a young package and the setup.py is currently configured for Mac OS X. The CDF library has different names depending upon platform. Currently this must be set by hand in setup.py for other platforms. If installation fails because of an incorrect name, missing CDF library, or similar issue, after fixing the issue remove the build directory initially created in pysatCDF to force the system to re-compile.

# Motivation
Provide simple, robust access to CDF data in Python and simplify adding instruments to [pysat](https://github.com/rstoneback/pysat).
