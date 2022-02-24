"""pysatCDF is a simple reader for NASA's Common Data Format (CDF) files.

pysatCDF uses NASA's C library to do the actual loading and couples
Python to this library via an intermediate Fortran layer.

"""
# set version
here = os.path.abspath(os.path.dirname(__file__))
with open(os.path.join(here, 'version.txt')) as version_file:
    __version__ = version_file.read().strip()

from ._cdf import CDF as CDF

del here
