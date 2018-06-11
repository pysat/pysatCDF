# pysatCDF
[![Build Status](https://travis-ci.org/rstoneback/pysatCDF.svg?branch=master)](https://travis-ci.org/rstoneback/pysatCDF)
[![Coverage Status](https://coveralls.io/repos/github/rstoneback/pysatCDF/badge.svg?branch=master)](https://coveralls.io/github/rstoneback/pysatCDF?branch=master)
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
pysatCDF has been tested on Mac OS X and Ubuntu 15.04. Support is included for building on windows if the mingw environment is present. 

# Motivation
Provide simple, robust access to CDF data in Python and simplify adding instruments to [pysat](https://github.com/rstoneback/pysat).

# Installation in POSIX compatible environments
Actual CDF loading is performed by the [NASA CDF libraries] (http://cdf.gsfc.nasa.gov/html/sw_and_docs.html) 
which are included with pysatCDF.

To install pysatCDF
```
git clone https://github.com/rstoneback/pysatCDF.git
cd pysatCDF
python setup.py install
```

# Installing pysatCDF in Windows

Python environment: Python 2.7.x
To compile pysatCDF in Windows, you need a POSIX compatible C/ Fortran compiling environment. Follow the below instructions to achieve this.

1. Install MSYS2 from http://repo.msys2.org. The distrib folder contains msys2-x86_64-latest.exe (64-bit version) to install MSYS2. 
2. Assuming you installed it in its default location C:\msys64, launch MSYS2 environment from C:\msys64\msys2.exe. This launches a shell session.
3. Now you need to make sure everything is up to date.  This terminal command will run updates
		pacman -Syuu
4. After running this command, you will be asked to close the terminal window using close button and not exit() command. Go ahead and do that.
5. Relaunch and run 'pacman -Syuu' again.
6. After the second run, you should be up to date. If you run the update command again, you will be informed that there was nothing more to update. Now you need to install build tools and your compiler toolchains.
		pacman -S base-devel git mingw-w64-x86_64-toolchain
If it prompts you to make a selection and says (default:all), just press enter.  This install may take a bit.
7. Now you need to set up your MSYS2 environment to use whatever python interpreter you want to build pysatCDF for. In my case the path was C:\Python27_64, but yours will be wherever python.exe exists.
8. Update MSYS2 path to include the folders with python binary and Scripts. To do that, navigate to your home directory in MSYS2. Mine is C:\msys64\home\gayui.
8. Edit the .bash_profile file to add the below lines somewhere in the file.
		# Add System python
		export PATH=$PATH:/c/Python27_64:/c/Python27_64/Scripts
Note the unix-style paths. So C: becomes /c/. If your python was in C:\foo\bar\python you would put /c/foo/bar/python and /c/foo/bar/python/Scripts
9. Next step is to add the mingw64 bin folder to your windows system path. Right-click on computer, hit properties. Then click advanced system settings, then environment variables. Find the system variable (as opposed to user variables) named PATH. This is a semicolon delimited list of the OS search paths for binaries. Add another semicolon and the path C:\msys64\mingw64\bin
10. Now you should have access to Python from within your MSYS2 environment. And your windows path should have access to the mingw binaries. To verify this, launch the mingw64 MSYS2 environment.
		C:\msys64\mingw64.exe
Run the command
		which python
and confirm that it points to the correct python version you want to be using.
11. Microsoft Visual C++ 9.0 is required to compile C sources. Download and install the right version of Microsoft Visual C++ for Python 2.7 from - 
		http://aka.ms/vcpython27
12. We are now getting close to installing pysatCDF. Do the following in the shell environment that is already opened.
		mkdir src
		cd src
		git clone https://github.com/rstoneback/pysatCDF.git
		cd pysatCDF
13. Using a text editor of your choice, create a file called setup.cfg in
		C:\msys64\home\gayui\src\pysatCDF (note: gayui will be replaced with your username)
Put the following in the file before saving and closing it.
		[build]
		compiler=mingw32
14. In your MSYS2 MINGW64 environment, run
		python setup.py install
This should compile and install the package to your site-packages for the python you are using.
15. You should now be able to import pysatCDF in your Python environment. If you get an ImportError, restart Python and import again.
