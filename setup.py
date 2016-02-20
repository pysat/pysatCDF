import setuptools
import numpy.distutils.core
import os
import sys
import numpy as np

# path to base CDF directory
# set manually, if needed
# ex: '/Applications/cdf36_1-dist'
base_cdf = None

# name of library
# mac os x, libcdf.a
# leave to None to let program try for itself,
# or set by hand
lib_name = None

# manual f2py command for Mac OS X
# f2py -c --include-paths $CDF_INC -I$CDF_INC $CDF_LIB/libcdf.a -m fortran_cdf fortran_cdf.f -lm -lc


# need to find the CDF library
def find_CDF_base(lib_name):
    
    # try environment variables
    cdf_base = os.getenv('CDF_BASE')
    if cdf_base is not None:
        # check
        if os.path.exists(cdf_base):
            return cdf_base
            
    # that didn't work
    # look in default locations
    platform = sys.platform
    user_dir = os.path.expanduser('~')
    if platform == 'darwin':
        defaults = ['/Applications/', user_dir]
    elif (platform == 'linux') | (platform == 'linux2'):
        defaults = ['/usr/local/', user_dir]
    elif platform == 'win32':
        defaults = ['c:\\CDF Distribution\\']
    else:
        raise ValueError('Unknown platform, set CDF library in setup.py.')
        
    # collect a list of posible directories to search in
    # grab all dirs, select those that start with .cdf
    search_dir = []
    for default in defaults:
        sub_dirs = os.listdir(default)
        for sub in sub_dirs:
            if sub[0:3].lower == 'cdf':
                search_dir.append(sub)
        search_dir = np.sort(search_dir)
        for test_dir in search_dir[::-1]:
            test_path = os.path.join(default, test_dir, 'lib',  lib_name)
            if os.path.exists(test_path):
                return os.path.join(default, test_dir)

    raise ValueError('Could not find CDF library, please set base directory in setup.py.')
    


platform = sys.platform
if lib_name is None:
    if platform == 'darwin':
        lib_name = 'libcdf.a'
    elif (platform == 'linux') | (platform == 'linux2'):
        lib_name = 'libcdf.so'
    elif (platform == 'win32'):
        lib_name = 'libcdf.lib'
    else:
        raise ValueError('Unknown platform, please set library name manually.')    

if base_cdf is None:
    # try and find base directory
    base_cdf = find_CDF_base(lib_name)
    
cdf_lib_dir = os.path.join(base_cdf, 'lib')
cdf_include_dir = os.path.join(base_cdf, 'include')

# setup fortran extension
#---------------------------------------------------------------------------  
ext1 = numpy.distutils.core.Extension(
    name = 'fortran_cdf',
    sources = [os.path.join('pysatCDF', 'fortran_cdf.f')], 
    include_dirs = [cdf_include_dir],
    f2py_options = ['--include-paths', cdf_include_dir],
    extra_link_args = ['-lm', '-lc'],
    extra_objects = [os.path.join(cdf_lib_dir, lib_name) ],
    extra_f77_compile_args = ['-lm', '-lc'],

    )


# call setup
#--------------------------------------------------------------------------
numpy.distutils.core.setup( 

    name = 'pysatCDF',
    version = '0.1',        
    packages = ['pysatCDF'],
    ext_modules = [ext1, ]    

)  