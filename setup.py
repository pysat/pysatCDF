import setuptools
import numpy.distutils.core
import os
import sys

#f2py -c --include-paths $CDF_INC -I$CDF_INC $CDF_LIB/libcdf.a -m fortran_cdf fortran_cdf.f -lm -lc
base_cdf = '/Applications/cdf36_1-dist/'


names = { 'win32': ['libcdf.lib'],
            'darwin': ['libcdf.a'],
            'linux2': ['libcdf.so'],
            'linux': ['libcdf.so'],
            }
names = names.get(sys.platform, ['libcdf.so'])

# setup fortran 90 extension
#---------------------------------------------------------------------------  
ext1 = numpy.distutils.core.Extension(
    name = 'fortran_cdf',
    sources = ['pysatCDF/fortran_cdf.f'], 
    include_dirs = [base_cdf + 'include'],
    #library_dirs = [base_cdf + '/lib'],
    #libraries = names,
    f2py_options = ['--include-paths', base_cdf +'include'],
    extra_link_args = ['-lm', '-lc'],
    extra_objects = [base_cdf +'/lib/'+names[0] ],
    extra_f77_compile_args = ['-lm', '-lc'],

    )


# call setup
#--------------------------------------------------------------------------
numpy.distutils.core.setup( 

    name = 'pysatCDF',
    version = '0.1',        
    packages = ['pysatCDF'],
    #package_data = {'': ['*.so', '*.so.*/*','*.so.*/*/*'],},
    #include_package_data = True  ,
    ext_modules = [ext1, ]    

)  