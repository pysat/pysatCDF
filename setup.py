import setuptools
import numpy.distutils.core
import os

#f2py -c --include-paths $CDF_INC -I$CDF_INC $CDF_LIB/libcdf.a -m fortran_cdf fortran_cdf.f -lm -lc

# setup fortran 90 extension
#---------------------------------------------------------------------------  
ext1 = numpy.distutils.core.Extension(
    name = 'fortran_cdf',
    sources = ['pysatCDF/fortran_cdf.f'], 
    include_dirs = ['/Applications/cdf36_1-dist/include'],
    f2py_options = ['--include-paths', '/Applications/cdf36_1-dist/include'],
    extra_link_args = ['-lm', '-lc'],
    extra_objects = ['/Applications/cdf36_1-dist/lib/libcdf.a' ],
    extra_f77_compile_args = ['-lm', '-lc'],
    #depends = ['/Applications/cdf36_1-dist/include', '/Applications/cdf36_1-dist/include/cdf.inc']
    
    
    #depends = ['fortran_cdf.f'],

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