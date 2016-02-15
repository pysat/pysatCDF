import setuptools
import numpy.distutils.core
import os

# setup fortran 90 extension
#---------------------------------------------------------------------------  
ext1 = numpy.distutils.core.Extension(
    name = 'fortran_cdf',
    sources = ['pysatCDF/fortran_cdf.f'], 
               #'/Applications/cdf36_1-dist/include/cdf.inc'],
    library_dirs = ['/Applications/cdf36_1-dist/lib','/Applications/cdf36_1-dist/include'],
    libraries = ['libcdf.a', 'cdf.inc'],
    #runtime_library_dirs = ['/Applications/cdf36_1-dist/lib','/Applications/cdf36_1-dist/include'],
    include_dirs = ['/Applications/cdf36_1-dist/include','/Applications/cdf36_1-dist/lib'],
    f2py_options = ['--include-paths', '/Applications/cdf36_1-dist/include',
    '-I/Applications/cdf36_1-dist/include', '-lc', '-lm'], 
                    #'-I', '/Applications/cdf36_1-dist/include',
                    #'/Applications/cdf36_1-dist/lib/libcdf.a', '-l','m', '-l','c'],
    extra_link_args = ['-lm', '-lc'],
    extra_objects = ['/Applications/cdf36_1-dist/include/cdf.inc','/Applications/cdf36_1-dist/lib/libcdf.a' ],
    extra_f77_compile_args = ['-I/Applications/cdf36_1-dist/include','-lm', '-lc'],
    depends = ['/Applications/cdf36_1-dist/include', '/Applications/cdf36_1-dist/include/cdf.inc']
    
    
    #depends = ['fortran_cdf.f'],

    )


# call setup
#--------------------------------------------------------------------------
numpy.distutils.core.setup( 

    name = 'pysatCDF',
    version = '0.1',        
    packages = ['pysatCDF'],
    #package_data = {'': ['*.so', '*.so.*/*'],},
    #include_package_data = True  ,
    #ext_modules = [ext1, ]    

)  