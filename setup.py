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
            print(sub)
            if sub[0:3].lower() == 'cdf':
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
        lib_name = 'libcdf.a'
    elif (platform == 'win32'):
        lib_name = 'libcdf.lib'
    else:
        raise ValueError('Unknown platform, please set library name manually.')    

if base_cdf is None:
    # try and find base directory
    base_cdf = find_CDF_base(lib_name)
    
cdf_lib_dir = os.path.join(base_cdf, 'lib')
cdf_include_dir = os.path.join(base_cdf, 'include')

cdf_lib_path = os.path.join(cdf_lib_dir, lib_name)

if platform == 'darwin':
    extra_link_args = ['-lm', '-lc']
elif (platform == 'linux') | (platform == 'linux2'):
    extra_link_args = [os.path.join(cdf_lib_dir,'libcdf.so'), '-lm', '-lc']
elif (platform == 'win32'):
    extra_link_args = [os.path.join(cdf_lib_dir,'dllcdf.lib'), '/nodefaultlib:libcd']
else:
    raise ValueError('Unknown platform, please set library name manually.')    


# setup fortran extension
#---------------------------------------------------------------------------  
ext1 = numpy.distutils.core.Extension(
    name = 'fortran_cdf',
    sources = [os.path.join('pysatCDF', 'fortran_cdf.f')], 
    include_dirs = [cdf_include_dir],
    #library_dirs = [cdf_lib_dir],
    f2py_options = ['--include-paths', cdf_include_dir],
    extra_link_args = extra_link_args,
    extra_objects = [cdf_lib_path],
    #extra_f77_compile_args = [cdf_lib_path, '-fsecond-underscore', '-Wtabs'],
    #extra_compile_args = ['-fsecond-underscore'],
    )


# call setup
#--------------------------------------------------------------------------
numpy.distutils.core.setup( 

    name = 'pysatCDF',
    version = '0.1.1',        
    packages = ['pysatCDF'],
    ext_modules = [ext1, ],
    description= 'Simple Common Data Format (CDF) File reader.',
    url='http://github.com/rstoneback/pysatCDF',    
    # Author details
    author='Russell Stoneback',
    author_email='rstoneba@utdallas.edu',

    # Choose your license
    license='BSD',
    # See https://pypi.python.org/pypi?%3Aaction=list_classifiers
    classifiers=[
        # How mature is this project? Common values are
        #   3 - Alpha
        #   4 - Beta
        #   5 - Production/Stable
        'Development Status :: 3 - Alpha',

        # Indicate who your project is intended for
        'Intended Audience :: Science/Research',
        'Topic :: Scientific/Engineering :: Astronomy',
        'Topic :: Scientific/Engineering :: Physics',
        'Topic :: Scientific/Engineering :: Atmospheric Science',
        # Pick your license as you wish (should match "license" above)
        'License :: OSI Approved :: BSD License',

        # Specify the Python versions you support here. In particular, ensure
        # that you indicate whether you support Python 2, Python 3 or both.
        #'Programming Language :: Python :: 2',
        #'Programming Language :: Python :: 2.6',
        'Programming Language :: Python :: 2.7',
        #'Programming Language :: Python :: 3',
        #'Programming Language :: Python :: 3.2',
        #'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
    ],

    #install_requires = ['numpy'],
)  



