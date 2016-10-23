
import os
import sys

#import setuptools
from setuptools import setup
#from setuptools.command.install import install

import numpy as np
import numpy.distutils
import numpy.distutils.core
from numpy.distutils.command.build_ext import build_ext
from numpy.distutils.command.build_src import build_src
from numpy.distutils.command.build import build

from subprocess import call
#import setuptools


# path to base CDF directory if CDF library already installed and you want to use it
# leave to None to install CDF library
# system will look for installed packed before installing a pysatCDF specific CDF install
base_cdf = None

# leave items below to None unless base_cdf set to something other than None

# name of library, e.g. for mac os x, libcdf.a
lib_name = None
# shared library name, needed for some systems ( do not use Mac OS X)
shared_lib_name = None

# CDF compile options
# note that the shared library name will be appended to extra_link_args automatically
extra_link_args = None
# OS and ENV comes from CDF installation instructions
os_name = None
env_name = None

# manual f2py command for Mac OS X
# f2py -c --include-paths $CDF_INC -I$CDF_INC $CDF_LIB/libcdf.a -m fortran_cdf fortran_cdf.f -lm -lc

# some solutions in creating this file come from
# https://github.com/Turbo87/py-xcsoar/blob/master/setup.py

# code to find if a CDF library is already installed in default locations
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
    # grab all dirs, select those that start with cdf
    search_dir = []
    for default in defaults:
        sub_dirs = os.listdir(default)
        for sub in sub_dirs:
            #print(sub)
            if sub[0:3].lower() == 'cdf':
                search_dir.append(sub)
        search_dir = np.sort(search_dir)
        for test_dir in search_dir[::-1]:
            test_path = os.path.join(default, test_dir, 'lib',  lib_name)
            if os.path.exists(test_path):
                return os.path.join(default, test_dir)

    raise ValueError('Could not find CDF library, please set base directory in setup.py.')


# get system parameters
platform = sys.platform
if platform == 'darwin':
    os_name = 'macosx'
    env_name = 'x86_64'
    lib_name = 'libcdf.a'
    # including shared lib in mac breaks things
    shared_lib_name = None #'libcdf.dylib' #
    extra_link_args = ['-lm', '-lc']
elif (platform == 'linux') | (platform == 'linux2'):
    os_name = 'linux'
    env_name = 'gnu'
    lib_name = 'libcdf.a'
    shared_lib_name = 'libcdf.so'
    extra_link_args = ['-lm', '-lc']
elif (platform == 'win32'):
    lib_name = 'libcdf.lib'
    shared_lib_name = 'dllcdf.lib'
    extra_link_args = ['/nodefaultlib:libcd']
else:
    if (lib_name is None) or ((base_cdf is None) and ((os_name is None) 
                                or (env_name is None) or (extra_link_args is None)) ):
        raise ValueError('Unknown platform, please set setup.py parameters manually.')

# CDF directory hasn't been specified by user, see if an installation can be found
if base_cdf is None:
    try:
        base_cdf = find_CDF_base(lib_name)
        print(' '.join(('Found CDF installation at', base_cdf)))
    except ValueError:
        print( 'Unable to find CDF installation in default location.')
        base_cdf = None


if base_cdf is None:
    build_cdf_flag = True
    # library not provided, build library included with pysatCDF
else:
    build_cdf_flag = False
    #raise NotImplementedError
    if not os.path.isdir(base_cdf):
        raise ValueError('CDF directory supplied is not an actual directory.')
    if (lib_name is None):
        raise ValueError('Attempting to use pre-installed CDF library as directed. Please set setup.py parameters manually.')    
        

BASEPATH = os.path.dirname(os.path.abspath(__file__))
CDF_PATH = os.path.join(BASEPATH, 'cdf36_1-dist')

class CDFBuild(build):
    def run(self):
        CDF_build(self, self.build_temp)
        build.run(self)
        return
        
def CDF_build(self, ppath):
    
    # build CDF Library
    build_path = os.path.abspath(ppath)
    
    # check if library already exists
    if (not os.path.isfile(os.path.join(self.build_lib, 'pysatCDF', 'lib', lib_name))) or self.force:
        cmd0 = ['make', 'clean']
        cmd = ['make',
            'OS=' + os_name,
            'ENV=' + env_name,
            'CURSES=no',
            'UCOPTIONS=-Dsingle_underscore',
            'all',]
        cmd2 = ['make',
            'INSTALLDIR='+build_path,
            'install',]
    
        # clean any previous attempts
        def compile0():
            call(cmd0, cwd=CDF_PATH)
        # set compile options via makefile
        def compile1():
            call(cmd, cwd=CDF_PATH)
        # do the installation
        def compile2():
            call(cmd2, cwd=CDF_PATH)
    
        self.execute(compile0, [], 'Cleaning CDF')
        self.execute(compile1, [], 'Configuring CDF')
        self.execute(compile2, [], 'Compiling CDF')
        self.execute(compile0, [], 'Cleaning CDF')
    
        # copy resulting tool to library build folder
        self.mkpath(os.path.join(ppath, 'pysatCDF'))
    
        if not self.dry_run:
            self.copy_tree(os.path.join(ppath, 'include'), 
                            os.path.join(self.build_lib, 'pysatCDF', 'include'))
            self.mkpath(os.path.join(self.build_lib, 'pysatCDF', 'lib'))
            self.copy_file(os.path.join(ppath, 'lib', lib_name), 
                            os.path.join(self.build_lib, 'pysatCDF', 'lib', lib_name))
    
            if shared_lib_name is not None:
                self.copy_file(os.path.join(ppath, 'lib', shared_lib_name), 
                                os.path.join(self.build_lib, 'pysatCDF', 'lib', shared_lib_name))
    
    # run original build code
    #build.run(self)
    return


class ExtensionBuild(build_src):
    def run(self):

        CDF_build(self, self.build_src)
        #print 'yo yo yo'
        #print (self.__dict__)
        lib_path = os.path.abspath(os.path.join(self.build_lib, 'pysatCDF'))
        # set directories for the CDF library installed with pysatCDF
        self.extensions[0].include_dirs = [os.path.join(lib_path, 'include')]
        self.extensions[0].f2py_options = ['--include-paths', os.path.join(lib_path, 'include')]
        self.extensions[0].extra_objects = [os.path.join(lib_path, 'lib', lib_name)]
        # add shared library, if provided
        if shared_lib_name is not None:
            self.extensions[0].extra_link_args.append(os.path.join(lib_path, 'lib', shared_lib_name))

        build_src.run(self)
        return

# almost to building
if not build_cdf_flag:
    print (' '.join(('Using CDF installation at', base_cdf)))
    f2py_cdf_include_path = os.path.join(base_cdf, 'include')
    f2py_cdf_lib_path = os.path.join(base_cdf, 'lib', lib_name)
    cmdclass = {}
else:
    print ('Building CDF for pysatCDF.')
    cmdclass={'build': CDFBuild,
              'build_src': ExtensionBuild,
              }
    f2py_cdf_include_path = ''
    f2py_cdf_lib_path = ''

# setup fortran extension
#---------------------------------------------------------------------------  

ext1 = numpy.distutils.core.Extension(
        name = 'pysatCDF.fortran_cdf',
        sources = [os.path.join('pysatCDF', 'fortran_cdf.f')], 
        include_dirs = [f2py_cdf_include_path],
        f2py_options = ['--include-paths', f2py_cdf_include_path],
        extra_objects = [f2py_cdf_lib_path],
        extra_link_args = extra_link_args,  
        )


# call setup
#--------------------------------------------------------------------------
numpy.distutils.core.setup( 

    name = 'pysatCDF',
    version = '0.2.1.4',        
    packages = ['pysatCDF'],
    cmdclass = cmdclass,
    ext_modules = [ext1, ],
    description= 'Simple NASA Common Data Format (CDF) File reader.',
    long_description = 'pysatCDF is a reader for CDF files and provides '+
                       'additional support for exporting to pysat data formats (not required). '+
                       'The NASA CDF library is included.',
    url='http://github.com/rstoneback/pysatCDF',    
    # Author details
    author='Russell Stoneback',
    author_email='rstoneba@utdallas.edu',
    #data_files = [('', ['cdf36_1-dist/CDF_copyright.txt'])],
    
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
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3.4',
    ],
    #install_requires = ['numpy'],
)  





