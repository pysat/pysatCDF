
import os
import sys

from setuptools import setup

import numpy as np
import numpy.distutils
from numpy.distutils.command.build_ext import build_ext
from numpy.distutils.command.build_src import build_src
from numpy.distutils.command.build import build
import numpy.distutils.core

from subprocess import call

# Path to base CDF directory if CDF library already installed and you
# want to use it. Leave to None to install CDF library.
# System will look for installed package before installing a
# pysatCDF specific CDF install
base_cdf = None
build_cdf_flag = True

# Leave items below to None
# name of library, e.g. for mac os x, libcdf.a
lib_name = None

# Shared library name, needed for some systems (do not use on Mac OS X)
shared_lib_name = None
# CDF compile options
# Note that the shared library name will be appended to
# extra_link_args automatically.
extra_link_args = None
# OS and ENV comes from CDF installation instructions
os_name = None
env_name = None

# Manual f2py command for Mac OS X
# f2py -c --include-paths $CDF_INC -I$CDF_INC $CDF_LIB/libcdf.a -m
# fortran_cdf fortran_cdf.f -lm -lc

# Some solutions in creating this file come from
# https://github.com/Turbo87/py-xcsoar/blob/master/setup.py

# Get system parameters
platform = sys.platform
if platform == 'darwin':
    os_name = 'macosx'
    env_name = 'x86_64'
    lib_name = 'libcdf.a'

    # Including shared lib in mac breaks things, 'libcdf.dylib'
    shared_lib_name = None
    extra_link_args = ['-lm', '-lc']
elif (platform == 'linux') | (platform == 'linux2'):
    os_name = 'linux'
    env_name = 'gnu'
    lib_name = 'libcdf.a'

    # Disabling shared lib, but would be 'libcdf.so'
    shared_lib_name = None
    extra_link_args = ['-lm', '-lc']
elif (platform == 'win32'):
    os_name = 'mingw'
    env_name = 'gnu'
    lib_name = 'libcdf.a'
    shared_lib_name = None
    extra_link_args = []
else:
    check = ((os_name is None) or (env_name is None)
             or (extra_link_args is None))
    if (lib_name is None) or ((base_cdf is None) and check):
        estr = 'Unknown platform, please set setup.py parameters manually.'
        raise ValueError(estr)

BASEPATH = os.path.dirname(os.path.abspath(__file__))
CDF_PATH = os.path.join(BASEPATH, 'cdf36_3-dist')


class CDFBuild(build):
    def run(self):
        CDF_build(self, self.build_temp)
        build.run(self)
        return


def CDF_build(self, ppath):

    # Build CDF Library
    build_path = os.path.abspath(ppath)
    if platform == 'win32':
        # Replace backslashes with forward slashes to avoid path being
        # mangled by escape sequences
        build_path = build_path.replace('\\', '/')

    # Check if library already exists
    if (not os.path.isfile(os.path.join(self.build_lib,
                                        'pysatCDF',
                                        'lib',
                                        lib_name))) or self.force:
        cmd0 = ['make', 'clean']
        cmd = ['make',
               'OS=' + os_name,
               'ENV=' + env_name,
               'CURSES=no',
               'SHARED=yes',
               'UCOPTIONS=-Dsingle_underscore',
               'INSTALLDIR=' + build_path,  # -undefined -Wl,dynamic_lookup',
               'all', ]
        cmd2 = ['make',
                # 'INSTALLDIR='+build_path,
                'INSTALLDIR=' + build_path,
                'install', '>/dev/null 2>&1']

        # Clean any previous attempts
        def compile0():
            call(cmd0, cwd=CDF_PATH)

        # Set compile options via makefile
        def compile1():
            call(cmd, cwd=CDF_PATH)

        # Do the installation
        def compile2():
            call(cmd2, cwd=CDF_PATH)
        self.execute(compile0, [], 'Cleaning CDF')
        self.execute(compile1, [], 'Configuring CDF')
        self.execute(compile2, [], 'Compiling CDF')
        self.execute(compile0, [], 'Cleaning CDF')

        # Copy resulting tool to library build folder
        self.mkpath(os.path.join(ppath, 'pysatCDF'))

        if not self.dry_run:
            self.copy_tree(os.path.join(ppath, 'include'),
                           os.path.join(self.build_lib, 'pysatCDF', 'include'))
            self.mkpath(os.path.join(self.build_lib, 'pysatCDF', 'lib'))
            self.copy_file(os.path.join(ppath, 'lib', lib_name),
                           os.path.join(self.build_lib, 'pysatCDF', 'lib',
                                        lib_name))

            if shared_lib_name is not None:
                self.copy_file(os.path.join(ppath, 'lib', shared_lib_name),
                               os.path.join(self.build_lib, 'pysatCDF', 'lib',
                                            shared_lib_name))

    return


class ExtensionBuild(build_src):
    def run(self):

        CDF_build(self, self.build_src)
        lib_path = os.path.abspath(os.path.join(self.build_lib, 'pysatCDF'))

        # Set directories for the CDF library installed with pysatCDF
        self.extensions[0].include_dirs = [os.path.join(lib_path, 'include')]
        self.extensions[0].f2py_options = ['--include-paths',
                                           os.path.join(lib_path, 'include'),
                                           '--quiet']
        self.extensions[0].extra_objects = [os.path.join(lib_path, 'lib',
                                                         lib_name)]

        # Add shared library, if provided
        if shared_lib_name is not None:
            self.extensions[0].extra_link_args.append(
                os.path.join(lib_path, 'lib', shared_lib_name))

        build_src.run(self)
        return


# Almost to building
if not build_cdf_flag:
    print(' '.join(('Using CDF installation at', base_cdf)))
    f2py_cdf_include_path = os.path.join(base_cdf, 'include')
    f2py_cdf_lib_path = os.path.join(base_cdf, 'lib', lib_name)
    cmdclass = {}
else:
    print('Building CDF for pysatCDF.')
    cmdclass = {'build': CDFBuild,
                'build_src': ExtensionBuild, }
    f2py_cdf_include_path = ''
    f2py_cdf_lib_path = ''

# Setup fortran extension
# ---------------------------------------------------------------------------

ext1 = numpy.distutils.core.Extension(
    name='pysatCDF.fortran_cdf',
    sources=[os.path.join('pysatCDF', 'fortran_cdf.f')],
    include_dirs=[f2py_cdf_include_path],
    f2py_options=['--quiet', '--include-paths', f2py_cdf_include_path],
    extra_objects=[f2py_cdf_lib_path],
    extra_f77_compile_args=['--std=legacy'],
    extra_link_args=extra_link_args)


# Call setup
# --------------------------------------------------------------------------
numpy.distutils.core.setup(ext_modules=[ext1], cmdclass=cmdclass)
