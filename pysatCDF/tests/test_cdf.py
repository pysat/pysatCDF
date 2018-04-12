import numpy as np

from nose.tools import assert_raises, raises
import nose.tools

import pysatCDF

class TestBasics:
    def setup(self):
        """Runs before every method to create a clean testing setup."""
        # self.meta = pysat.Meta()
        # self.testInst = pysat.Instrument('pysat', 'testing', tag='', clean_level='clean')

    def teardown(self):
        """Runs after every method to clean up previous testing."""
        # del self.testInst

    def test_vefi_load(self):
        import os
        fname = os.path.join(pysatCDF.__path__[0],'tests', 'test_data', 'cnofs_vefi_bfield_1sec_20080601_v05.cdf')
        
        with pysatCDF.CDF(fname) as cdf:
            data = cdf.data
            
        check = []
        # basic checks on data that was loaded
        check.append(data['B_flag'][0] == 0)
        check.append(int(data['altitude'][0]) == 694)
        check.append(data['year'][0] == 2008)

        assert np.all(check)

    def test_vefi_load_and_chameleon_data_access(self):
        import os
        fname = os.path.join(pysatCDF.__path__[0],'tests', 'test_data', 'cnofs_vefi_bfield_1sec_20080601_v05.cdf')
        
        with pysatCDF.CDF(fname) as cdf:
            data = cdf.data
            # check on spacepy CDF attribute access mechanism
            assert (cdf['year'].attrs['FILLVAL'] == 65535)
            
            # basic checks on spacepy CDF data access
            assert (cdf['B_flag'][...][0] == 0)
            assert (int(cdf['altitude'][...][0]) == 694)
            assert (cdf['year'][...][0] == 2008)
        
    def test_vefi_pysat_load(self):
        import os
        import pysat
        
        fname = os.path.join(pysatCDF.__path__[0],'tests', 'test_data', 'cnofs_vefi_bfield_1sec_20080601_v05.cdf')
        pysat.utils.set_data_dir(os.path.join(pysatCDF.__path__[0],'tests', 'test_data'))
        vefi = pysat.Instrument(platform='cnofs', name='vefi',
                                tag='dc_b', manual_org=True)
        vefi.load(fname='cnofs_vefi_bfield_1sec_20080601_v05.cdf')
            
        check = []
        # basic checks on data that was loaded
        check.append(vefi[0, 'B_flag'] == 0)
        check.append(int(vefi[0, 'altitude']) == 694)
        check.append(vefi[0, 'year'] == 2008)

        assert np.all(check)
