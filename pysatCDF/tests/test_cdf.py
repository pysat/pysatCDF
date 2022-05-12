import os

import pysatCDF


class TestBasics(object):

    def setup(self):
        """Run before every method to create a clean testing setup."""

    def teardown(self):
        """Run after every method to clean up previous testing."""

    def test_vefi_load(self):
        """Load VEFI file and perform basic data checks."""
        fname = os.path.join(pysatCDF.__path__[0], 'tests', 'test_data',
                             'cnofs_vefi_bfield_1sec_20080601_v05.cdf')

        with pysatCDF.CDF(fname) as cdf:
            data = cdf.data

        # Basic checks on data that was loaded
        assert data['B_flag'][0] == 0
        assert int(data['altitude'][0]) == 694
        assert data['year'][0] == 2008

        return

    def test_vefi_load_and_chameleon_data_access(self):
        """Load VEFI file and utilize spacepy like access."""
        fname = os.path.join(pysatCDF.__path__[0], 'tests', 'test_data',
                             'cnofs_vefi_bfield_1sec_20080601_v05.cdf')

        with pysatCDF.CDF(fname) as cdf:
            # Check on spacepy CDF attribute access mechanism
            assert (cdf['year'].attrs['FILLVAL'] == 65535)

            # Basic checks on spacepy CDF data access
            assert (cdf['B_flag'][...][0] == 0)
            assert (int(cdf['altitude'][...][0]) == 694)
            assert (cdf['year'][...][0] == 2008)
            assert (cdf['year'][0] == 2008)

            # Test repr
            test_str = repr(cdf['year'])
            assert test_str.find('CDF filename :') >= 0
            assert test_str.find('CDF variable name: year') >= 0

        return

    def test_vefi_load_to_pysat(self):
        """Load VEFI file and perform to_pysat()."""
        fname = os.path.join(pysatCDF.__path__[0], 'tests', 'test_data',
                             'cnofs_vefi_bfield_1sec_20080601_v05.cdf')

        with pysatCDF.CDF(fname) as cdf:
            data, meta = cdf.to_pysat()

        # Basic checks on data that was loaded
        assert data['B_flag'][0] == 0
        assert int(data['altitude'][0]) == 694
        assert data['year'][0] == 2008

        return
