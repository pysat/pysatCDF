from __future__ import print_function
from __future__ import absolute_import
import copy
import numpy as np
import string
import sys

import pandas
import pysat

from pysatCDF import fortran_cdf


class CDF(object):
    """Reads data from NASA Common Data Format (CDF) files.

    pysatCDF presents a Python interface to NASA CDF files.
    To provide an easy installation experience the CDF library
    is included with the software and should be built
    automatically when pysatCDF is installed. In addition
    to zVariable support in CDFs, pysatCDF provides
    functionality to load CDF data and export it into a
    format for pysat integration.

    pysatCDF provides Fortran calls to the simplest CDF fortran
    interface, which is itself mapped from C
    code. The pysatCDF Fortran is wrapped up by f2py for Python and
    is used by the high level python in pysatCDF.
    The routines have been observed to be stable over many
    data loads.

    Note when opening a CDF file with this module all data is
    automatically loaded from disk unless specific variables
    are excluded upon instantiation.

    """

    def __init__(self, fname):
        # In CDF docs it says don't include .cdf in name
        name = fname
        if fname[-4:].lower() == '.cdf':
            name = fname[:-4]

        self.fname = name
        status = fortran_cdf.open(name)

        self.data_loaded = False

        # CDF library numeric codes for data types.
        cdty = {}
        cdty['real4'] = 21
        cdty['float'] = 44
        cdty['real8'] = 22
        cdty['double'] = 45

        cdty['byte'] = 41
        cdty['int1'] = 1
        cdty['int2'] = 2
        cdty['int4'] = 4
        cdty['uint1'] = 11
        cdty['uint2'] = 12
        cdty['uint4'] = 14

        cdty['char'] = 51
        cdty['uchar'] = 52
        cdty['epoch'] = 31
        cdty['epoch16'] = 32
        cdty['TT2000'] = 33
        self.cdf_data_types = cdty

        if status == 0:
            # Inquire as to files contents.
            self.inquire()

            # Get all attribute info.
            self._read_all_attribute_info()

            # Get z variable info, basic stats on the variables.
            self._read_all_z_variable_info()

            # Load variables.
            self.load_all_variables()

            # Load all variable attribute data (zVariables).
            self._read_all_z_attribute_data()
        else:
            raise IOError(fortran_cdf.statusreporter(status))

    def __enter__(self):
        return self

    def __exit__(self, type, value, tb):
        pass

    def __getitem__(self, key):
        """Return CDF variable by name."""
        return chameleon(self.fname, key, self.data[key], self.meta[key],
                         self.z_variable_info[key])

    def inquire(self):
        """Maps to fortran CDF_Inquire.

        Assigns parameters returned by CDF_Inquire
        to pysatCDF instance. Not intended
        for regular direct use by user.

        """

        name = copy.deepcopy(self.fname)
        stats = fortran_cdf.inquire(name)

        # Break out fortran output into something meaningful.
        status = stats[0]
        if status == 0:
            self._num_dims = stats[1]
            self._dim_sizes = stats[2]
            self._encoding = stats[3]
            self._majority = stats[4]
            self._max_rec = stats[5]
            self._num_r_vars = stats[6]
            self._num_z_vars = stats[7]
            self._num_attrs = stats[8]
        else:
            raise IOError(fortran_cdf.statusreporter(status))

    def _read_all_z_variable_info(self):
        """Gets all CDF z-variable information, not data though.

        Maps to calls using var_inquire. Gets information on
        data type, number of elements, number of dimensions, etc.

        """

        self.z_variable_info = {}
        self.z_variable_names_by_num = {}

        # Call Fortran that grabs all of the basic stats on all of the
        # zVariables in one go.
        info = fortran_cdf.z_var_all_inquire(self.fname, self._num_z_vars,
                                             len(self.fname))
        status = info[0]
        data_types = info[1]
        num_elems = info[2]
        rec_varys = info[3]
        dim_varys = info[4]
        num_dims = info[5]
        dim_sizes = info[6]
        rec_nums = info[7]
        var_nums = info[8]
        var_names = info[9]

        if status == 0:
            for i in np.arange(len(data_types)):
                out = {}
                out['data_type'] = data_types[i]
                out['num_elems'] = num_elems[i]
                out['rec_vary'] = rec_varys[i]
                out['dim_varys'] = dim_varys[i]
                out['num_dims'] = num_dims[i]

                # Only looking at first possible extra dimension.
                out['dim_sizes'] = dim_sizes[i, :1]
                if out['dim_sizes'][0] == 0:
                    out['dim_sizes'][0] += 1
                out['rec_num'] = rec_nums[i]
                out['var_num'] = var_nums[i]
                var_name = ''.join(var_names[i].astype('U'))
                out['var_name'] = var_name.rstrip()
                self.z_variable_info[out['var_name']] = out
                self.z_variable_names_by_num[out['var_num']] = var_name
        else:
            raise IOError(fortran_cdf.statusreporter(status))

    def load_all_variables(self):
        """Loads all variables from CDF.

        Note this routine is called automatically
        upon instantiation.

        """

        self.data = {}

        # Need to add r variable names.
        file_var_names = self.z_variable_info.keys()

        # Collect variable information for each, organize it neatly for
        # fortran call.
        dim_sizes = []
        rec_nums = []
        data_types = []
        names = []
        for i, name in enumerate(file_var_names):
            dim_sizes.extend(self.z_variable_info[name]['dim_sizes'])
            rec_nums.append(self.z_variable_info[name]['rec_num'])
            data_types.append(self.z_variable_info[name]['data_type'])
            names.append(name.ljust(256))
        dim_sizes = np.array(dim_sizes)
        rec_nums = np.array(rec_nums)
        data_types = np.array(data_types)

        # Individually load all variables by each data type.
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['real4'],
                                   fortran_cdf.get_multi_z_real4)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['float'],
                                   fortran_cdf.get_multi_z_real4)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['real8'],
                                   fortran_cdf.get_multi_z_real8)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['double'],
                                   fortran_cdf.get_multi_z_real8)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['int4'],
                                   fortran_cdf.get_multi_z_int4)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['uint4'],
                                   fortran_cdf.get_multi_z_int4,
                                   data_offset=2 ** 32)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['int2'],
                                   fortran_cdf.get_multi_z_int2)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['uint2'],
                                   fortran_cdf.get_multi_z_int2,
                                   data_offset=2 ** 16)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['int1'],
                                   fortran_cdf.get_multi_z_int1)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['uint1'],
                                   fortran_cdf.get_multi_z_int1,
                                   data_offset=2 ** 8)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['byte'],
                                   fortran_cdf.get_multi_z_int1)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['epoch'],
                                   fortran_cdf.get_multi_z_real8,
                                   epoch=True)
        self._call_multi_fortran_z(names, data_types, rec_nums, 2 * dim_sizes,
                                   self.cdf_data_types['epoch16'],
                                   fortran_cdf.get_multi_z_epoch16,
                                   epoch16=True)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['TT2000'],
                                   fortran_cdf.get_multi_z_tt2000,
                                   epoch=True)

        # Mark data has been loaded.
        self.data_loaded = True

        return

    def _call_multi_fortran_z(self, names, data_types, rec_nums,
                              dim_sizes, input_type_code, func,
                              epoch=False, data_offset=None, epoch16=False):
        """Calls fortran functions to load CDF variable data

        Parameters
        ----------
        names : list-like
            List of variables names.
        data_types : list-like
            List of all loaded data type codes as used by CDF.
        rec_nums : list-like
            List of record numbers in CDF file. Provided by variable_info.
        dim_sizes : list-like
            List of dimensions as provided by variable_info.
        input_type_code : int
            Specific type code to load.
        func : function
            Fortran function via python interface that will be used for
            actual loading.
        epoch : bool
            Flag indicating type is epoch. Translates things to datetime
            standard. (default=False)
        data_offset :
            Offset value to be applied to data. Required for unsigned
            integers in CDF. (default=None)
        epoch16 : bool
            Flag indicating type is epoch16. Translates things to datetime
            standard. (default=False)


        """

        # Isolate input type code variables from total supplied types.
        idx, = np.where(data_types == input_type_code)

        if len(idx) > 0:
            # Read all data of a given type at once.
            max_rec = rec_nums[idx].max()
            sub_names = np.array(names)[idx]
            sub_sizes = dim_sizes[idx]
            status, data = func(self.fname, sub_names.tolist(),
                                sub_sizes, sub_sizes.sum(), max_rec,
                                len(sub_names))
            if status == 0:
                # Account for quirks of CDF data storage for certain types.
                if data_offset is not None:
                    data = data.astype(int)
                    idx, idy, = np.where(data < 0)
                    data[idx, idy] += data_offset
                if epoch:
                    # Account for difference in seconds between
                    # CDF epoch and python's epoch, leap year in there
                    # (datetime(1971,1,2) -
                    #      datetime(1,1,1)).total_seconds()*1000
                    data -= 62167219200000
                    data = data.astype('<M8[ms]')
                if epoch16:
                    data[0::2, :] -= 62167219200
                    data = data[0::2, :] * 1E9 + data[1::2, :] / 1.E3
                    data = data.astype('datetime64[ns]')
                    sub_sizes /= 2

                # All data of a type has been loaded and tweaked as necessary.
                # Parse through returned array to break out the individual
                # variables as appropriate.
                self._process_return_multi_z(data, sub_names, sub_sizes)
            else:
                raise IOError(fortran_cdf.statusreporter(status))

        return

    def _process_return_multi_z(self, data, names, dim_sizes):
        """Process and attach data from various `fortran_cdf` 'get' functions.
        """

        d1 = 0
        d2 = 0
        for name, dim_size in zip(names, dim_sizes):
            d2 = d1 + dim_size
            if dim_size == 1:
                self.data[name.rstrip()] = data[d1, :]
            else:
                self.data[name.rstrip()] = data[d1:d2, :]
            d1 += dim_size

        return

    def _read_all_attribute_info(self):
        """Read all attribute properties, g, r, and z attributes"""

        num = copy.deepcopy(self._num_attrs)
        fname = copy.deepcopy(self.fname)
        out = fortran_cdf.inquire_all_attr(fname, num, len(fname))
        status = out[0]
        names = out[1].astype('U')
        scopes = out[2]
        max_gentries = out[3]
        max_rentries = out[4]
        max_zentries = out[5]
        attr_nums = out[6]

        global_attrs_info = {}
        var_attrs_info = {}
        if status == 0:
            for (name, scope, gentry,
                 rentry, zentry, num) in zip(names, scopes, max_gentries,
                                             max_rentries, max_zentries,
                                             attr_nums):
                name = ''.join(name)
                name = name.rstrip()
                nug = {}
                nug['scope'] = scope
                nug['max_gentry'] = gentry
                nug['max_rentry'] = rentry
                nug['max_zentry'] = zentry
                nug['attr_num'] = num
                flag = (gentry == 0) & (rentry == 0) & (zentry == 0)
                if not flag:
                    if scope == 1:
                        global_attrs_info[name] = nug
                    elif scope == 2:
                        var_attrs_info[name] = nug

            self.global_attrs_info = global_attrs_info
            self.var_attrs_info = var_attrs_info
        else:
            raise IOError(fortran_cdf.statusreporter(status))

        return

    def _read_all_z_attribute_data(self):
        """Read all CDF z-attribute data"""
        self.meta = {}

        # Collect attribute info needed to get more info from
        # fortran routines.
        max_entries = []
        attr_nums = []
        names = []
        attr_names = []
        names = self.var_attrs_info.keys()
        num_z_attrs = len(names)
        exp_attr_nums = []
        for key in names:
            max_entries.append(self.var_attrs_info[key]['max_zentry'])
            attr_nums.append(self.var_attrs_info[key]['attr_num'])
        attr_nums = np.array(attr_nums)
        max_entries = np.array(max_entries)

        info = fortran_cdf.z_attr_all_inquire(self.fname, attr_nums,
                                              num_z_attrs, max_entries,
                                              self._num_z_vars, len(self.fname))

        status = info[0]
        data_types = info[1]
        num_elems = info[2]
        entry_nums = info[3]

        if status == 0:
            for i, name in enumerate(names):
                self.var_attrs_info[name]['data_type'] = data_types[i]
                self.var_attrs_info[name]['num_elems'] = num_elems[i]
                self.var_attrs_info[name]['entry_num'] = entry_nums[i]
                exp_attr_nums.extend([self.var_attrs_info[name]['attr_num']]
                                     * len(entry_nums[i]))
                attr_names.extend([name] * len(entry_nums[i]))
        else:
            raise IOError(fortran_cdf.statusreporter(status))

        # All the info is now packed up.
        # Need to break it out to make it easier to load via fortran.
        # Includes:
        # attribute  id, entry id (zVariable ID), data_type, num_elems
        data_types = data_types.flatten()
        num_elems = num_elems.flatten()
        entry_nums = entry_nums.flatten()
        attr_nums = np.array(exp_attr_nums)

        # Drop everything that isn't valid
        idx, = np.where(entry_nums > 0)

        data_types = data_types[idx]
        num_elems = num_elems[idx]
        entry_nums = entry_nums[idx]
        attr_nums = attr_nums[idx]
        attr_names = np.array(attr_names)[idx]

        # Grab corresponding variable name for each attribute
        var_names = [self.z_variable_names_by_num[i].rstrip()
                     for i in entry_nums]

        # The names that go along with this are already set up
        # in `attr_names`.
        # Chunk by data type, grab largest num_elems.

        # Get data back, shorten to num_elems, add to structure.
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems,
                                        entry_nums, attr_nums, var_names,
                                        self.cdf_data_types['real4'],
                                        fortran_cdf.get_multi_z_attr_real4)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems,
                                        entry_nums, attr_nums, var_names,
                                        self.cdf_data_types['float'],
                                        fortran_cdf.get_multi_z_attr_real4)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems,
                                        entry_nums, attr_nums, var_names,
                                        self.cdf_data_types['real8'],
                                        fortran_cdf.get_multi_z_attr_real8)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems,
                                        entry_nums, attr_nums, var_names,
                                        self.cdf_data_types['double'],
                                        fortran_cdf.get_multi_z_attr_real8)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems,
                                        entry_nums, attr_nums, var_names,
                                        self.cdf_data_types['byte'],
                                        fortran_cdf.get_multi_z_attr_int1)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems,
                                        entry_nums, attr_nums, var_names,
                                        self.cdf_data_types['int1'],
                                        fortran_cdf.get_multi_z_attr_int1)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems,
                                        entry_nums, attr_nums, var_names,
                                        self.cdf_data_types['uint1'],
                                        fortran_cdf.get_multi_z_attr_int1,
                                        data_offset=256)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems,
                                        entry_nums, attr_nums, var_names,
                                        self.cdf_data_types['int2'],
                                        fortran_cdf.get_multi_z_attr_int2)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems,
                                        entry_nums, attr_nums, var_names,
                                        self.cdf_data_types['uint2'],
                                        fortran_cdf.get_multi_z_attr_int2,
                                        data_offset=65536)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems,
                                        entry_nums, attr_nums, var_names,
                                        self.cdf_data_types['int4'],
                                        fortran_cdf.get_multi_z_attr_int4)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems,
                                        entry_nums, attr_nums, var_names,
                                        self.cdf_data_types['uint4'],
                                        fortran_cdf.get_multi_z_attr_int4,
                                        data_offset=2 ** 32)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems,
                                        entry_nums, attr_nums, var_names,
                                        self.cdf_data_types['char'],
                                        fortran_cdf.get_multi_z_attr_char)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems,
                                        entry_nums, attr_nums, var_names,
                                        self.cdf_data_types['uchar'],
                                        fortran_cdf.get_multi_z_attr_char)
        return

    def _call_multi_fortran_z_attr(self, names, data_types, num_elems,
                                   entry_nums, attr_nums, var_names,
                                   input_type_code, func, data_offset=None):
        """Calls Fortran function that reads attribute data.

        data_offset translates unsigned into signed.
        If number read in is negative, offset added.
        """
        # Isolate input type code variables.
        idx, = np.where(data_types == input_type_code)

        if len(idx) > 0:
            # Maximum array dimension.
            max_num = num_elems[idx].max()
            sub_num_elems = num_elems[idx]
            sub_names = np.array(names)[idx]
            sub_var_names = np.array(var_names)[idx]

            # zVariable numbers, 'entry' number.
            sub_entry_nums = entry_nums[idx]

            # Attribute number.
            sub_attr_nums = attr_nums[idx]
            status, data = func(self.fname, sub_attr_nums, sub_entry_nums,
                                len(sub_attr_nums), max_num, len(self.fname))
            if (status == 0).all():
                if data_offset is not None:
                    data = data.astype(int)
                    idx, idy, = np.where(data < 0)
                    data[idx, idy] += data_offset
                self._process_return_multi_z_attr(data, sub_names,
                                                  sub_var_names, sub_num_elems)
            else:
                # Raise the first error.
                idx, = np.where(status != 0)
                raise IOError(fortran_cdf.statusreporter(status[idx][0]))
        return

    def _process_return_multi_z_attr(self, data, attr_names, var_names,
                                     sub_num_elems):
        '''process and attach data from fortran_cdf.get_multi_*'''

        for i, (attr_name, var_name, num_e) in enumerate(zip(attr_names,
                                                             var_names,
                                                             sub_num_elems)):
            if var_name not in self.meta.keys():
                self.meta[var_name] = {}
            if num_e == 1:
                self.meta[var_name][attr_name] = data[i, 0]
            else:
                if data[i].dtype == '|S1':
                    chars = []
                    for d in data[i, :num_e]:
                        try:
                            chars.append(d.astype('U'))
                        except UnicodeDecodeError:
                            # Uninterpretable character was encountered.
                            # Fill inserted.
                            chars.append('*')
                    self.meta[var_name][attr_name] = ''.join(chars).rstrip()
                else:
                    self.meta[var_name][attr_name] = data[i, 0:num_e]

    def to_pysat(self, flatten_twod=True, units_label='UNITS',
                 name_label='LONG_NAME', fill_label='FILLVAL',
                 plot_label='FIELDNAM', min_label='VALIDMIN',
                 max_label='VALIDMAX', notes_label='VAR_NOTES',
                 desc_label='CATDESC', axis_label='LABLAXIS'):
        """Export loaded CDF data into data, meta for pysat module.

        Parameters
        ----------
        flatten_twod : bool (True)
            If True, then two dimensional data is flattened across
            columns. Name mangling is used to group data, first column
            is 'name', last column is 'name_end'. In between numbers are
            appended 'name_1', 'name_2', etc. All data for a given 2D array
            may be accessed via, data.ix[:,'item':'item_end']
            If False, then 2D data is stored as a series of DataFrames,
            indexed by Epoch. data.ix[0, 'item']
        units_label : str
            Identifier within metadata for units. Defults to CDAWab standard.
            (default='UNITS')
        name_label : str
            Identifier within metadata for variable name, not normally present
            within CDAWeb files. If not, will use values from the variable name
            in the file. (default='LONG_NAME')
        fill_label : str
            Identifier within metadata for Fill Values. Defults to CDAWab
            standard. (default='FILLVAL')
        plot_label : str
            Identifier within metadata for variable name used when plotting.
            Defults to CDAWab standard. (default='FIELDNAM')
        min_label : str
            Identifier within metadata for minimim variable value.
            Defults to CDAWab standard. (default='VALIDMIN')
        max_label : str
            Identifier within metadata for maximum variable value.
            Defults to CDAWab standard. (default='VALIDMAX')
        notes_label : str
            Identifier within metadata for notes. Defults to CDAWab standard.
             (default='VAR_NOTES')
        desc_label : str
            Identifier within metadata for a variable description.
            Defults to CDAWab standard. (default='CATDESC')
        axis_label : str
            Identifier within metadata for axis name used when plotting.
            Defults to CDAWab standard. (default='LABLAXIS')


        Returns
        -------
        data : pandas.DataFrame, pysat.Meta
            Data suitable for attachment to a pysat.Instrument object.
        meta : pysat.Meta
            pysat Metadata class suitable for attachment to a pysat.Instrument
            object.

        Note
        ----
        The *_labels should be set to the values in the file, if present.
        Note that once the meta object returned from this function is attached
        to a pysat.Instrument object then the *_labels on the Instrument
        are assigned to the newly attached Meta object.

        The pysat Meta object will use data with labels that match the patterns
        in *_labels even if the case does not match.

        """

        # Copy data.
        cdata = self.data.copy()

        # Create a dictionary of the labels for use in initializing
        # the Metadata.
        labels = {'units': (units_label, str), 'name': (name_label, str),
                  'notes': (notes_label, str), 'desc': (desc_label, str),
                  'plot': (plot_label, str), 'axis': (axis_label, str),
                  'scale': ('scale', str), 'min_val': (min_label, float),
                  'max_val': (max_label, float),
                  'fill_val': (fill_label, float)}

        # Create pysat.Meta object using data above
        # and utilize the attribute labels provided by the user.
        meta = pysat.Meta(pandas.DataFrame.from_dict(self.meta,
                                                     orient='index'),
                          labels=labels)

        # Account for different possible cases for Epoch, epoch, EPOCH, epOch.
        lower_names = [name.lower() for name in meta.keys()]
        for name, true_name in zip(lower_names, meta.keys()):
            if name == 'epoch':
                meta.data.rename(index={true_name: 'Epoch'}, inplace=True)
                epoch = cdata.pop(true_name)
                cdata['Epoch'] = epoch

        # Ready to format data, iterate over all of the data names
        # and put into a pandas DataFrame.
        two_d_data = []
        drop_list = []
        for name in cdata.keys():
            temp = np.shape(cdata[name])

            # Treat 2 dimensional data differently.
            if len(temp) == 2:
                if not flatten_twod:
                    # Put 2D data into a Frame at each time,
                    # remove data from dict when adding to the DataFrame.
                    frame = pandas.DataFrame(cdata[name].flatten(),
                                             columns=[name])
                    drop_list.append(name)

                    step = temp[0]
                    new_list = []
                    new_index = np.arange(step)
                    for i in np.arange(len(epoch)):
                        new_list.append(frame.iloc[i * step:(i + 1) * step, :])
                        new_list[-1].index = new_index

                    new_frame = pandas.Series(new_list, index=epoch, name=name)
                    two_d_data.append(new_frame)

                else:
                    # Flatten 2D into series of 1D columns.
                    new_names = [name + '_{i}'.format(i=i)
                                 for i in np.arange(temp[0] - 2)]
                    new_names.append(name + '_end')
                    new_names.insert(0, name)

                    # Remove data from dict when adding to the DataFrame.
                    drop_list.append(name)
                    frame = pandas.DataFrame(cdata[name].T,
                                             index=epoch,
                                             columns=new_names)
                    two_d_data.append(frame)

        for name in drop_list:
            _ = cdata.pop(name)

        # All of the data left over is 1D, add as Series
        data = pandas.DataFrame(cdata, index=epoch)
        two_d_data.append(data)
        data = pandas.concat(two_d_data, axis=1)
        data.drop('Epoch', axis=1, inplace=True)
        return data, meta


class chameleon(object):
    """Provides multiple access mechanisms for larger CDF object.

    Supports spacepy access pattern along with pysatCDF native
    data access pattern.

    """

    def __init__(self, fname, name, data, attr, info):
        self.fname = fname
        self.data = data
        self.attrs = attr
        self.name = name
        self.info = info

    def __getitem__(self, key):
        if key is Ellipsis:
            return self.data
        else:
            return self.data[key]

    def __repr__(self):
        out = 'CDF filename : ' + self.fname + '\n'
        out += 'CDF variable name: ' + self.name + '\n'
        for key in self.info.keys():
            out += key + " : " + str(self.info[key]) + '\n'

        return out
