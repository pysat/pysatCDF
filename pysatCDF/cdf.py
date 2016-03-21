from __future__ import print_function
from __future__ import absolute_import
import sys
import copy

import numpy as np

from . import fortran_cdf



class CDF(object):
    """Reads data from NASA Common Data Format (CDF) files.
    
    """
    
    def __init__(self, fname):
        # in CDF docs it says don't include .cdf in name
        if fname[-4:].lower() == '.cdf':
            name = fname[:-4]
        else:
            name = fname

        self.fname = name
        status = fortran_cdf.open(name)

        self.data_loaded = False
        
        # CDF library numeric codes for data types
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
            # inquire as to files contents
            self.inquire()            
            # get all attribute info
            self._read_all_attribute_info()    
            # get z variable info
            self._read_all_z_variable_info()
            # load variables
            self.load_all_variables()
            # load all variable attribute data (zVariables)
            self._read_all_z_attribute_data()
        else:            
            raise IOError(fortran_cdf.statusreporter(status))

    def __enter__(self):
        return self

    def __exit__(self, type, value, tb):
        pass

    def __getitem__(self, key):
        '''variable with name'''
        if not self.data_loaded:
            dim_size = self.z_variable_info[key]['dim_sizes']
            # only tracking up to two dimensional things
            dim_size = dim_size[0]
            if dim_size == 0:
                dim_size += 1
            rec_num = self.z_variable_info[key]['rec_num']
            status, data = fortran_cdf.get_z_var(self.fname, key, dim_size, rec_num )
            if status == 0:
                if dim_size == 1:
                    data = data[0,:]
                return data
            else:
                #raise ValueError('CDF Error status :', status)
                raise IOError(fortran_cdf.statusreporter(status))
        else:
            return chameleon(self.fname, key, self.data[key], 
                             self.meta[key], 
                             self.z_variable_info[key])

    def inquire(self):
        """Maps to fortran CDF_Inquire"""
        
        name = copy.deepcopy(self.fname)
        stats = fortran_cdf.inquire(name)

        # break out fortran output into something meaningful
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
    
    def _read_r_variable_info(self):
        """ Reads r-variable info, one at a time"""
        
        for i in xrange(self._num_r_vars):
            info = fortran_cdf.var_inquire(self.fname, i+1)


    def _read_z_variable_info(self):
        """Reads z-Variable info, one at a time"""
        
        self.z_variable_info = {}
        i = 1
        good = 1
        while True:
            info = fortran_cdf.z_var_inquire(self.fname, i+1)
            status = info[0]
            if (status == 0):
                out = {}
                out['data_type'] = info[1]
                out['num_elems'] = info[2]
                out['rec_vary'] = info[3]
                out['dim_varys'] = info[4]
                out['num_dims'] = info[5]
                # only looking at first possible extra dimension
                out['dim_sizes'] = info[6][:1]
                if out['dim_sizes'][0] == 0:
                    out['dim_sizes'][0] += 1
                out['rec_num'] = info[7]
                out['var_name'] = info[8].rstrip()
                self.z_variable_info[out['var_name']] = out
                good += 1
                #print (out, good)
                #print(out)
            i += 1
            if good >= self._num_z_vars:
                break

    def _read_all_z_variable_info(self):
        """Gets all CDF z-variable information, not data though."""
        
        self.z_variable_info = {}
        self.z_variable_names_by_num = {}
        
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

        if (status == 0):
            for i in np.arange(len(data_types)):
                out = {}
                out['data_type'] = data_types[i]
                out['num_elems'] = num_elems[i]
                out['rec_vary'] = rec_varys[i]
                out['dim_varys'] = dim_varys[i]
                out['num_dims'] = num_dims[i]
                # only looking at first possible extra dimension
                out['dim_sizes'] = dim_sizes[i,:1]
                if out['dim_sizes'][0] == 0:
                    out['dim_sizes'][0] += 1
                out['rec_num'] = rec_nums[i]
                out['var_num'] = var_nums[i]
                #print var_name
                var_name = ''.join(var_names[i])
                out['var_name'] = var_name.rstrip()
                self.z_variable_info[out['var_name']] = out
                self.z_variable_names_by_num[out['var_num']] = var_name
             


    def load_all_variables(self):
        """Loads all variables from CDF.
        
        Note this routine is called automatically
        upon instantiation.
        
        """
        
        self.data = {}
        # need to add r variable names
        names = self.z_variable_info.keys()
        # collect variable information for each
        # organize it neatly for fortran call
        dim_sizes = []
        rec_nums = []
        data_types = []
        for i,name in enumerate(names):
            dim_sizes.extend(self.z_variable_info[name]['dim_sizes'])
            rec_nums.append(self.z_variable_info[name]['rec_num'])
            data_types.append(self.z_variable_info[name]['data_type'])
            names[i] = name.ljust(256)    
        dim_sizes = np.array(dim_sizes) 
        rec_nums = np.array(rec_nums)
        data_types = np.array(data_types)


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
                                   data_offset=2**32)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['int2'], 
                                   fortran_cdf.get_multi_z_int2)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['uint2'], 
                                   fortran_cdf.get_multi_z_int2,
                                   data_offset=2**16)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['int1'], 
                                   fortran_cdf.get_multi_z_int1)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['uint1'], 
                                   fortran_cdf.get_multi_z_int1,
                                   data_offset=2**8)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['byte'], 
                                   fortran_cdf.get_multi_z_int1)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['epoch'], 
                                   fortran_cdf.get_multi_z_real8,
                                   epoch=True)
        self._call_multi_fortran_z(names, data_types, rec_nums, 2*dim_sizes,
                                   self.cdf_data_types['epoch16'], 
                                   fortran_cdf.get_multi_z_epoch16,
                                   epoch16=True)
        self._call_multi_fortran_z(names, data_types, rec_nums, dim_sizes,
                                   self.cdf_data_types['TT2000'], 
                                   fortran_cdf.get_multi_z_tt2000,
                                   epoch=True)
                         
        self.data_loaded = True

    def _call_multi_fortran_z(self, names, data_types, rec_nums,
                                    dim_sizes, input_type_code, func,
                                    epoch=False, data_offset=None, epoch16=False):
        """Calls fortran functions to load CDF variable data
        
        data_offset translates betwen unsigned to signed integers
        applied for data vlues less than 0
        
        """
        # isolate input type code variables
        idx, = np.where(data_types == input_type_code)

        if len(idx) > 0:
            max_rec = rec_nums[idx].max()
            sub_names = np.array(names)[idx]
            sub_sizes = dim_sizes[idx]
            status, data = func(self.fname, sub_names.tolist(),
                            sub_sizes, sub_sizes.sum(), max_rec, len(sub_names) ) 
            if status == 0:
                if data_offset is not None:
                    data = data.astype(int)
                    idx, idy, = np.where(data < 0)
                    data[idx,idy] += data_offset 

                if epoch:
                    # account for difference in seconds between
                    # CDF epoch and python's epoch, leap year in there
                    # (datetime(1971,1,2) - 
                    #      datetime(1,1,1)).total_seconds()*1000
                    data -= 62167219200000
                    data = data.astype('<M8[ms]')
                if epoch16:
                    data[0::2,:] -= 62167219200
                    data = data[0::2,:]*1E9 + data[1::2,:]/1.E3
                    data = data.astype('datetime64[ns]')
                    sub_sizes /= 2
                self._process_return_multi_z(data, sub_names, sub_sizes)   
            else:
                #raise ValueError('CDF Error code :', status)  
                raise IOError(fortran_cdf.statusreporter(status))
                
    def _process_return_multi_z(self, data, names, dim_sizes):
        '''process and attach data from fortran_cdf.get_multi_*'''
        # process data
        d1 = 0
        d2 = 0
        for name, dim_size in zip(names, dim_sizes):
            d2 = d1 + dim_size
            if dim_size == 1:
                self.data[name.rstrip()] = data[d1, :]
            else:
                self.data[name.rstrip()] = data[d1:d2, :]
            d1 += dim_size    
                

    
    def _read_attribute_info(self):
        """Read all attribute info, one at a time"""
        i = 1
        good = 0
        while True:
            #for i in xrange(self._num_attrs):
            info = fortran_cdf.attr_inquire(self.fname, i)
            
            status = info[0]
            if (status == 0):
                name = info[1].rstrip()
                scope = info[2]
                max_entry = info[3]
                good += 1
                #print (name, scope, max_entry)
            i += 1
            if good >= self._num_attrs:
                break


    def _read_all_attribute_info(self):
        """Read all attribute properties, g, r, and z attributes"""
        
        num = copy.deepcopy(self._num_attrs)
        name = copy.deepcopy(self.fname)
        out = fortran_cdf.inquire_all_attr(name, num, len(name))
        status = out[0]
        names = out[1]
        scopes = out[2]
        max_gentries = out[3]
        max_rentries = out[4]
        max_zentries = out[5]
        attr_nums = out[6]

        global_attrs_info = {}
        var_attrs_info = {}
        if status == 0:
            for name, scope, gentry, rentry, zentry, num in zip(names, scopes, 
                    max_gentries, max_rentries,max_zentries, attr_nums):
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
                    if scope ==1:
                        global_attrs_info[name] = nug  
                    elif scope ==2:
                        var_attrs_info[name] = nug
                  
                
            self.global_attrs_info = global_attrs_info
            self.var_attrs_info = var_attrs_info
        else:
            raise IOError(fortran_cdf.statusreporter(status))            

    def _read_all_z_attribute_data(self):
        """Read all CDF z-attribute data"""
        self.meta = {}        
        # collect attribute info needed to get more info from 
        # fortran routines
        max_entries = []; attr_nums = []
        names = [];  attr_names = []
        names = self.var_attrs_info.keys()
        num_z_attrs = len(names)
        exp_attr_nums = []
        for key in names:
            max_entries.append(self.var_attrs_info[key]['max_zentry'])
            attr_nums.append(self.var_attrs_info[key]['attr_num'])
        attr_nums = np.array(attr_nums)
        max_entries = np.array(max_entries)

        info = fortran_cdf.z_attr_all_inquire(self.fname, attr_nums,
                 num_z_attrs, max_entries, self._num_z_vars, len(self.fname))

        status = info[0]
        data_types = info[1]
        num_elems = info[2]
        entry_nums = info[3]
        

        if (status == 0):
            for i,name in enumerate(names):
                self.var_attrs_info[name]['data_type'] = data_types[i]
                self.var_attrs_info[name]['num_elems'] = num_elems[i]
                self.var_attrs_info[name]['entry_num'] = entry_nums[i]
                exp_attr_nums.extend([self.var_attrs_info[name]['attr_num']]*len(entry_nums[i]))
                attr_names.extend([name]*len(entry_nums[i]))
        else:
            raise IOError(fortran_cdf.statusreporter(status))            
                
        # all the info is now packed up
        # need to break it out to make it easier to load via fortran
        # all of this junk
        # attribute  id, entry id (zVariable ID), data_type, num_elems
        # should just need to flatten this stuff

        data_types = data_types.flatten()
        num_elems = num_elems.flatten()
        entry_nums = entry_nums.flatten()
        attr_nums = np.array(exp_attr_nums)
        # drop everything that isn't valis
        idx, = np.where(entry_nums > 0)
        
        data_types = data_types[idx]
        num_elems = num_elems[idx]
        entry_nums = entry_nums[idx]
        attr_nums = attr_nums[idx]
        attr_names = np.array(attr_names)[idx]
        # grad corresponding variable name for each attribute
        var_names = [self.z_variable_names_by_num[i].rstrip() for i in entry_nums]

        # the names that go along with this are already set up

        # in attr_names
        # chunk by data type, grab largest num_elems
        
        # get data back, shorten to num_elems, add to structure
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems, 
                                   entry_nums, attr_nums, var_names, self.cdf_data_types['real4'], 
                                   fortran_cdf.get_multi_z_attr_real4)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems, 
                                   entry_nums, attr_nums, var_names, self.cdf_data_types['float'], 
                                   fortran_cdf.get_multi_z_attr_real4)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems, 
                                   entry_nums, attr_nums, var_names, self.cdf_data_types['real8'], 
                                   fortran_cdf.get_multi_z_attr_real8)                              
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems, 
                                   entry_nums, attr_nums, var_names, self.cdf_data_types['double'], 
                                   fortran_cdf.get_multi_z_attr_real8)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems, 
                                   entry_nums, attr_nums, var_names, self.cdf_data_types['byte'], 
                                   fortran_cdf.get_multi_z_attr_int1)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems, 
                                   entry_nums, attr_nums, var_names, self.cdf_data_types['int1'], 
                                   fortran_cdf.get_multi_z_attr_int1)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems, 
                                   entry_nums, attr_nums, var_names, self.cdf_data_types['uint1'], 
                                   fortran_cdf.get_multi_z_attr_int1,
                                   data_offset=256)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems, 
                                   entry_nums, attr_nums, var_names, self.cdf_data_types['int2'], 
                                   fortran_cdf.get_multi_z_attr_int2)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems, 
                                   entry_nums, attr_nums, var_names, self.cdf_data_types['uint2'], 
                                   fortran_cdf.get_multi_z_attr_int2,
                                   data_offset=65536)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems, 
                                   entry_nums, attr_nums, var_names, self.cdf_data_types['int4'], 
                                   fortran_cdf.get_multi_z_attr_int4)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems, 
                                   entry_nums, attr_nums, var_names, self.cdf_data_types['uint4'], 
                                   fortran_cdf.get_multi_z_attr_int4,
                                   data_offset=2**32)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems, 
                                   entry_nums, attr_nums, var_names, self.cdf_data_types['char'], 
                                   fortran_cdf.get_multi_z_attr_char)
        self._call_multi_fortran_z_attr(attr_names, data_types, num_elems, 
                                   entry_nums, attr_nums, var_names, self.cdf_data_types['uchar'], 
                                   fortran_cdf.get_multi_z_attr_char)


                                                                                                                                                                
    def _call_multi_fortran_z_attr(self, names, data_types, num_elems,
                                    entry_nums, attr_nums, var_names,
                                    input_type_code, func, data_offset=None):
        """Calls Fortran function that reads attribute data.
        
        data_offset translates unsigned into signed.
        If number read in is negative, offset added.
        """ 
        # isolate input type code variables
        idx, = np.where(data_types == input_type_code)

        if len(idx) > 0:
            # maximimum array dimension
            max_num = num_elems[idx].max()
            sub_num_elems = num_elems[idx]
            sub_names = np.array(names)[idx]
            sub_var_names = np.array(var_names)[idx]
            # zVariable numbers, 'entry' number
            sub_entry_nums = entry_nums[idx]
            # attribute number
            sub_attr_nums = attr_nums[idx]
            status, data = func(self.fname, sub_attr_nums, sub_entry_nums,
                            len(sub_attr_nums), max_num, len(self.fname) ) 
            if (status == 0).all():
                if data_offset is not None:
                    data = data.astype(int)
                    idx, idy, = np.where(data < 0)
                    data[idx,idy] += data_offset 
                self._process_return_multi_z_attr(data, sub_names, 
                        sub_var_names, sub_num_elems)   
            else:
                #raise ValueError('CDF Error code :', status) 
                idx, = np.where(status != 0)
                # raise first error
                raise IOError(fortran_cdf.statusreporter(status[idx][0]))

    def _process_return_multi_z_attr(self, data, attr_names, var_names, sub_num_elems):
        '''process and attach data from fortran_cdf.get_multi_*'''
        # process data

        for i, (attr_name, var_name, num_e) in enumerate(zip(attr_names, var_names, sub_num_elems)):
            if var_name not in self.meta.keys():
                self.meta[var_name] = {}
            if num_e == 1:
                self.meta[var_name][attr_name] = data[i,0]
            else:
                if data[i].dtype == '|S1':
                    self.meta[var_name][attr_name] = ''.join(data[i,0:num_e]).rstrip()
                else:
                    self.meta[var_name][attr_name] = data[i,0:num_e]

    def to_pysat(self):
        """
        Exports loaded CDF data into data,meta for pysat module
        
        Returns
        -------
        data, meta
        
        """
        
        import string
        import pysat
        import pandas
         
        meta = pysat.Meta(pysat.DataFrame.from_dict(self.meta, 
                                                        orient = 'index'))
        # all column names should be lower case
        lower_names = map(string.lower, meta.data.columns)
        meta.data.columns = lower_names 
        if 'lablaxis' in meta.data.columns:
            meta.data.drop('long_name', inplace=True, axis=1)
            meta.data.rename(columns={'lablaxis': 'long_name'}, inplace=True)
        if 'catdesc' in meta.data.columns:
            meta.data.rename(columns={'catdesc': 'description'}, inplace=True) 
            
        # account for different possible cases for Epoch, epoch, EPOCH, epOch
        lower_names = map(string.lower, meta.data.index.values) 
        for name, true_name in zip(lower_names, meta.data.index.values): 
            if name == 'epoch':
                meta.data.rename(index={true_name : 'Epoch'}, inplace=True)
                epoch = self.data.pop(true_name)
                self.data['Epoch'] = epoch
                
        # treat 2 dimensional data differently
        two_d_data = []
        for name in self.data.keys():
            temp = np.shape(self.data[name])
            
            if len(temp) == 2:
                new_names = [name+'_{i}'.format(i=i) for i in np.arange(temp[0]-2)]
                new_names.append(name+'_end')
                new_names.insert(0, name)
                frame = pysat.DataFrame(self.data.pop(name).T, 
                                        index = epoch,
                                        columns = new_names)
                two_d_data.append(frame)
                
        # series of 1D data streams                                           
        data = pysat.DataFrame(self.data, index=epoch)
        two_d_data.append(data)
        data = pandas.concat(two_d_data , axis=1)
        data.drop('Epoch', axis=1, inplace=True)
        return data, meta
                                               
class chameleon(object):
    
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
        out += 'CDF variable name: ' + self.name +'\n'
        for key in self.info.keys():
            out += key + " : " + str(self.info[key]) + '\n'

        return out      
