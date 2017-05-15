C----------------------------------------------------------------------
C To compile, install NADA CDF library then run at terminal:
C---------------------------------------------------------------------
C f2py -c --include-paths $CDF_INC -I$CDF_INC $CDF_LIB/libcdf.a -m fortran_cdf fortran_cdf.f -lm -lc


C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                        

        subroutine true_open (fname, id, status)
        INCLUDE 'cdf.inc'
         ! CDF identifier.
        INTEGER*4 id           
        ! CDF completion status.
        INTEGER*4 status      
        ! filename to open    
        character*(*) fname         

        CALL CDF_open_cdf (fname, id, status)
C        IF (status .NE. CDF_OK) CALL StatusHandler (status)
        return
        end        
        
C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------
                                
        subroutine true_close(id, status)
        INCLUDE 'cdf.inc'

        ! CDF identifier.
        INTEGER*4 id            
        ! CDF completion status.
        INTEGER*4 status              

        CALL CDF_close_cdf (id, status)
C        IF (status .NE. CDF_OK) CALL StatusHandler (status)

        return 
        end
                                
C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------


        subroutine open(status, fname)
        INCLUDE 'cdf.inc'
        ! CDF completion status.
        INTEGER*4 status, temp_status      
        ! filename to open    
        character*(*)   fname       
        ! CDF identifier.
        INTEGER*4 id            

Cf2py   intent(in) fname
Cf2py   intent(out) status

        CALL true_open (fname, id, status)

        CALL true_close (id, temp_status)

        return 
        end

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------
  
        subroutine attr_inquire(status, attr_name, attr_scope,
     &max_entry, fname, attr_num)
        INCLUDE 'cdf.inc'
        INTEGER*4 status, temp_status        
        character*(*)  fname        

        INTEGER*4 id
        INTEGER*4 attr_num
        CHARACTER attr_name*(CDF_ATTR_NAME_LEN256)
        INTEGER*4 attr_scope
        INTEGER*4 max_entry 

Cf2py   intent(in) fname, attr_num
Cf2py   intent(out) attr_name, attr_scope 
Cf2py   intent(out) max_entry, status

        CALL true_open (fname, id, temp_status)

        CALL CDF_attr_inquire (id, attr_num, attr_name, attr_scope, 
     &max_entry, status)
C        IF (status .LT. CDF_OK) THEN
C          IF (status .NE. NO_SUCH_ATTR) CALL StatusHandler (status) 
C        ENDIF
        
        CALL true_close(id, temp_status)

        return 
        end

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------   
      
        subroutine inquire_all_attr(status, attr_names, 
     &attr_scopes, max_gentries, max_rentries, max_zentries,
     &attr_nums, fname, max_attr_num, fname_len)
        implicit NONE
        INCLUDE 'cdf.inc'
        integer*4 fname_len
        ! filename to open  
        character*(fname_len)   fname 
        ! total number of attr
        INTEGER*4 max_attr_num 
        INTEGER*4 status, temp_status        
        CHARACTER*(256)    attr_names(max_attr_num)
        INTEGER*4, dimension(max_attr_num) :: attr_scopes
        INTEGER*4, dimension(max_attr_num) :: max_gentries
        INTEGER*4, dimension(max_attr_num) :: max_rentries
        INTEGER*4, dimension(max_attr_num) :: max_zentries

        INTEGER*4, dimension(max_attr_num) :: attr_nums
        INTEGER*4 id, j, flag, good
        
Cf2py   intent(in) fname, max_attr_num, fname_len
Cf2py   intent(out) status, attr_names, attr_scopes, max_gentries, attr_nums 
Cf2py   intent(out) max_rentries, max_zentries
Cf2py   depend(max_attr_num) attr_scopes, attr_names, max_entries, attr_nums
Cf2py   depend(fname_len) fname

        CALL true_open (fname, id, temp_status)

        flag = 1
        j = 1
        good = 1
        if (temp_status .eq. CDF_OK) then
        do while (flag .eq. 1)
          CALL CDF_inquire_attr (id, j, attr_names(good),
     &attr_scopes(good), max_gentries(good), max_rentries(good),
     &max_zentries(good), status)
          IF (status .LT. CDF_OK) THEN
            IF (status .NE. NO_SUCH_ATTR) CALL StatusHandler (status) 
          else 
            attr_nums(good) = j
            ! get more information about the attribute
            good = good+1
          ENDIF
          j = j + 1
          if ( good .gt. max_attr_num) then
            flag = 0
          end if
        end do
        CALL true_close(id, temp_status)
        end if


        return 
        end

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           

        subroutine z_attr_all_inquire(status, data_types, num_elems,
     &entry_nums, fname, attr_nums, num_attr, max_entries, num_vars,
     &fname_len)
        INCLUDE 'cdf.inc'
        ! CDF completion status.
        INTEGER*4 status, temp_status
        ! maximum atttribute number in CDF
        INTEGER*4 max_num        
        ! number of attributes
        INTEGER*4 num_attr
        INTEGER*4 id, fname_len, loop
        ! filename to open   
        character*(fname_len)   fname  
        ! id number for each attribute
        INTEGER*4 attr_nums(num_attr)    
        ! max variable id number for each attribute
        INTEGER*4 max_entries(num_attr) 
        ! number of variables
        INTEGER*4 num_vars 
        INTEGER*4 entryN  
        
        INTEGER*4, DIMENSION(num_attr,num_vars) :: data_types
        INTEGER*4, DIMENSION(num_attr,num_vars) :: num_elems
        INTEGER*4, DIMENSION(num_attr,num_vars) :: entry_nums

Cf2py   intent(in) fname, attr_nums, num_attr, max_entries
Cf2py   intent(in) num_vars, fname_len
Cf2py   intent(out) status, data_types, num_elems, entry_nums
Cf2py   depend(num_attr) attr_nums, max_entries
Cf2py   depend(num_attr,num_vars) data_types, num_elems, entry_nums
Cf2py   depend(fname_len) fname
C        write(6,*) CDF_BYTE, CDF_CHAR, CDF_INT1, CDF_UCHAR
C        write(6,*) CDF_UINT1, CDF_INT2, CDF_UINT2, CDF_INT4
C        write(6,*) CDF_UINT4, CDF_REAL4, CDF_FLOAT, CDF_REAL8
C        write(6,*) CDF_DOUBLE, CDF_EPOCH, CDF_EPOCH16
        CALL true_open (fname, id, temp_status)
! attr_nums, input atttribute number to get info        
        ! loop over attributes
        do loop = 1, num_attr
          ! loop over variables
          ! max entry comes from input
          do entryN = 1, max_entries(loop)
            CALL CDF_inquire_attr_zentry(id, attr_nums(loop),
     &entryN, data_types(loop, entryN), num_elems(loop, entryN),
     &status)
            if (status .lt. CDF_OK) then
              IF (status .NE. NO_SUCH_ENTRY) CALL StatusHandler(status)
            else
              entry_nums(loop, entryN) = entryN
            endif
          end do
        end do
        CALL true_close (id, temp_status)

        return 
        end

                                                                                        
C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                                                        
                                                         
 
        subroutine get_multi_z_attr_real8(status, buffer, 
     &fname, attr_nums, entry_nums, num_attr, max_dim,
     &fname_len)
        INCLUDE 'cdf.inc'
        INTEGER fname_len, max_dim 
        character*(fname_len)   fname 
        ! filename to open     
        ! attr_nums : attribute numbers to pull from
        ! entry_nums: zVar numbers, called entry nums
        ! num_attr : number of attributes to pull
        ! max_dim : maximum array dimension 
        ! num : length of filename
        ! buffer : data to return
        INTEGER*4 id, j
        INTEGER*4 temp_status, num_attr
        INTEGER*4, dimension(num_attr) :: status
        INTEGER*4, dimension(num_attr) :: attr_nums
        INTEGER*4, dimension(num_attr) :: entry_nums
        REAL*8, DIMENSION(num_attr, max_dim) :: buffer
Cf2py   intent(in) fname, attr_nums, entry_nums, num_attr
Cf2py   intent(in) max_dim, num
Cf2py   intent(out) status, buffer
Cf2py   depend(num_attr) status, attr_nums, entry_nums
Cf2py   depend(fname_len) fname

        CALL true_open (fname, id, temp_status) 
        
        do j=1, num_attr              

          CALL CDF_get_attr_zentry(id, attr_nums(j),
     &entry_nums(j), buffer(j,:), status(j))
                                                        
          IF (status(j) .NE. CDF_OK) CALL StatusHandler(status(j))
        end do
        
        CALL true_close (id, temp_status)        
        return
        end 

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                                                                                                 
 
        subroutine get_multi_z_attr_real4(status, buffer, 
     &fname, attr_nums, entry_nums, num_attr, max_dim,
     &fname_len)
        INCLUDE 'cdf.inc'
        INTEGER fname_len, max_dim 
        character*(fname_len)   fname 
        ! filename to open     
        ! attr_nums : attribute numbers to pull from
        ! entry_nums: zVar numbers, called entry nums
        ! num_attr : number of attributes to pull
        ! max_dim : maximum array dimension 
        ! num : length of filename
        ! buffer : data to return
        INTEGER*4 id, j
        INTEGER*4 temp_status, num_attr
        INTEGER*4, dimension(num_attr) :: status
        INTEGER*4, dimension(num_attr) :: attr_nums
        INTEGER*4, dimension(num_attr) :: entry_nums
        REAL*4, DIMENSION(num_attr, max_dim) :: buffer
Cf2py   intent(in) fname, attr_nums, entry_nums, num_attr
Cf2py   intent(in) max_dim, num
Cf2py   intent(out) status, buffer
Cf2py   depend(num_attr) status, attr_nums, entry_nums
Cf2py   depend(fname_len) fname

        CALL true_open (fname, id, temp_status) 
        
        do j=1, num_attr              

          CALL CDF_get_attr_zentry(id, attr_nums(j),
     &entry_nums(j), buffer(j,:), status(j))
                                                        
          IF (status(j) .NE. CDF_OK) CALL StatusHandler (status(j))
        end do
        
        CALL true_close (id, temp_status)        
        return
        end 

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                                                                                                 
 
        subroutine get_multi_z_attr_int4(status, buffer, 
     &fname, attr_nums, entry_nums, num_attr, max_dim,
     &fname_len)
        INCLUDE 'cdf.inc'
        INTEGER fname_len, max_dim 
        character*(fname_len)   fname 
        ! filename to open     
        ! attr_nums : attribute numbers to pull from
        ! entry_nums: zVar numbers, called entry nums
        ! num_attr : number of attributes to pull
        ! max_dim : maximum array dimension 
        ! num : length of filename
        ! buffer : data to return
        INTEGER*4 id, j
        INTEGER*4 temp_status, num_attr
        INTEGER*4, dimension(num_attr) :: status
        INTEGER*4, dimension(num_attr) :: attr_nums
        INTEGER*4, dimension(num_attr) :: entry_nums
        INTEGER*4, DIMENSION(num_attr, max_dim) :: buffer
Cf2py   intent(in) fname, attr_nums, entry_nums, num_attr
Cf2py   intent(in) max_dim, num
Cf2py   intent(out) status, buffer
Cf2py   depend(num_attr) status, attr_nums, entry_nums
Cf2py   depend(fname_len) fname

        CALL true_open (fname, id, temp_status) 
        
        do j=1, num_attr              

          CALL CDF_get_attr_zentry(id, attr_nums(j),
     &entry_nums(j), buffer(j,:), status(j))
                                                        
          IF (status(j) .NE. CDF_OK) CALL StatusHandler (status(j))
        end do
        
        CALL true_close (id, temp_status)        
        return
        end 

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                                                                                                 
 
        subroutine get_multi_z_attr_int2(status, buffer, 
     &fname, attr_nums, entry_nums, num_attr, max_dim,
     &fname_len)
        INCLUDE 'cdf.inc'
        INTEGER fname_len, max_dim 
        character*(fname_len)   fname 
        ! filename to open     
        ! attr_nums : attribute numbers to pull from
        ! entry_nums: zVar numbers, called entry nums
        ! num_attr : number of attributes to pull
        ! max_dim : maximum array dimension 
        ! num : length of filename
        ! buffer : data to return
        INTEGER*4 id, j
        INTEGER*4 temp_status, num_attr
        INTEGER*4, dimension(num_attr) :: status
        INTEGER*4, dimension(num_attr) :: attr_nums
        INTEGER*4, dimension(num_attr) :: entry_nums
        INTEGER*2, DIMENSION(num_attr, max_dim) :: buffer
Cf2py   intent(in) fname, attr_nums, entry_nums, num_attr
Cf2py   intent(in) max_dim, num
Cf2py   intent(out) status, buffer
Cf2py   depend(num_attr) status, attr_nums, entry_nums
Cf2py   depend(fname_len) fname

        CALL true_open (fname, id, temp_status) 
        
        do j=1, num_attr              

          CALL CDF_get_attr_zentry(id, attr_nums(j),
     &entry_nums(j), buffer(j,:), status(j))
                                                        
          IF (status(j) .NE. CDF_OK) CALL StatusHandler (status(j))
        end do
        
        CALL true_close (id, temp_status)        
        return
        end 

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                                                                                                 
 
        subroutine get_multi_z_attr_int1(status, buffer, 
     &fname, attr_nums, entry_nums, num_attr, max_dim,
     &fname_len)
        INCLUDE 'cdf.inc'
        INTEGER fname_len, max_dim 
        character*(fname_len)   fname 
        ! filename to open     
        ! attr_nums : attribute numbers to pull from
        ! entry_nums: zVar numbers, called entry nums
        ! num_attr : number of attributes to pull
        ! max_dim : maximum array dimension 
        ! num : length of filename
        ! buffer : data to return
        INTEGER*4 id, j
        INTEGER*4 temp_status, num_attr
        INTEGER*4, dimension(num_attr) :: status
        INTEGER*4, dimension(num_attr) :: attr_nums
        INTEGER*4, dimension(num_attr) :: entry_nums
        INTEGER*1, DIMENSION(num_attr, max_dim) :: buffer
Cf2py   intent(in) fname, attr_nums, entry_nums, num_attr
Cf2py   intent(in) max_dim, num
Cf2py   intent(out) status, buffer
Cf2py   depend(num_attr) status, attr_nums, entry_nums
Cf2py   depend(fname_len) fname

        CALL true_open (fname, id, temp_status) 
        
        do j=1, num_attr              

          CALL CDF_get_attr_zentry(id, attr_nums(j),
     &entry_nums(j), buffer(j,:), status(j))
                                                        
          IF (status(j) .NE. CDF_OK) CALL StatusHandler(status(j))
        end do
        
        CALL true_close (id, temp_status)        
        return
        end 

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                                                                                                 
 
        subroutine get_multi_z_attr_char(status, buffer, 
     &fname, attr_nums, entry_nums, num_attr, max_dim,
     &fname_len)
        INCLUDE 'cdf.inc'
        INTEGER fname_len, max_dim 
        character*(fname_len)   fname 
        ! filename to open     
        ! attr_nums : attribute numbers to pull from
        ! entry_nums: zVar numbers, called entry nums
        ! num_attr : number of attributes to pull
        ! max_dim : maximum array dimension 
        ! num : length of filename
        ! buffer : data to return
        INTEGER*4 id, j
        INTEGER*4 temp_status, num_attr
        INTEGER*4, dimension(num_attr) :: status
        INTEGER*4, dimension(num_attr) :: attr_nums
        INTEGER*4, dimension(num_attr) :: entry_nums
        CHARACTER*(max_dim) :: buffer(num_attr)
Cf2py   intent(in) fname, attr_nums, entry_nums, num_attr
Cf2py   intent(in) max_dim, num
Cf2py   intent(out) status, buffer
Cf2py   depend(num_attr) status, attr_nums, entry_nums
Cf2py   depend(fname_len) fname

        CALL true_open (fname, id, temp_status) 
        
        do j=1, num_attr              

          CALL CDF_get_attr_zentry(id, attr_nums(j),
     &entry_nums(j), buffer(j), status(j))
                                                        
          IF (status(j) .NE. CDF_OK) CALL StatusHandler(status(j))
        end do
        
        CALL true_close (id, temp_status)        
        return
        end 

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------    

                        
        subroutine r_var_inquire(status, data_type, num_elems,
     &rec_vary, dim_varys, var_name, fname, var_num)
        INCLUDE 'cdf.inc'
        ! CDF completion status.
        INTEGER*4 status      
        ! filename to open   
        character*(*)   fname        
        
        INTEGER*4 id
        CHARACTER var_name*(CDF_VAR_NAME_LEN256)
        INTEGER*4 data_type
        INTEGER*4 num_elems, var_num
        INTEGER*4 rec_vary
        INTEGER*4 dim_varys(CDF_MAX_DIMS)

Cf2py   intent(in) fname, var_num
Cf2py   intent(out) status, data_type, num_elems, rec_vary, dim_varys, var_name

        CALL true_open (fname, id, status)

        CALL CDF_var_inquire (id, var_num, var_name, 
     &data_type, num_elems, rec_vary, dim_varys, status)
        IF (status .NE. CDF_OK) CALL StatusHandler (status)
        
        CALL true_close (id, status)

        return 
        end
              
                      
                             
                                    
C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           

        subroutine z_var_inquire(status, data_type, num_elems,
     &rec_vary, dim_varys, num_dims, dim_sizes, rec_num,var_name, 
     &fname,var_num)
        INCLUDE 'cdf.inc'
        ! CDF completion status.
        INTEGER*4 status, temp_status        
        ! filename to open  
        character*(*)   fname       
        
        INTEGER*4 id
        CHARACTER var_name*(CDF_VAR_NAME_LEN256)
        INTEGER*4 data_type
        INTEGER*4 num_elems, var_num
        INTEGER*4 rec_vary, rec_num
        INTEGER*4 dim_varys(CDF_MAX_DIMS)

        INTEGER*4 num_dims
        INTEGER*4 dim_sizes(CDF_MAX_DIMS)

Cf2py   intent(in) fname, var_num
Cf2py   intent(out) status, data_type, num_elems
Cf2py   intent(out) rec_vary, dim_varys, num_dims
Cf2py   intent(out) dim_sizes, var_name, rec_num


        CALL true_open (fname, id, temp_status)

        CALL CDF_inquire_zvar (id, var_num, var_name,
     &data_type, num_elems, num_dims, dim_sizes,
     &rec_vary, dim_varys, status)

        ! get length of variable, rec_num
        CALL CDF_get_zvar_maxwrittenrecnum (id, var_num,
     &rec_num, temp_status)
     
        IF (status .NE. CDF_OK) THEN
          IF (status .NE. NO_SUCH_VAR) CALL StatusHandler (status)
        ENDIF
        CALL true_close (id, temp_status)

        return 
        end

                                            
C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           

        subroutine z_var_all_inquire(status, data_types, num_elems,
     &rec_varys, dim_varys, num_dims, dim_sizes, rec_nums,var_nums,
     &var_names, fname, max_num, fname_len)
        INCLUDE 'cdf.inc'
        INTEGER*4 status, temp_status, max_num        
        INTEGER*4 id, fname_len, j, flag, good
        ! filename to open    
        character*(fname_len)   fname     
        
        CHARACTER*(256) var_names(max_num)
        INTEGER*4, DIMENSION(max_num) :: data_types, num_dims
        INTEGER*4, DIMENSION(max_num) :: num_elems, var_nums
        INTEGER*4, DIMENSION(max_num) :: rec_varys, rec_nums
        INTEGER*4, DIMENSION(max_num, CDF_MAX_DIMS) :: dim_varys
        INTEGER*4, DIMENSION(max_num, CDF_MAX_DIMS) :: dim_sizes

Cf2py   intent(in) fname, max_num, fname_len
Cf2py   intent(out) status, data_types, num_elems
Cf2py   intent(out) rec_varys, dim_varys, num_dims, var_names
Cf2py   intent(out) dim_sizes, var_names, rec_nums, var_nums
Cf2py   depend(fname_len) fname

        CALL true_open (fname, id, temp_status)

        flag = 1
        j = 1
        good = 1
        if (temp_status .eq. CDF_OK) then
          do while (flag .eq. 1)
            CALL CDF_inquire_zvar (id, j, var_names(good),
     &data_types(good), num_elems(good), num_dims(good), 
     &dim_sizes(good,:), rec_varys(good), dim_varys(good,:), 
     &status)

            ! get length of variable, rec_num
            if (status .eq. CDF_OK) THEN
              CALL CDF_get_zvar_maxwrittenrecnum (id, j,
     &rec_nums(good), temp_status)
            endif
     
            IF (status .NE. CDF_OK) THEN
              IF (status .NE. NO_SUCH_VAR) CALL StatusHandler (status)
            else
              var_nums(good) = j
              good = good + 1
            ENDIF
            j = j + 1
            if ( good .gt. max_num) then
              flag = 0
            end if
          end do
        CALL true_close (id, temp_status)
        endif

        return 
        end
                                                                                        
                                                                                                                                                                                
C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                         
                                                                                                                                                                           
        subroutine inquire(status, num_dims, dim_sizes, encoding, 
     &majority, max_rec, num_r_vars, num_z_vars, num_attrs, fname)
        INCLUDE 'cdf.inc'
        ! CDF completion status.
        INTEGER*4 status, temp_status    
        ! filename to open    
        character*(*)   fname         
        ! CDF identifier.
        INTEGER*4 id            

        INTEGER*4 num_dims
        INTEGER*4 dim_sizes(CDF_MAX_DIMS)
        INTEGER*4 encoding 
        INTEGER*4 majority 
        INTEGER*4 max_rec 
        INTEGER*4 num_r_vars, num_z_vars
        INTEGER*4 num_attrs

Cf2py   intent(in) fname
Cf2py   intent(out) status, num_dims, dim_sizes, encoding, majority, max_rec
Cf2py   intent(out) num_r_vars, num_z_vars, num_attrs

        CALL true_open (fname, id, temp_status)

        CALL CDF_inquire (id, num_dims, dim_sizes, encoding, majority, 
     &max_rec, num_r_vars, num_attrs, status)
        IF (status .NE. CDF_OK) CALL StatusHandler (status)

        CALL CDF_get_num_zvars (id, num_z_vars, status)
        IF (status .NE. CDF_OK) CALL StatusHandler (status) 

        CALL true_close(id, temp_status)
        
        return 
        end

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                         
 
        subroutine get_z_real4(status, buffer, fname, 
     &in_name, dim_size, rec_num)
        INCLUDE 'cdf.inc'
        character*(*)   in_name
        ! filename to open
        character*(*)   fname         
        INTEGER*4 id
        INTEGER*4 status, temp_status
        INTEGER*4 rec_num, dim_size
        REAL*4, DIMENSION(dim_size, rec_num) :: buffer
        INTEGER*4 var_num
Cf2py   intent(in) fname, in_name, dim_size, rec_num
Cf2py   intent(out) status, buffer

        CALL true_open (fname, id, temp_status)              
        ! get information about variable
        var_num = CDF_get_var_num(id, in_name)
                                                                        
        CALL CDF_get_var_allrecords_varname (id, in_name, buffer, 
     &status)
        IF (status .NE. CDF_OK) CALL StatusHandler (status)

        CALL true_close (id, temp_status)        
        return
        end 

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                         
 
        subroutine get_multi_z_real8(status, buffer, fname, 
     &in_names, dim_sizes, max_dim, max_rec, num)
        INCLUDE 'cdf.inc'
        ! filename to open 
        character*(*)   fname 
        INTEGER num, max_rec, max_dim     
        character*(256)   in_names(num)
        INTEGER*4 id, j, i1, i2
        INTEGER*4 status, temp_status
        INTEGER*4, dimension(num) :: dim_sizes
        REAL*8, DIMENSION(max_dim, max_rec) :: buffer
        INTEGER*4 var_num
Cf2py   intent(in) fname, in_names, dim_sizes, max_rec
Cf2py   intent(in) max_rec, num
Cf2py   intent(out) status, buffer
Cf2py   depend(num) dim_sizes, in_names

        CALL true_open (fname, id, temp_status) 
        
        i1 = 1
        i2 = 1
        do j=1, num              
        ! get information about variable
        var_num = CDF_get_var_num(id, in_names(j))
        i2 = i1 + dim_sizes(j) - 1
                                                                        
        CALL CDF_get_var_allrecords_varname (id, in_names(j), 
     &buffer(i1:i2,:), status)
        i1 = i2+1
        IF (status .NE. CDF_OK) CALL StatusHandler (status)
        end do
        
        CALL true_close (id, temp_status)        
        return
        end 

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                         
 
        subroutine get_multi_z_tt2000(status, buffer, fname, 
     &in_names, dim_sizes, max_dim, max_rec, num)
        INCLUDE 'cdf.inc'
        ! filename to open
        character*(*)   fname  
        INTEGER num, max_rec, max_dim     
        character*(256)   in_names(num)
        INTEGER*4 id, j, i1, i2
        INTEGER*4 status, temp_status
        INTEGER*4, dimension(num) :: dim_sizes
        REAL*8, DIMENSION(max_dim, max_rec) :: buffer
        INTEGER(KIND=KIND_INT8),DIMENSION(max_dim, max_rec)::ttbuffer
        INTEGER*4 var_num
Cf2py   intent(in) fname, in_names, dim_sizes, max_rec
Cf2py   intent(in) max_rec, num
Cf2py   intent(out) status, buffer
Cf2py   depend(num) dim_sizes, in_names

        CALL true_open (fname, id, temp_status) 
        
        i1 = 1
        i2 = 1
        do j=1, num              
        ! get information about variable
        var_num = CDF_get_var_num(id, in_names(j))
        i2 = i1 + dim_sizes(j) - 1
                                                                        
        CALL CDF_get_var_allrecords_varname (id, in_names(j), 
     &ttbuffer(i1:i2,:), status)
        i1 = i2+1
        IF (status .NE. CDF_OK) CALL StatusHandler (status)
           
        end do
        
        do j=1, max_rec
          do i=1, max_dim
            CALL TT2000_to_EPOCH(ttbuffer(i,j), buffer(i,j))
          end do
        end do
        
        CALL true_close (id, temp_status)        
        return
        end 

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                         
 
        subroutine get_multi_z_epoch16(status, buffer, fname, 
     &in_names, dim_sizes, max_dim, max_rec, num)
        INCLUDE 'cdf.inc'
        ! filename to open
        character*(*)   fname  
        INTEGER num, max_rec, max_dim     
        character*(256)   in_names(num)
        INTEGER*4 id, j, i1, i2
        INTEGER*4 status, temp_status
        INTEGER*4, dimension(num) :: dim_sizes
        REAL*8, DIMENSION(max_dim, max_rec) :: buffer
        INTEGER*4 var_num
Cf2py   intent(in) fname, in_names, dim_sizes, max_rec
Cf2py   intent(in) max_rec, num
Cf2py   intent(out) status, buffer
Cf2py   depend(num) dim_sizes, in_names

        CALL true_open (fname, id, temp_status) 
        
        i1 = 1
        i2 = 1
        do j=1, num              
          ! get information about variable
          var_num = CDF_get_var_num(id, in_names(j))
          i2 = i1 + dim_sizes(j) - 1
                                                                        
          CALL CDF_get_var_allrecords_varname (id, in_names(j), 
     &buffer(i1:i2,:), status)
          i1 = i2+1
          IF (status .NE. CDF_OK) CALL StatusHandler (status)
           
        end do
        
        
        CALL true_close (id, temp_status)        
        return
        end 


C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                         
 
        subroutine get_multi_z_real4(status, buffer, fname, 
     &in_names, dim_sizes, max_dim, max_rec, num)
        INCLUDE 'cdf.inc'
        ! filename to open
        character*(*)   fname  
        INTEGER num, max_rec, max_dim     
        character*(256)   in_names(num)
        INTEGER*4 id, j, i1, i2
        INTEGER*4 status, temp_status
        INTEGER*4, dimension(num) :: dim_sizes
        REAL*4, DIMENSION(max_dim, max_rec) :: buffer
        INTEGER*4 var_num
Cf2py   intent(in) fname, in_names, dim_sizes, max_rec
Cf2py   intent(in) max_rec, num
Cf2py   intent(out) status, buffer
Cf2py   depend(num) dim_sizes, in_names

        CALL true_open (fname, id, temp_status) 
        
        i1 = 1
        i2 = 1
        do j=1, num              
        ! get information about variable
        var_num = CDF_get_var_num(id, in_names(j))
        i2 = i1 + dim_sizes(j) - 1
                                                                        
        CALL CDF_get_var_allrecords_varname (id, in_names(j), 
     &buffer(i1:i2,:), status)
        i1 = i2+1
        IF (status .NE. CDF_OK) CALL StatusHandler (status)
        end do
        
        CALL true_close (id, temp_status)        
        return
        end 

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                         
 
                                                         
 
        subroutine get_multi_z_int4(status, buffer, fname, 
     &in_names, dim_sizes, max_dim, max_rec, num)
        INCLUDE 'cdf.inc'
        ! filename to open 
        character*(*)   fname 
        INTEGER num, max_rec, max_dim     
        character*(256)   in_names(num)
        INTEGER*4 id, j, i1, i2
        INTEGER*4 status, temp_status
        INTEGER*4, dimension(num) :: dim_sizes
        INTEGER*4, DIMENSION(max_dim, max_rec) :: buffer
        INTEGER*4 var_num
Cf2py   intent(in) fname, in_names, dim_sizes, max_rec
Cf2py   intent(in) max_rec, num
Cf2py   intent(out) status, buffer
Cf2py   depend(num) dim_sizes, in_names

        CALL true_open (fname, id, temp_status) 
        
        i1 = 1
        i2 = 1
        do j=1, num              
        ! get information about variable
        var_num = CDF_get_var_num(id, in_names(j))
        i2 = i1 + dim_sizes(j) - 1
                                                                        
        CALL CDF_get_var_allrecords_varname (id, in_names(j), 
     &buffer(i1:i2,:), status)
        i1 = i2+1
        IF (status .NE. CDF_OK) CALL StatusHandler (status)
        end do
        
        CALL true_close (id, temp_status)        
        return
        end 

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                         
 
        subroutine get_multi_z_int2(status, buffer, fname, 
     &in_names, dim_sizes, max_dim, max_rec, num)
        INCLUDE 'cdf.inc'
        ! filename to open 
        character*(*)   fname 
        INTEGER num, max_rec, max_dim     
        character*(256)   in_names(num)
        INTEGER*4 id, j, i1, i2
        INTEGER*4 status, temp_status
        INTEGER*4, dimension(num) :: dim_sizes
        INTEGER*2, DIMENSION(max_dim, max_rec) :: buffer
        INTEGER*4 var_num
Cf2py   intent(in) fname, in_names, dim_sizes, max_rec
Cf2py   intent(in) max_rec, num
Cf2py   intent(out) status, buffer
Cf2py   depend(num) dim_sizes, in_names

        CALL true_open (fname, id, temp_status) 
        
        i1 = 1
        i2 = 1
        do j=1, num              
        ! get information about variable
        var_num = CDF_get_var_num(id, in_names(j))
        i2 = i1 + dim_sizes(j) - 1
                                                                        
        CALL CDF_get_var_allrecords_varname (id, in_names(j), 
     &buffer(i1:i2,:), status)
        i1 = i2+1
        IF (status .NE. CDF_OK) CALL StatusHandler (status)
        end do
        
        CALL true_close (id, temp_status)        
        return
        end 

C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                         
 
        subroutine get_multi_z_int1(status, buffer, fname, 
     &in_names, dim_sizes, max_dim, max_rec, num)
        INCLUDE 'cdf.inc'
        ! filename to open 
        character*(*)   fname 
        INTEGER num, max_rec, max_dim     
        character*(256)   in_names(num)
        INTEGER*4 id, j, i1, i2
        INTEGER*4 status, temp_status
        INTEGER*4, dimension(num) :: dim_sizes
        INTEGER*1, DIMENSION(max_dim, max_rec) :: buffer
        INTEGER*4 var_num
Cf2py   intent(in) fname, in_names, dim_sizes, max_rec
Cf2py   intent(in) max_rec, num
Cf2py   intent(out) status, buffer
Cf2py   depend(num) dim_sizes, in_names

        CALL true_open (fname, id, temp_status) 
        
        i1 = 1
        i2 = 1
        do j=1, num              
        ! get information about variable
        var_num = CDF_get_var_num(id, in_names(j))
        i2 = i1 + dim_sizes(j) - 1
                                                                        
        CALL CDF_get_var_allrecords_varname (id, in_names(j), 
     &buffer(i1:i2,:), status)
        i1 = i2+1
        IF (status .NE. CDF_OK) CALL StatusHandler (status)
        end do
        
        CALL true_close (id, temp_status)        
        return
        end 


C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           
                                                         
 
        subroutine get_z_real8(status, buffer, fname, 
     &in_name, dim_size, rec_num)
        INCLUDE 'cdf.inc'
        character*(*)   in_name
         ! filename to open
        character*(*)   fname        
        INTEGER*4 id
        INTEGER*4 status, temp_status
        INTEGER*4 rec_num, dim_size
        REAL*8, DIMENSION(dim_size, rec_num) :: buffer
        INTEGER*4 var_num
Cf2py   intent(in) fname, in_name, dim_size, rec_num
Cf2py   intent(out) status, buffer

        CALL true_open (fname, id, temp_status)              
        ! get information about variable
        var_num = CDF_get_var_num(id, in_name)
                                                                        
        CALL CDF_get_var_allrecords_varname (id, in_name, buffer, 
     &status)
        IF (status .NE. CDF_OK) CALL StatusHandler (status)

        CALL true_close (id, temp_status)        
        return
        end 


C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------                                           





C----------------------------------------------------------------------------
C Status handler.
C----------------------------------------------------------------------------

        SUBROUTINE StatusHandler (status)
        INTEGER*4 status

        INCLUDE 'cdf.inc'

        CHARACTER message*(CDF_ERRTEXT_LEN)
	INTEGER*4 statuso

        IF (status .LT. CDF_WARN) THEN
          WRITE (6,10)
10        FORMAT (' ','Error (halting)...')
          CALL CDF_get_status_text (status, message, statuso)
          IF (statuso .EQ. CDF_OK) WRITE (6,11) message
11        FORMAT (' ',A)
C          STOP
          
        ELSE
          IF (status .LT. CDF_OK) THEN
            WRITE (6,12)
12          FORMAT (' ','Warning...')
            CALL CDF_get_status_text (status, message, statuso)
            IF (statuso .EQ. CDF_OK) WRITE (6,13) message
13          FORMAT (' ',A)
          ELSE
            IF (status .GT. CDF_OK) THEN
              WRITE (6,14)
14            FORMAT (' ','Be advised that...')
              CALL CDF_get_status_text (status, message, statuso)
              IF (statuso .EQ. CDF_OK) WRITE (6,15) message
15            FORMAT (' ',A)
            END IF
          END IF
        END IF

        RETURN
        END

        SUBROUTINE StatusReporter (message, status)
        INTEGER*4 status

        INCLUDE 'cdf.inc'

        CHARACTER message*(CDF_ERRTEXT_LEN)
	INTEGER*4 statuso
Cf2py   intent(in) status
Cf2py   intent(out) message

        CALL CDF_get_status_text (status, message, statuso)
        
        RETURN
        END
