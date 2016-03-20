C------------------------------------------------------------------------------
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
C
C  NSSDC/CDF				Microsoft FORTRAN-specific include
C					file for FORTRAN interface.
C
C  Version 1.2, 9-Sep-96, Hughes STX.
C
C  Modification history:
C
C   V1.0   9-Mar-93, J Love     Original version.
C   V1.1  22-Nov-94, J Love	CDF V2.5.
C   V1.2   9-Sep-96, J Love	CDF V2.6.
C   V1.3  08-Apr-04, M Liu      Added  new data type CDF_EPOCH16 and some
C                               epoch functions related to the new type.
C
C------------------------------------------------------------------------------

	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB
     .                                  [ALIAS:'_cdf_lib',VARYING]
     .                                  ()
	END

	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_4
     .                                  [ALIAS:'_cdf_lib_4',VARYING]
     .                                  ()
	END

	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_5
     .                                  [ALIAS:'_cdf_lib_5',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_6
     .                                  [ALIAS:'_cdf_lib_6',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_7
     .                                  [ALIAS:'_cdf_lib_7',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_8
     .                                  [ALIAS:'_cdf_lib_8',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_9
     .                                  [ALIAS:'_cdf_lib_9',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_10
     .                                  [ALIAS:'_cdf_lib_10',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_11
     .                                  [ALIAS:'_cdf_lib_11',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_12
     .                                  [ALIAS:'_cdf_lib_12',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_13
     .                                  [ALIAS:'_cdf_lib_13',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_14
     .                                  [ALIAS:'_cdf_lib_14',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_15
     .                                  [ALIAS:'_cdf_lib_15',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_16
     .                                  [ALIAS:'_cdf_lib_16',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_17
     .                                  [ALIAS:'_cdf_lib_17',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_18
     .                                  [ALIAS:'_cdf_lib_18',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_19
     .                                  [ALIAS:'_cdf_lib_19',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_20
     .                                  [ALIAS:'_cdf_lib_20',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_21
     .                                  [ALIAS:'_cdf_lib_21',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_22
     .                                  [ALIAS:'_cdf_lib_22',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_23
     .                                  [ALIAS:'_cdf_lib_23',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_24
     .                                  [ALIAS:'_cdf_lib_24',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_LIB_25
     .                                  [ALIAS:'_cdf_lib_25',VARYING]
     .                                  ()
	END
	
	INTERFACE TO SUBROUTINE CDF_OPEN
     .                          [ALIAS:'_cdf_open',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_CREATE
     .                          [ALIAS:'_cdf_create',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_INQUIRE
     .                          [ALIAS:'_cdf_inquire',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_DOC
     .                          [ALIAS:'_cdf_doc',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_CLOSE
     .                          [ALIAS:'_cdf_close',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_DELETE
     .                          [ALIAS:'_cdf_delete',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_VAR_CREATE
     .                          [ALIAS:'_cdf_var_create',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_VAR_INQUIRE
     .                          [ALIAS:'_cdf_var_inquire',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_VAR_RENAME
     .                          [ALIAS:'_cdf_var_rename',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_VAR_GET
     .                          [ALIAS:'_cdf_var_get',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_VAR_PUT
     .                          [ALIAS:'_cdf_var_put',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_VAR_HYPER_GET
     .                          [ALIAS:'_cdf_var_hyper_get',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_VAR_HYPER_PUT
     .                          [ALIAS:'_cdf_var_hyper_put',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_VAR_CLOSE
     .                          [ALIAS:'_cdf_var_close',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_ATTR_CREATE
     .                          [ALIAS:'_cdf_attr_create',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_ATTR_INQUIRE
     .                          [ALIAS:'_cdf_attr_inquire',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_ATTR_RENAME
     .                          [ALIAS:'_cdf_attr_rename',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_ATTR_CORRECT_SCOPE
     .                          [ALIAS:'_cdf_attr_correct_scope',
     .				 VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_ATTR_ENTRY_INQUIRE
     .                          [ALIAS:'_cdf_attr_entry_inquire',
     .				 VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_ATTR_GET
     .                          [ALIAS:'_cdf_attr_get',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_ATTR_PUT
     .                          [ALIAS:'_cdf_attr_put',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE CDF_ERROR
     .                          [ALIAS:'_cdf_error',VARYING]
     .                          ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_VAR_NUM
     .                                  [ALIAS:'_cdf_var_num',VARYING]
     .                                  ()
	END
	
	INTERFACE TO INTEGER*4 FUNCTION CDF_ATTR_NUM
     .                                  [ALIAS:'_cdf_attr_num',VARYING]
     .                                  ()
	END

	INTERFACE TO SUBROUTINE EPOCH_BREAKDOWN
     .                          [ALIAS:'_epoch_breakdown',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE COMPUTE_EPOCH
     .                          [ALIAS:'_compute_epoch',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE PARSE_EPOCH
     .                          [ALIAS:'_parse_epoch',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE PARSE_EPOCH1
     .                          [ALIAS:'_parse_epoch1',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE PARSE_EPOCH2
     .                          [ALIAS:'_parse_epoch2',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE PARSE_EPOCH3
     .                          [ALIAS:'_parse_epoch3',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE ENCODE_EPOCH
     .                          [ALIAS:'_encode_epoch',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE ENCODE_EPOCH1
     .                          [ALIAS:'_encode_epoch1',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE ENCODE_EPOCH2
     .                          [ALIAS:'_encode_epoch2',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE ENCODE_EPOCH3
     .                          [ALIAS:'_encode_epoch3',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE ENCODE_EPOCHX
     .                          [ALIAS:'_encode_epochx',VARYING]
     .                          ()

	INTERFACE TO SUBROUTINE EPOCH16_BREAKDOWN
     .                       [ALIAS:'_epoch16_breakdown',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE COMPUTE_EPOCH16
     .                          [ALIAS:'_compute_epoch16',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE PARSE_EPOCH16
     .                          [ALIAS:'_parse_epoch16',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE PARSE_EPOCH16_1
     .                          [ALIAS:'_parse_epoch16_1',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE PARSE_EPOCH16_2
     .                          [ALIAS:'_parse_epoch16_2',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE PARSE_EPOCH16_3
     .                          [ALIAS:'_parse_epoch16_3',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE ENCODE_EPOCH16
     .                          [ALIAS:'_encode_epoch16',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE ENCODE_EPOCH16_1
     .                          [ALIAS:'_encode_epoch16_1',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE ENCODE_EPOCH16_2
     .                          [ALIAS:'_encode_epoch16_2',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE ENCODE_EPOCH16_3
     .                          [ALIAS:'_encode_epoch16_3',VARYING]
     .                          ()
	END
	
	INTERFACE TO SUBROUTINE ENCODE_EPOCH16_X
     .                          [ALIAS:'_encode_epoch16_x',VARYING]
     .                          ()
	END
        INTERFACE TO SUBROUTINE CDF_GETRVARSRECORDDATA
     .                        [ALIAS:'_cdf_getrvarsrecorddata',VARYING]
     .                          ()
        END

        INTERFACE TO SUBROUTINE CDF_PUTRVARSRECORDDATA
     .                        [ALIAS:'_cdf_putrvarsrecorddata',VARYING]
     .                          ()
        END

        INTERFACE TO SUBROUTINE CDF_GETZVARSRECORDDATA
     .                        [ALIAS:'_cdf_getzvarsrecorddata',VARYING]
     .                          ()
        END

        INTERFACE TO SUBROUTINE CDF_PUTZVARSRECORDDATA
     .                        [ALIAS:'_cdf_putzvarsrecorddata',VARYING]
     .                          ()
        END
