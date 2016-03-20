#------------------------------------------------------------------------------
#
#  NSSDC/CDF					Definitions for using CDF.
#						Korn, Bash, and posixSHELL.
#
#  Version 1.2, 19-Mar-96, Hughes STX.
#
#  Modification History:
#
#   V1.0  14-Jun-95, J Love	Original version.
#   V1.1  18-Jul-95, J Love	Added CDFexport.
#   V1.2  19-Mar-96, J Love	CDF V2.6.
#   V1.3  05-Aug-08, M Liu      CDF V3.3.
#   V1.4  12-Apr-11, M Liu      CDF V3.3.2 for adding leap seconds env. var.
#
#-----------------------------------------------------------------------------
# Before using this script file, change the definition of CDF_BASE for where
# CDF was installed on your machine (ie. the directory containing the `bin',
# `lib', and `include' directories).  All CDF users can then use the command...
#
#	$ . <path>/bin/definitions.K
#
# ...to setup the necessary environment variables and aliases (where <path>
# is the full directory path leading to where CDF was installed).  Note that
# `<path>/bin' could also be added to a user's path.  
#-----------------------------------------------------------------------------

export CDF_BASE=<cdf_install_dir>

export CDF_INC=$CDF_BASE/include
export CDF_LIB=$CDF_BASE/lib
export CDF_BIN=$CDF_BASE/bin
export CDF_JAVA=$CDF_BASE/cdfjava
export CDF_HELP=$CDF_BASE/lib/cdf/help
export CDF_LEAPSECONDSTABLE=$CDF_BASE/CDFLeapSeconds.txt

alias cdfedit=$CDF_BIN/cdfedit
alias cdfexport=$CDF_BIN/cdfexport
alias cdfconvert=$CDF_BIN/cdfconvert
alias cdfinquire=$CDF_BIN/cdfinquire
alias cdfstats=$CDF_BIN/cdfstats
alias cdfdump=$CDF_BIN/cdfdump
alias cdfirsdump=$CDF_BIN/cdfirsdump
alias cdfcompare=$CDF_BIN/cdfcompare
alias skeletontable=$CDF_BIN/skeletontable
alias skeletoncdf=$CDF_BIN/skeletoncdf
alias cdfdir=$CDF_BIN/cdfdir
alias cdfmerge=$CDF_BIN/cdfmerge
alias cdfbrowse="$CDF_BIN/cdfedit -prompt -browse"
alias cdfvalidate=$CDF_BIN/cdfvalidate
alias cdfleapsecondsinfo=$CDF_BIN/cdfleapsecondsinfo
# The following statements are relevant only for Mac OS X
if [ "$OSTYPE" = "darwin" ]
then
  alias cdf-to-netCDF=$CDF_BIN/cdf-to-netCDF
  alias netCDF-to-cdf=$CDF_BIN/netCDF-to-cdf
fi
