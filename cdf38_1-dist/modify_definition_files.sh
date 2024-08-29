#! /bin/bash
search="<cdf_install_dir>"
replace=$1   # Current working directory
whichos=`uname`
case "$whichos" in
  CYGWIN*) topath=1;;
  MINGW*) topath=1;;
  Darwin*) topath=2;;
  * ) topath=0;;
esac

for file in `ls $replace/bin/definitions.*`
do
    extension="${file:${#file}-1}"
    if [ $topath -eq 1 ]; then
      if [ $extension == "B" ] || [ $extension == "K" ]; then
        echo "export PATH=\"$CDF_BASE/lib:\$PATH\"" >> $file
      else
        echo "setenv PATH \"$CDF_BASE/lib:\$PATH\"" >> $file 
      fi
    elif [ $topath -eq 2 ]; then
      if [ $extension == "B" ] || [ $extension == "K" ]; then
        echo "if [ -z \$DYLD_LIBRARY_PATH ]; then" >> $file
        echo "  export DYLD_LIBRARY_PATH=\$CDF_BASE/lib" >> $file
        echo "else"  >> $file
        echo "  LDY=\`printenv DYLD_LIBRARY_PATH\`" >> $file
        echo "  if ! [[ \$LDY =~ \"\$CDF_BASE/lib\" ]]; then" >> $file
        echo "    export DYLD_LIBRARY_PATH=\$CDF_BASE/lib:\$DYLD_LIBRARY_PATH" >> $file
        echo "  fi" >> $file
        echo "fi" >> $file
      else
        echo "if \$?DYLD_LIBRARY_PATH then" >> $file
        echo "  set LDY=\`printenv DYLD_LIBRARY_PATH\`" >> $file
        echo "  if ( \$LDY !~ \*\${CDF_BASE}/lib\* ) then" >> $file
        echo "    setenv DYLD_LIBRARY_PATH \$CDF_BASE/lib:\$DYLD_LIBRARY_PATH" >> $file
        echo "  endif" >> $file
        echo "else" >> $file
        echo "  setenv DYLD_LIBRARY_PATH \$CDF_BASE/lib"  >> $file
        echo "endif" >> $file
      fi
    else
      if [ $extension == "B" ] || [ $extension == "K" ]; then
        echo "if [ -z \$LD_LIBRARY_PATH ]; then" >> $file
        echo "  export LD_LIBRARY_PATH=\$CDF_BASE/lib" >> $file
        echo "else"  >> $file
        echo "  LDY=\`printenv LD_LIBRARY_PATH\`" >> $file
        echo "  if ! [[ \$LDY =~ \"\$CDF_BASE/lib\" ]]; then" >> $file
        echo "    export LD_LIBRARY_PATH=\$CDF_BASE/lib:\$LD_LIBRARY_PATH" >> $file
        echo "  fi" >> $file
        echo "fi" >> $file
      else
        echo "if \$?LD_LIBRARY_PATH then" >> $file
        echo "  set LDY=\`printenv LD_LIBRARY_PATH\`" >> $file
        echo "  if ( \$LDY !~ \*\$CDF_BASE/lib\* ) then" >> $file
        echo "    setenv LD_LIBRARY_PATH \$CDF_BASE/lib:\$LD_LIBRARY_PATH" >> $file
        echo "  endif" >> $file
        echo "else" >> $file
        echo "  setenv LD_LIBRARY_PATH \$CDF_BASE/lib"  >> $file
        echo "endif" >> $file
      fi
    fi

    echo "Modifying the definition file $file .."
    if `type ed > /dev/null 2>&1`
    then
      ed - $file << editend
      1,\$s:$search:$replace:g
      w
      q
editend
    elif `type sed > /dev/null 2>&1`
      then
        sed -e "s,$search,$replace,g" < $file > $file.new
        mv $file.new $file
    else
        echo Not modified... as both "ed" and "sed" line editor not installed.
        echo Use any available text editor to manually modify "$search" to "$replace".
    fi
done

