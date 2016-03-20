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
#      extension=`echo $file | cut -f2 -d"."`
       extension="${file:${#file}-1}"
       if [ $topath -eq 1 ]; then
         if [ $extension == "B" ] || [ $extension == "K" ]; then
           echo "export PATH=\"$search/lib:$PATH\"" >> $file
         else
           echo "setenv PATH \"$search/lib:$PATH\"" >> $file 
         fi
       elif [ $topath -eq 2 ]; then
         if [ $extension == "B" ] || [ $extension == "K" ]; then
             echo "export DYLD_LIBRARY_PATH=$search/lib" >> $file
         else
             echo "setenv DYLD_LIBRARY_PATH $search/lib" >> $file
         fi
       else
         if [ $extension == "B" ] || [ $extension == "K" ]; then
             echo "export LD_LIBRARY_PATH=$search/lib" >> $file
         else
             echo "setenv LD_LIBRARY_PATH $search/lib" >> $file
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

