#! /bin/sh

EXPORT_TMP_BINARIES="/home/ftp/tmp/pm3/binaries"
EXPORT_TMP_BOOTSTRAP="/home/ftp/tmp/pm3/bootstrap"
EXPORT_BINARIES="/home/ftp/pub/m3/binaries"
cd ${EXPORT_TMP_BINARIES}

for i in *
  do if test -f ${i}/DONE -a -f ${EXPORT_TMP_BOOTSTRAP}/${i}/TODO
    then if test `cat ${i}/DONE` = `cat ${EXPORT_TMP_BOOTSTRAP}/${i}/TODO` \
             -a -f ${i}/`cat ${i}/DONE`.tgz
      then (cd ${i}; tar -z -x -p --exclude '/*' --exclude '*../*' \
             --exclude '*/..*' -f `cat DONE`.tgz)
      mv ${i}/`cat ${i}/DONE` ${EXPORT_BINARIES}/${i}
      rm ${EXPORT_TMP_BOOTSTRAP}/${i}/TODO
      rm ${i}/`cat ${i}/DONE`.tgz
      rm ${i}/DONE
    fi
  fi
done
