#! /bin/sh

EXPORT_TMP_BINARIES="/tmp/pm3/binaries"
EXPORT_TMP_BOOTSTRAP="/tmp/pm3/bootstrap"
EXPORT_BINARIES="/web/m3/binaries"
cd EXPORT_TMP_BINARIES

for i in .
  do if test -f ${i}/DONE -a test -f ${EXPORT_TMP_BOOTSTRAP}/${i}/TODO \
             -a `cat ${i}/DONE` = `cat ${EXPORT_TMP_BOOTSTRAP}/${i}/TODO`
    then cp -a ${i}/`cat ${i}/DONE` ${EXPORT_BINARIES}/${i}
    rm ${EXPORT_TMP_BOOTSTRAP}/${i}/TODO
    rm ${i}/DONE
  fi
done
