#! /bin/sh

ARCH=`arch`
OS=`uname -s`
REV=`uname -r`
ZZP=`cat zzp`

SKIP_DOWNLOAD="F"
SKIP_BUILD="F"
SKIP_UPLOAD="F"
SKIP_CLEAN="T"

case ${ARCH}-${OS}-${REV} in
  i[3456789]86-Linux-*)
    TARGET=LINUXELF
  ;;
  i[3456789]86-cygwin32-*)
    TARGET=NT386GNU
  ;;
  sun4-SunOS-4*)
    TARGET=SPARC
  ;;
  sun4-SunOS-[56]*)
    TARGET=SOLgnu
  ;;
  *)
  TARGET=UNKNOWN
  ;;
esac

#
# Download the TODO/DONE, check, download the source and bootstrap,
# untar the files.
#

if test ${SKIP_DOWNLOAD} = "F"
  then rm TODO DONE
  ftp -n <<EOF
open m3.polymtl.ca
user ftp ${TARGET}@m3.polymtl.ca
cd /tmp/pm3/bootstrap/${TARGET}
get TODO
cd /tmp/pm3/binaries/${TARGET}
get DONE
EOF
fi

if test ! -f TODO || ( test -f DONE && test `cat TODO` = `cat DONE` )
  then echo "nothing to do"
  exit 0
fi

VERSION=`cat TODO`
echo building ${VERSION}

if test ${SKIP_DOWNLOAD} = "F"
  then rm -r ${TARGET}
  mkdir ${TARGET}
  ( 
  cd ${TARGET}
  ftp -n <<EOF
open m3.polymtl.ca
user ftp ${TARGET}@m3.polymtl.ca
binary
cd /pub/m3/bootstrap/${TARGET}
get ${VERSION}.tgz ${VERSION}-boot.tgz
cd /pub/m3/pkg
get ${VERSION}.tgz
EOF

  gunzip ${VERSION}.tgz
  tar -x -p -f ${VERSION}.tar
  rm ${VERSION}.tar
  gunzip ${VERSION}-boot.tgz
  tar -x -p -f ${VERSION}-boot.tar
  rm ${VERSION}-boot.tar
  )

fi

#
# Try to build it.
#

if test ${SKIP_BUILD} = "F"
  then LD_LIBRARY_PATH=`pwd`/${TARGET}/${VERSION}/binaries/${TARGET}/usr/local/pm3/lib/m3/${TARGET}:${LD_LIBRARY_PATH}
  export LD_LIBRARY_PATH

  if (cd ${TARGET}/${VERSION} && make exportall && cd binaries && mv ${TARGET}\
      ${VERSION} && tar cf ../../../${VERSION}.tar ${VERSION}) \
      1> out.tmp 2>errs.tmp
    then rm -r ${TARGET}
    gzip ${VERSION}.tar
    mv ${VERSION}.tar.gz ${VERSION}.tgz
    COMPLETED="T"
  else
    COMPLETED="F"
  fi
fi

#
# Now, upload the result
#

if test ${SKIP_UPLOAD} = "F"
  then if test ${COMPLETED} = "T"
    then ftp -n <<EOF
open m3.polymtl.ca
user ftp ${TARGET}@m3.polymtl.ca
quote site group binaries
quote site gpass ${ZZP}
binary
cd /tmp/pm3/binaries/${TARGET}
put ${VERSION}.tgz
put TODO DONE
EOF
  else
    ftp -n <<EOF
open m3.polymtl.ca
user ftp ${TARGET}@m3.polymtl.ca
quote site group binaries
quote site gpass ${ZZP}
binary
cd /tmp/pm3/binaries/${TARGET}
put errs.tmp NOTDONE.errs
put out.tmp NOTDONE.out
EOF

  fi
fi

if test ${SKIP_CLEAN} = "F"
  then rm ${VERSION}.tar ${VERSION}.tgz
fi

