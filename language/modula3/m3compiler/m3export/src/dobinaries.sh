#! /bin/sh

ARCH=`arch`
OS=`uname -s`
REV=`uname -r`
ZZP=`cat zzp`

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

rm TODO DONE
ftp -n <<EOF
open m3.polymtl.ca
user ftp ${TARGET}@m3.polymtl.ca
cd /tmp/pm3/bootstrap/${TARGET}
get TODO
cd /tmp/pm3/binaries/${TARGET}
get DONE
EOF

if test ! -f TODO || ( test -f DONE && test `cat TODO` = `cat DONE` )
  then echo "nothing to do"
  exit 0
fi

VERSION=`cat TODO`
rm -r ${TARGET}
mkdir ${TARGET}
echo building ${VERSION}

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

if (cd ${TARGET}/${VERSION} && make exportall && cd binaries && mv ${TARGET} \
    ${VERSION} && tar cf ../../../${VERSION}.tar ${VERSION}) \
    1> out.tmp 2>errs.tmp
  then rm -r ${TARGET}
  gzip ${VERSION}.tar
  mv ${VERSION}.tar.gz ${VERSION}.tgz
ftp -n <<EOF
open m3.polymtl.ca
user ftp ${TARGET}@m3.polymtl.ca
site group binaries
site gpass ${ZZP}
binary
cd /tmp/pm3/binaries/${TARGET}
put ${VERSION}.tgz
put TODO DONE
EOF
  else
ftp -n <<EOF
open m3.polymtl.ca
user ftp ${TARGET}@m3.polymtl.ca
site group binaries
site gpass ${ZZP}
binary
cd /tmp/pm3/binaries/${TARGET}
put errs.tmp NOTDONE.errs
put out.tmp NOTDONE.out
EOF

fi

rm ${VERSION}.tar ${VERSION}.tgz

