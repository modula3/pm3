#! /bin/sh

p=$1

if test -d ${p}/src
  then if test \( `ls ${p} | wc -l` -gt 1 \) -a -f ${p}/src/m3makefile -a \
      \( `grep OtherPackage ${p}/src/m3makefile | wc -l` -ge 1 \) -a \
      \! -d ${p}/gcc -a \! -d ${p}/gdb
    then echo Package ${p} in src
    SEARCH=T
  else echo Package ${p} complete
    SEARCH=F
  fi
else
  echo No package in ${p}
  SEARCH=T
fi

if test ${SEARCH} = T
  then for i in `ls ${p}`
    do if test $i \!= 'src'
      then $0 ${p}/${i}
    fi
  done
fi
