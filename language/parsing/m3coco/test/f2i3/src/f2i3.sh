#/bin/bash

# preprocess a FORTRAN file from LAPACK
# for converting using f2i3

file=$1
tmp=/tmp/tmp.f

# throw away all comments that does not contain information
# that is automatically retrievable
perl -p -e "s/(\* +\w+ +\(.*?(input|output|workspace).*?\)).*(\n)|(\*.*\n)|(.*\n)/\1\3\5/" $file >$tmp

# the last usable comment marks the end of the interface definitions
# find its line number
#    grep - list all comments with line numbers
#    tail - choose the last line
#    perl - extract the line number
num=`grep -n "^*" $tmp | tail -n 1 | perl -p -e "s/(\d+).*/\1/"`

# we will throw away the lines below
head -n $num $tmp
