#/bin/bash

# preprocess a FORTRAN file from LAPACK
# for converting using f2i3

file=$1
tmp=/tmp/tmp.f

#  this replaces newlines by newlines
#sed "N
#s/\n/\\
#/
#" $file

# FORTRAN uses '$' to mark the beginning of a line
# that is a continuation of the preceding one.
# We want to join all line parts onto one line
# since this simplifys later parsing.
# The simplest thing would be to remove all occurences
# of the regular expression '\n *\$'.
# But neither 'perl' nor 'sed' support this immediately.
# Though, processing multi-line patterns with 'sed' is hard
# but possible:
# 'sed' allows for joining two successive lines implicitly,
# that is 0,1->0; 2,3->2; 4,5->3
# In a first step we can process all continued even lines (first 'sed')
# then we prepend a blank line ('echo') and
# and process the result again with 'sed' which now operates on the
# lines that had odd numbers before.
# The last step is to remove all blank lines using inverse 'grep' for blank lines.
# Caution: In the first 'sed' invokation
# we cannot remove the line feed completely
# since we would damage the parity.
# Instead we insert a blank line after each joined one.
(echo;
sed -e N -e "s/\n *[$]\(.*\)/\1\\
/" $file) | \
sed -e N -e "s/\n *[$]/\1/" | \
grep -v "^$" >$tmp

# throw away all comments that does not contain information
# that is automatically retrievable
perl -i -p -e "s/(\* +\w+ +\(.*?(input|output|workspace).*?\)).*(\n)|(\*.*\n)|(.*\n)/\1\3\5/" $tmp

# the last usable comment marks the end of the interface definitions
# find its line number
#    grep - list all comments with line numbers
#    tail - choose the last line
#    perl - extract the line number
num=`grep -n "^*" $tmp | tail -n 1 | perl -p -e "s/(\d+).*/\1/"`

# we will throw away the lines below
# f2i3 needs an extra character for grateful exit
(head -n $num $tmp;echo) | f2i3 2>/dev/null
