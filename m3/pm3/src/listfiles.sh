#! /bin/sh
#
# $1: file containing the list
# $2: current prefix for files
# $3: desired prefix for files

grep install_file $1 | sed -e "s+install_file(\"\([^\"]*\)\",[ ]*\"$2\([^\"]*\)\".*+/\1 \2+" | sed -e "s+^.*/\([^/]*\) \(.*\)+$3\2/\1+" | sort | uniq
grep link_file $1 | sed -e "s+link_file(\"\([^\"]*\)\",[ ]*\"$2\([^\"]*\)\".*+$3\2+"
