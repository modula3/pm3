src=$1
dst=$2

# duplicate directory structure
#   run this once
#(cd $src; \
#  for dir in `find . -type d`; do \
#    echo $dir; \
#    mkdir $dst/$dir; \
#  done )

# test m3pp on all Modula files we can find
(cd $src; \
  for file in `find . -name "*.[im][3g]"`; do \
    echo $file; \
    m3pp $file >$dst/$file; \
  done )
