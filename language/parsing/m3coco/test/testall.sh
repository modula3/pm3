echo >test.log
TESTROOT=$PWD
for dir in sgml f2i3 expr misc taste/xref taste/compiler taste/pretty; do \
  (echo "---------- processing" $dir "-------------"; \
  cd $dir ; \
  # rm -r LINUXLIBC6 ; \
  cm3 -clean
  cm3 >>$TESTROOT/test.log ; \
  diff -x libsgml.a -rq /tmp/m3cocotest/$dir .) ; \
done
