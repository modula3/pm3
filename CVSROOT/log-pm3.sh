#!/bin/sh

echo "------------------------------------------------------------------------------" > /usr/tmp/log.$$
date >> /usr/tmp/log.$$
cat - >> /usr/tmp/log.$$
cat /usr/tmp/log.$$ >> $CVSROOT/CVSROOT/commitlog-pm3
cat /usr/tmp/log.$$ | /usr/bin/Mail -s "PM3 CVS commitlog `date`" wagner

rm /usr/tmp/log.$$
