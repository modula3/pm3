# See the file LICENSE for redistribution information.
#
# Copyright (c) 1999-2002
#	Sleepycat Software.  All rights reserved.
#
# $Id$
#
# TEST	env006
# TEST	Make sure that all the utilities exist and run.
proc env006 { } {
	source ./include.tcl

	puts "Env006: Run underlying utilities."

	set rlist {
	{ "db_archive"			"Env006.a"}
	{ "db_checkpoint"		"Env006.b"}
	{ "db_deadlock"			"Env006.c"}
	{ "db_dump"			"Env006.d"}
	{ "db_load"			"Env006.e"}
	{ "db_printlog"			"Env006.f"}
	{ "db_recover"			"Env006.g"}
	{ "db_stat"			"Env006.h"}
	{ "db_upgrade"			"Env006.h"}
	{ "db_verify"			"Env006.h"}
	}
	foreach pair $rlist {
		set cmd [lindex $pair 0]
		set msg [lindex $pair 1]

		puts "\t$msg: $cmd"

		set stat [catch {exec $util_path/$cmd -?} ret]
		error_check_good $cmd $stat 1

		#
		# Check for "usage", but only check "sage" so that
		# we can handle either Usage or usage.
		#
		error_check_good $cmd.err [is_substr $ret sage] 1
	}
}
