# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996-2002
#	Sleepycat Software.  All rights reserved.
#
# $Id$
#
# Test system utilities
#
# Timestamp -- print time along with elapsed time since last invocation
# of timestamp.
proc timestamp {{opt ""}} {
	global __timestamp_start

	set now [clock seconds]

	# -c	accurate to the click, instead of the second.
	# -r	seconds since the Epoch
	# -t	current time in the format expected by db_recover -t.
	# -w	wallclock time
	# else	wallclock plus elapsed time.
	if {[string compare $opt "-r"] == 0} {
		return $now
	} elseif {[string compare $opt "-t"] == 0} {
		return [clock format $now -format "%y%m%d%H%M.%S"]
	} elseif {[string compare $opt "-w"] == 0} {
		return [clock format $now -format "%c"]
	} else {
		if {[string compare $opt "-c"] == 0} {
			set printclicks 1
		} else {
			set printclicks 0
		}

		if {[catch {set start $__timestamp_start}] != 0} {
			set __timestamp_start $now
		}
		set start $__timestamp_start

		set elapsed [expr $now - $start]
		set the_time [clock format $now -format ""]
		set __timestamp_start $now

		if { $printclicks == 1 } {
			set pc_print [format ".%08u" [__fix_num [clock clicks]]]
		} else {
			set pc_print ""
		}

		format "%02d:%02d:%02d$pc_print (%02d:%02d:%02d)" \
		    [__fix_num [clock format $now -format "%H"]] \
		    [__fix_num [clock format $now -format "%M"]] \
		    [__fix_num [clock format $now -format "%S"]] \
		    [expr $elapsed / 3600] \
		    [expr ($elapsed % 3600) / 60] \
		    [expr ($elapsed % 3600) % 60]
	}
}

proc __fix_num { num } {
	set num [string trimleft $num "0"]
	if {[string length $num] == 0} {
		set num "0"
	}
	return $num
}

# Add a {key,data} pair to the specified database where
# key=filename and data=file contents.
proc put_file { db txn flags file } {
	source ./include.tcl

	set fid [open $file r]
	fconfigure $fid -translation binary
	set data [read $fid]
	close $fid

	set ret [eval {$db put} $txn $flags {$file $data}]
	error_check_good put_file $ret 0
}

# Get a {key,data} pair from the specified database where
# key=filename and data=file contents and then write the
# data to the specified file.
proc get_file { db txn flags file outfile } {
	source ./include.tcl

	set fid [open $outfile w]
	fconfigure $fid -translation binary
	if [catch {eval {$db get} $txn $flags {$file}} data] {
		puts -nonewline $fid $data
	} else {
		# Data looks like {{key data}}
		set data [lindex [lindex $data 0] 1]
		puts -nonewline $fid $data
	}
	close $fid
}

# Add a {key,data} pair to the specified database where
# key=file contents and data=file name.
proc put_file_as_key { db txn flags file } {
	source ./include.tcl

	set fid [open $file r]
	fconfigure $fid -translation binary
	set filecont [read $fid]
	close $fid

	# Use not the file contents, but the file name concatenated
	# before the file contents, as a key, to ensure uniqueness.
	set data $file$filecont

	set ret [eval {$db put} $txn $flags {$data $file}]
	error_check_good put_file $ret 0
}

# Get a {key,data} pair from the specified database where
# key=file contents and data=file name
proc get_file_as_key { db txn flags file} {
	source ./include.tcl

	set fid [open $file r]
	fconfigure $fid -translation binary
	set filecont [read $fid]
	close $fid

	set data $file$filecont

	return [eval {$db get} $txn $flags {$data}]
}

# open file and call dump_file to dumpkeys to tempfile
proc open_and_dump_file {
    dbname env outfile checkfunc dump_func beg cont } {
	global encrypt
	global passwd
	source ./include.tcl

	set encarg ""
	if { $encrypt > 0 && $env == "NULL" } {
		set encarg "-encryptany $passwd"
	}
	set envarg ""
	set txn ""
	set txnenv 0
	if { $env != "NULL" } {
		append envarg " -env $env "
		set txnenv [is_txnenv $env]
		if { $txnenv == 1 } {
			append envarg " -auto_commit "
			set t [$env txn]
			error_check_good txn [is_valid_txn $t $env] TRUE
			set txn "-txn $t"
		}
	}
	set db [eval {berkdb open} $envarg -rdonly -unknown $encarg $dbname]
	error_check_good dbopen [is_valid_db $db] TRUE
	$dump_func $db $txn $outfile $checkfunc $beg $cont
	if { $txnenv == 1 } {
		error_check_good txn [$t commit] 0
	}
	error_check_good db_close [$db close] 0
}

# open file and call dump_file to dumpkeys to tempfile
proc open_and_dump_subfile {
    dbname env outfile checkfunc dump_func beg cont subdb} {
	global encrypt
	global passwd
	source ./include.tcl

	set encarg ""
	if { $encrypt > 0 && $env == "NULL" } {
		set encarg "-encryptany $passwd"
	}
	set envarg ""
	set txn ""
	set txnenv 0
	if { $env != "NULL" } {
		append envarg "-env $env"
		set txnenv [is_txnenv $env]
		if { $txnenv == 1 } {
			append envarg " -auto_commit "
			set t [$env txn]
			error_check_good txn [is_valid_txn $t $env] TRUE
			set txn "-txn $t"
		}
	}
	set db [eval {berkdb open -rdonly -unknown} \
	    $envarg $encarg {$dbname $subdb}]
	error_check_good dbopen [is_valid_db $db] TRUE
	$dump_func $db $txn $outfile $checkfunc $beg $cont
	if { $txnenv == 1 } {
		error_check_good txn [$t commit] 0
	}
	error_check_good db_close [$db close] 0
}

# Sequentially read a file and call checkfunc on each key/data pair.
# Dump the keys out to the file specified by outfile.
proc dump_file { db txn outfile checkfunc } {
	source ./include.tcl

	dump_file_direction $db $txn $outfile $checkfunc "-first" "-next"
}

proc dump_file_direction { db txn outfile checkfunc start continue } {
	source ./include.tcl

	# Now we will get each key from the DB and dump to outfile
	set c [eval {$db cursor} $txn]
	error_check_good db_cursor [is_valid_cursor $c $db] TRUE
	dump_file_walk $c $outfile $checkfunc $start $continue
	error_check_good curs_close [$c close] 0
}

proc dump_file_walk { c outfile checkfunc start continue {flag ""} } {
	set outf [open $outfile w]
	for {set d [eval {$c get} $flag $start] } \
	    { [llength $d] != 0 } \
	    {set d [eval {$c get} $flag $continue] } {
		set kd [lindex $d 0]
		set k [lindex $kd 0]
		set d2 [lindex $kd 1]
		$checkfunc $k $d2
		puts $outf $k
		# XXX: Geoff Mainland
		# puts $outf "$k $d2"
	}
	close $outf
}

proc dump_binkey_file { db txn outfile checkfunc } {
	source ./include.tcl

	dump_binkey_file_direction $db $txn $outfile $checkfunc \
	    "-first" "-next"
}
proc dump_bin_file { db txn outfile checkfunc } {
	source ./include.tcl

	dump_bin_file_direction $db $txn $outfile $checkfunc "-first" "-next"
}

# Note: the following procedure assumes that the binary-file-as-keys were
# inserted into the database by put_file_as_key, and consist of the file
# name followed by the file contents as key, to ensure uniqueness.
proc dump_binkey_file_direction { db txn outfile checkfunc begin cont } {
	source ./include.tcl

	set d1 $testdir/d1

	set outf [open $outfile w]

	# Now we will get each key from the DB and dump to outfile
	set c [eval {$db cursor} $txn]
	error_check_good db_cursor [is_valid_cursor $c $db] TRUE

	set inf $d1
	for {set d [$c get $begin] } { [llength $d] != 0 } \
	    {set d [$c get $cont] } {
		set kd [lindex $d 0]
		set keyfile [lindex $kd 0]
		set data [lindex $kd 1]

		set ofid [open $d1 w]
		fconfigure $ofid -translation binary

		# Chop off the first few bytes--that's the file name,
		# added for uniqueness in put_file_as_key, which we don't
		# want in the regenerated file.
		set namelen [string length $data]
		set keyfile [string range $keyfile $namelen end]
		puts -nonewline $ofid $keyfile
		close $ofid

		$checkfunc $data $d1
		puts $outf $data
		flush $outf
	}
	close $outf
	error_check_good curs_close [$c close] 0
	fileremove $d1
}

proc dump_bin_file_direction { db txn outfile checkfunc begin cont } {
	source ./include.tcl

	set d1 $testdir/d1

	set outf [open $outfile w]

	# Now we will get each key from the DB and dump to outfile
	set c [eval {$db cursor} $txn]

	for {set d [$c get $begin] } \
	    { [llength $d] != 0 } {set d [$c get $cont] } {
		set k [lindex [lindex $d 0] 0]
		set data [lindex [lindex $d 0] 1]
		set ofid [open $d1 w]
		fconfigure $ofid -translation binary
		puts -nonewline $ofid $data
		close $ofid

		$checkfunc $k $d1
		puts $outf $k
	}
	close $outf
	error_check_good curs_close [$c close] 0
	fileremove -f $d1
}

proc make_data_str { key } {
	set datastr ""
	for {set i 0} {$i < 10} {incr i} {
		append datastr $key
	}
	return $datastr
}

proc error_check_bad { func result bad {txn 0}} {
	if { [binary_compare $result $bad] == 0 } {
		if { $txn != 0 } {
			$txn abort
		}
		flush stdout
		flush stderr
		error "FAIL:[timestamp] $func returned error value $bad"
	}
}

proc error_check_good { func result desired {txn 0} } {
	if { [binary_compare $desired $result] != 0 } {
		if { $txn != 0 } {
			$txn abort
		}
		flush stdout
		flush stderr
		error "FAIL:[timestamp]\
		    $func: expected $desired, got $result"
	}
}

# Locks have the prefix of their manager.
proc is_substr { str sub } {
	if { [string first $sub $str]  == -1 } {
		return 0
	} else {
		return 1
	}
}

proc release_list { l } {

	# Now release all the locks
	foreach el $l {
		catch { $el put } ret
		error_check_good lock_put $ret 0
	}
}

proc debug { {stop 0} } {
	global __debug_on
	global __debug_print
	global __debug_test

	set __debug_on 1
	set __debug_print 1
	set __debug_test $stop
}

# Check if each key appears exactly [llength dlist] times in the file with
# the duplicate tags matching those that appear in dlist.
proc dup_check { db txn tmpfile dlist {extra 0}} {
	source ./include.tcl

	set outf [open $tmpfile w]
	# Now we will get each key from the DB and dump to outfile
	set c [eval {$db cursor} $txn]
	set lastkey ""
	set done 0
	while { $done != 1} {
		foreach did $dlist {
			set rec [$c get "-next"]
			if { [string length $rec] == 0 } {
				set done 1
				break
			}
			set key [lindex [lindex $rec 0] 0]
			set fulldata [lindex [lindex $rec 0] 1]
			set id [id_of $fulldata]
			set d [data_of $fulldata]
			if { [string compare $key $lastkey] != 0 && \
			    $id != [lindex $dlist 0] } {
				set e [lindex $dlist 0]
				error "FAIL: \tKey \
				    $key, expected dup id $e, got $id"
			}
			error_check_good dupget.data $d $key
			error_check_good dupget.id $id $did
			set lastkey $key
		}
		#
		# Some tests add an extra dup (like overflow entries)
		# Check id if it exists.
		if { $extra != 0} {
			set okey $key
			set rec [$c get "-next"]
			if { [string length $rec] != 0 } {
				set key [lindex [lindex $rec 0] 0]
				#
				# If this key has no extras, go back for
				# next iteration.
				if { [string compare $key $lastkey] != 0 } {
					set key $okey
					set rec [$c get "-prev"]
				} else {
					set fulldata [lindex [lindex $rec 0] 1]
					set id [id_of $fulldata]
					set d [data_of $fulldata]
					error_check_bad dupget.data1 $d $key
					error_check_good dupget.id1 $id $extra
				}
			}
		}
		if { $done != 1 } {
			puts $outf $key
		}
	}
	close $outf
	error_check_good curs_close [$c close] 0
}

# Check if each key appears exactly [llength dlist] times in the file with
# the duplicate tags matching those that appear in dlist.
proc dup_file_check { db txn tmpfile dlist } {
	source ./include.tcl

	set outf [open $tmpfile w]
	# Now we will get each key from the DB and dump to outfile
	set c [eval {$db cursor} $txn]
	set lastkey ""
	set done 0
	while { $done != 1} {
		foreach did $dlist {
			set rec [$c get "-next"]
			if { [string length $rec] == 0 } {
				set done 1
				break
			}
			set key [lindex [lindex $rec 0] 0]
			if { [string compare $key $lastkey] != 0 } {
				#
				# If we changed files read in new contents.
				#
				set fid [open $key r]
				fconfigure $fid -translation binary
				set filecont [read $fid]
				close $fid
			}
			set fulldata [lindex [lindex $rec 0] 1]
			set id [id_of $fulldata]
			set d [data_of $fulldata]
			if { [string compare $key $lastkey] != 0 && \
			    $id != [lindex $dlist 0] } {
				set e [lindex $dlist 0]
				error "FAIL: \tKey \
				    $key, expected dup id $e, got $id"
			}
			error_check_good dupget.data $d $filecont
			error_check_good dupget.id $id $did
			set lastkey $key
		}
		if { $done != 1 } {
			puts $outf $key
		}
	}
	close $outf
	error_check_good curs_close [$c close] 0
}

# Parse duplicate data entries of the form N:data. Data_of returns
# the data part; id_of returns the numerical part
proc data_of {str} {
	set ndx [string first ":" $str]
	if { $ndx == -1 } {
		return ""
	}
	return [ string range $str [expr $ndx + 1] end]
}

proc id_of {str} {
	set ndx [string first ":" $str]
	if { $ndx == -1 } {
		return ""
	}

	return [ string range $str 0 [expr $ndx - 1]]
}

proc nop { {args} } {
	return
}

# Partial put test procedure.
# Munges a data val through three different partial puts.  Stores
# the final munged string in the dvals array so that you can check
# it later (dvals should be global).  We take the characters that
# are being replaced, make them capitals and then replicate them
# some number of times (n_add).  We do this at the beginning of the
# data, at the middle and at the end. The parameters are:
# db, txn, key -- as per usual.  Data is the original data element
# from which we are starting.  n_replace is the number of characters
# that we will replace.  n_add is the number of times we will add
# the replaced string back in.
proc partial_put { method db txn gflags key data n_replace n_add } {
	global dvals
	source ./include.tcl

	# Here is the loop where we put and get each key/data pair
	# We will do the initial put and then three Partial Puts
	# for the beginning, middle and end of the string.

	eval {$db put} $txn {$key [chop_data $method $data]}

	# Beginning change
	set s [string range $data 0 [ expr $n_replace - 1 ] ]
	set repl [ replicate [string toupper $s] $n_add ]

	# This is gross, but necessary:  if this is a fixed-length
	# method, and the chopped length of $repl is zero,
	# it's because the original string was zero-length and our data item
	# is all nulls.  Set repl to something non-NULL.
	if { [is_fixed_length $method] && \
	    [string length [chop_data $method $repl]] == 0 } {
		set repl [replicate "." $n_add]
	}

	set newstr [chop_data $method $repl[string range $data $n_replace end]]
	set ret [eval {$db put} $txn {-partial [list 0 $n_replace] \
	    $key [chop_data $method $repl]}]
	error_check_good put $ret 0

	set ret [eval {$db get} $gflags $txn {$key}]
	error_check_good get $ret [list [list $key [pad_data $method $newstr]]]

	# End Change
	set len [string length $newstr]
	set spl [expr $len - $n_replace]
	# Handle case where $n_replace > $len
	if { $spl < 0 } {
		set spl 0
	}

	set s [string range $newstr [ expr $len - $n_replace ] end ]
	# Handle zero-length keys
	if { [string length $s] == 0 } { set s "A" }

	set repl [ replicate [string toupper $s] $n_add ]
	set newstr [chop_data $method \
	    [string range $newstr 0 [expr $spl - 1 ] ]$repl]

	set ret [eval {$db put} $txn \
	    {-partial [list $spl $n_replace] $key [chop_data $method $repl]}]
	error_check_good put $ret 0

	set ret [eval {$db get} $gflags $txn {$key}]
	error_check_good get $ret [list [list $key [pad_data $method $newstr]]]

	# Middle Change
	set len [string length $newstr]
	set mid [expr $len / 2 ]
	set beg [expr $mid - [expr $n_replace / 2] ]
	set end [expr $beg + $n_replace - 1]
	set s [string range $newstr $beg $end]
	set repl [ replicate [string toupper $s] $n_add ]
	set newstr [chop_data $method [string range $newstr 0 \
	    [expr $beg - 1 ] ]$repl[string range $newstr [expr $end + 1] end]]

	set ret [eval {$db put} $txn {-partial [list $beg $n_replace] \
	    $key [chop_data $method $repl]}]
	error_check_good put $ret 0

	set ret [eval {$db get} $gflags $txn {$key}]
	error_check_good get $ret [list [list $key [pad_data $method $newstr]]]

	set dvals($key) [pad_data $method $newstr]
}

proc replicate { str times } {
	set res $str
	for { set i 1 } { $i < $times } { set i [expr $i * 2] } {
		append res $res
	}
	return $res
}

proc repeat { str n } {
	set ret ""
	while { $n > 0 } {
		set ret $str$ret
		incr n -1
	}
	return $ret
}

proc isqrt { l } {
	set s [expr sqrt($l)]
	set ndx [expr [string first "." $s] - 1]
	return [string range $s 0 $ndx]
}

# If we run watch_procs multiple times without an intervening
# testdir cleanup, it's possible that old sentinel files will confuse
# us.  Make sure they're wiped out before we spawn any other processes.
proc sentinel_init { } {
	source ./include.tcl

	set filelist {}
	set ret [catch {glob $testdir/begin.*} result]
	if { $ret == 0 } {
		set filelist $result
	}

	set ret [catch {glob $testdir/end.*} result]
	if { $ret == 0 } {
		set filelist [concat $filelist $result]
	}

	foreach f $filelist {
		fileremove $f
	}
}

proc watch_procs { pidlist {delay 30} {max 3600} {quiet 0} } {
	source ./include.tcl

	set elapsed 0

	# Don't start watching the processes until a sentinel
	# file has been created for each one.
	foreach pid $pidlist {
		while { [file exists $testdir/begin.$pid] == 0 } {
			tclsleep $delay
			incr elapsed $delay
			# If pids haven't been created in one-tenth
			# of the time allowed for the whole test, 
			# there's a problem.  Report an error and fail.
			if { $elapsed > [expr {$max / 10}] } {
				puts "FAIL: begin.pid not created"
				break
			}	
		}
	}

	while { 1 } {

		tclsleep $delay
		incr elapsed $delay

		# Find the list of processes with outstanding sentinel
		# files (i.e. a begin.pid and no end.pid).
		set beginlist {}
		set endlist {}
		set ret [catch {glob $testdir/begin.*} result]
		if { $ret == 0 } {
			set beginlist $result
		}
		set ret [catch {glob $testdir/end.*} result]
		if { $ret == 0 } {
			set endlist $result
		}

		set bpids {}
		catch {unset epids}
		foreach begfile $beginlist {
			lappend bpids [string range $begfile \
			    [string length $testdir/begin.] end]
		}
		foreach endfile $endlist {
			set epids([string range $endfile \
			    [string length $testdir/end.] end]) 1
		}

		# The set of processes that we still want to watch, $l,
		# is the set of pids that have begun but not ended
		# according to their sentinel files.
		set l {}
		foreach p $bpids {
			if { [info exists epids($p)] == 0 } {
				lappend l $p
			}
		}

		set rlist {}
		foreach i $l {
			set r [ catch { exec $KILL -0 $i } result ]
			if { $r == 0 } {
				lappend rlist $i
			}
		}
		if { [ llength $rlist] == 0 } {
			break
		} else {
			puts "[timestamp] processes running: $rlist"
		}

		if { $elapsed > $max } {
			# We have exceeded the limit; kill processes
			# and report an error
			foreach i $l {
				tclkill $i
			}
		}
	}
	if { $quiet == 0 } {
		puts "All processes have exited."
	}
}

# These routines are all used from within the dbscript.tcl tester.
proc db_init { dbp do_data } {
	global a_keys
	global l_keys
	source ./include.tcl

	set txn ""
	set nk 0
	set lastkey ""

	set a_keys() BLANK
	set l_keys ""

	set c [$dbp cursor]
	for {set d [$c get -first] } { [llength $d] != 0 } {
	    set d [$c get -next] } {
		set k [lindex [lindex $d 0] 0]
		set d2 [lindex [lindex $d 0] 1]
		incr nk
		if { $do_data == 1 } {
			if { [info exists a_keys($k)] } {
				lappend a_keys($k) $d2]
			} else {
				set a_keys($k) $d2
			}
		}

		lappend l_keys $k
	}
	error_check_good curs_close [$c close] 0

	return $nk
}

proc pick_op { min max n } {
	if { $n == 0 } {
		return add
	}

	set x [berkdb random_int 1 12]
	if {$n < $min} {
		if { $x <= 4 } {
			return put
		} elseif { $x <= 8} {
			return get
		} else {
			return add
		}
	} elseif {$n >  $max} {
		if { $x <= 4 } {
			return put
		} elseif { $x <= 8 } {
			return get
		} else {
			return del
		}

	} elseif { $x <= 3 } {
		return del
	} elseif { $x <= 6 } {
		return get
	} elseif { $x <= 9 } {
		return put
	} else {
		return add
	}
}

# random_data: Generate a string of random characters.
# If recno is 0 - Use average to pick a length between 1 and 2 * avg.
# If recno is non-0, generate a number between 1 and 2 ^ (avg * 2),
#   that will fit into a 32-bit integer.
# If the unique flag is 1, then make sure that the string is unique
# in the array "where".
proc random_data { avg unique where {recno 0} } {
	upvar #0 $where arr
	global debug_on
	set min 1
	set max [expr $avg+$avg-1]
	if { $recno  } {
		#
		# Tcl seems to have problems with values > 30.
		#
		if { $max > 30 } {
			set max 30
		}
		set maxnum [expr int(pow(2, $max))]
	}
	while {1} {
		set len [berkdb random_int $min $max]
		set s ""
		if {$recno} {
			set s [berkdb random_int 1 $maxnum]
		} else {
			for {set i 0} {$i < $len} {incr i} {
				append s [int_to_char [berkdb random_int 0 25]]
			}
		}

		if { $unique == 0 || [info exists arr($s)] == 0 } {
			break
		}
	}

	return $s
}

proc random_key { } {
	global l_keys
	global nkeys
	set x [berkdb random_int 0 [expr $nkeys - 1]]
	return [lindex $l_keys $x]
}

proc is_err { desired } {
	set x [berkdb random_int 1 100]
	if { $x <= $desired } {
		return 1
	} else {
		return 0
	}
}

proc pick_cursput { } {
	set x [berkdb random_int 1 4]
	switch $x {
		1 { return "-keylast" }
		2 { return "-keyfirst" }
		3 { return "-before" }
		4 { return "-after" }
	}
}

proc random_cursor { curslist } {
	global l_keys
	global nkeys

	set x [berkdb random_int 0 [expr [llength $curslist] - 1]]
	set dbc [lindex $curslist $x]

	# We want to randomly set the cursor.  Pick a key.
	set k [random_key]
	set r [$dbc get "-set" $k]
	error_check_good cursor_get:$k [is_substr Error $r] 0

	# Now move forward or backward some hops to randomly
	# position the cursor.
	set dist [berkdb random_int -10 10]

	set dir "-next"
	set boundary "-first"
	if { $dist < 0 } {
		set dir "-prev"
		set boundary "-last"
		set dist [expr 0 - $dist]
	}

	for { set i 0 } { $i < $dist } { incr i } {
		set r [ record $dbc get $dir $k ]
		if { [llength $d] == 0 } {
			set r [ record $dbc get $k $boundary ]
		}
		error_check_bad dbcget [llength $r] 0
	}
	return { [linsert r 0 $dbc] }
}

proc record { args } {
# Recording every operation makes tests ridiculously slow on
# NT, so we are commenting this out; for debugging purposes,
# it will undoubtedly be useful to uncomment this.
#	puts $args
#	flush stdout
	return [eval $args]
}

proc newpair { k data } {
	global l_keys
	global a_keys
	global nkeys

	set a_keys($k) $data
	lappend l_keys $k
	incr nkeys
}

proc rempair { k } {
	global l_keys
	global a_keys
	global nkeys

	unset a_keys($k)
	set n [lsearch $l_keys $k]
	error_check_bad rempair:$k $n -1
	set l_keys [lreplace $l_keys $n $n]
	incr nkeys -1
}

proc changepair { k data } {
	global l_keys
	global a_keys
	global nkeys

	set a_keys($k) $data
}

proc changedup { k olddata newdata } {
	global l_keys
	global a_keys
	global nkeys

	set d $a_keys($k)
	error_check_bad changedup:$k [llength $d] 0

	set n [lsearch $d $olddata]
	error_check_bad changedup:$k $n -1

	set a_keys($k) [lreplace $a_keys($k) $n $n $newdata]
}

# Insert a dup into the a_keys array with DB_KEYFIRST.
proc adddup { k olddata newdata } {
	global l_keys
	global a_keys
	global nkeys

	set d $a_keys($k)
	if { [llength $d] == 0 } {
		lappend l_keys $k
		incr nkeys
		set a_keys($k) { $newdata }
	}

	set ndx 0

	set d [linsert d $ndx $newdata]
	set a_keys($k) $d
}

proc remdup { k data } {
	global l_keys
	global a_keys
	global nkeys

	set d [$a_keys($k)]
	error_check_bad changedup:$k [llength $d] 0

	set n [lsearch $d $olddata]
	error_check_bad changedup:$k $n -1

	set a_keys($k) [lreplace $a_keys($k) $n $n]
}

proc dump_full_file { db txn outfile checkfunc start continue } {
	source ./include.tcl

	set outf [open $outfile w]
	# Now we will get each key from the DB and dump to outfile
	set c [eval {$db cursor} $txn]
	error_check_good dbcursor [is_valid_cursor $c $db] TRUE

	for {set d [$c get $start] } { [string length $d] != 0 } {
		set d [$c get $continue] } {
		set k [lindex [lindex $d 0] 0]
		set d2 [lindex [lindex $d 0] 1]
		$checkfunc $k $d2
		puts $outf "$k\t$d2"
	}
	close $outf
	error_check_good curs_close [$c close] 0
}

proc int_to_char { i } {
	global alphabet

	return [string index $alphabet $i]
}

proc dbcheck { key data } {
	global l_keys
	global a_keys
	global nkeys
	global check_array

	if { [lsearch $l_keys $key] == -1 } {
		error "FAIL: Key |$key| not in list of valid keys"
	}

	set d $a_keys($key)

	if { [info exists check_array($key) ] } {
		set check $check_array($key)
	} else {
		set check {}
	}

	if { [llength $d] > 1 } {
		if { [llength $check] != [llength $d] } {
			# Make the check array the right length
			for { set i [llength $check] } { $i < [llength $d] } \
			    {incr i} {
				lappend check 0
			}
			set check_array($key) $check
		}

		# Find this data's index
		set ndx [lsearch $d $data]
		if { $ndx == -1 } {
			error "FAIL: \
			    Data |$data| not found for key $key.  Found |$d|"
		}

		# Set the bit in the check array
		set check_array($key) [lreplace $check_array($key) $ndx $ndx 1]
	} elseif { [string compare $d $data] != 0 } {
		error "FAIL: \
		    Invalid data |$data| for key |$key|. Expected |$d|."
	} else {
		set check_array($key) 1
	}
}

# Dump out the file and verify it
proc filecheck { file txn } {
	global check_array
	global l_keys
	global nkeys
	global a_keys
	source ./include.tcl

	if { [info exists check_array] == 1 } {
		unset check_array
	}

	open_and_dump_file $file NULL $file.dump dbcheck dump_full_file \
	    "-first" "-next"

	# Check that everything we checked had all its data
	foreach i [array names check_array] {
		set count 0
		foreach j $check_array($i) {
			if { $j != 1 } {
				puts -nonewline "Key |$i| never found datum"
				puts " [lindex $a_keys($i) $count]"
			}
			incr count
		}
	}

	# Check that all keys appeared in the checked array
	set count 0
	foreach k $l_keys {
		if { [info exists check_array($k)] == 0 } {
			puts "filecheck: key |$k| not found.  Data: $a_keys($k)"
		}
		incr count
	}

	if { $count != $nkeys } {
		puts "filecheck: Got $count keys; expected $nkeys"
	}
}

proc cleanup { dir env { quiet 0 } } {
	global gen_upgrade
	global is_qnx_test
	global old_encrypt
	global passwd
	global upgrade_dir
	global upgrade_be
	global upgrade_method
	global upgrade_name
	source ./include.tcl

	if { $gen_upgrade == 1 } {
		set vers [berkdb version]
		set maj [lindex $vers 0]
		set min [lindex $vers 1]

		# Is this machine big or little endian?  We want to mark
		# the test directories appropriately, since testing
		# little-endian databases generated by a big-endian machine,
		# and/or vice versa, is interesting.
		if { [big_endian] } {
			set myendianness be
		} else {
			set myendianness le
		}

		if { $upgrade_be == 1 } {
			set version_dir "$myendianness-$maj.${min}be"
			set en be
		} else {
			set version_dir "$myendianness-$maj.${min}le"
			set en le
		}

		set dest $upgrade_dir/$version_dir/$upgrade_method
		exec mkdir -p $dest

		set dbfiles [glob -nocomplain $dir/*.db]
		foreach dbfile $dbfiles {
			set basename [string range $dbfile \
			    [expr [string length $dir] + 1] end-3]

			set newbasename $upgrade_name-$basename

			# db_dump file
			error_check_good db_dump($dbfile) \
			    [catch {exec $util_path/db_dump -k $dbfile > \
			    $dir/$newbasename.dump}] 0

			# tcl_dump file
			upgrade_dump $dbfile \
			    $dir/$newbasename.tcldump

			# Rename dbfile and any dbq files.
			file rename $dbfile $dir/$newbasename-$en.db
			foreach dbq \
			    [glob -nocomplain $dir/__dbq.$basename.db.*] {
				set s [string length $dir/__dbq.]
				set newname [string replace $dbq $s \
				    [expr [string length $basename] + $s - 1] \
				    $newbasename-$en]
				file rename $dbq $newname
			}
			set cwd [pwd]
			cd $dir
			catch {eval exec tar -cvf $dest/$newbasename.tar \
			    [glob $newbasename* __dbq.$newbasename-$en.db.*]}
			catch {exec gzip -9v $dest/$newbasename.tar}
			cd $cwd
		}
	}

#	check_handles
	set remfiles {}
	set ret [catch { glob $dir/* } result]
	if { $ret == 0 } {
		foreach fileorig $result {
			#
			# We:
			# - Ignore any env-related files, which are
			# those that have __db.* or log.* if we are
			# running in an env.  Also ignore files whose
			# names start with REPDIR_;  these are replication
			# subdirectories.
			# - Call 'dbremove' on any databases.
			# Remove any remaining temp files.
			#
			switch -glob -- $fileorig {
			*/DIR_* -
			*/__db.* -
			*/log.*	{
				if { $env != "NULL" } {
					continue
				} else {
					if { $is_qnx_test } {
						catch {berkdb envremove -force \
						    -home $dir} r
					}
					lappend remfiles $fileorig
				}
				}
			*.db	{
				set envargs ""
				set encarg ""
				#
				# If in an env, it should be open crypto
				# or not already.
				#
				if { $env != "NULL"} {
					set file [file tail $fileorig]
					set envargs " -env $env "
					if { [is_txnenv $env] } {
						append envargs " -auto_commit "
					}
				} else {
					if { $old_encrypt != 0 } {
						set encarg "-encryptany $passwd"
					}
					set file $fileorig
				}

				# If a database is left in a corrupt
				# state, dbremove might not be able to handle
				# it (it does an open before the remove).
				# Be prepared for this, and if necessary,
				# just forcibly remove the file with a warning
				# message.
				set ret [catch \
				    {eval {berkdb dbremove} $envargs $encarg \
				    $file} res]
				if { $ret != 0 } {
					# If it failed, there is a chance
					# that the previous run was using
					# encryption and we cannot know about
					# it (different tclsh instantiation).
					# Try to remove it with crypto.
					if { $env == "NULL" && \
					    $old_encrypt == 0} {
						set ret [catch \
				    		    {eval {berkdb dbremove} \
						    -encryptany $passwd \
				    		    $envargs $file} res]
					}
					if { $ret != 0 } {
						if { $quiet == 0 } {
							puts \
				    "FAIL: dbremove in cleanup failed: $res"
						}
						set file $fileorig
						lappend remfiles $file
					}
				}
				}
			default	{
				lappend remfiles $fileorig
				}
			}
		}
		if {[llength $remfiles] > 0} {
			eval fileremove -f $remfiles
		}
	}
}

proc log_cleanup { dir } {
	source ./include.tcl

	set files [glob -nocomplain $dir/log.*]
	if { [llength $files] != 0} {
		foreach f $files {
			fileremove -f $f
		}
	}
}

proc env_cleanup { dir } {
	global old_encrypt
	global passwd
	source ./include.tcl

	set encarg ""
	if { $old_encrypt != 0 } {
		set encarg "-encryptany $passwd"
	}
	set stat [catch {eval {berkdb envremove -home} $dir $encarg} ret]
	#
	# If something failed and we are left with a region entry
	# in /dev/shmem that is zero-length, the envremove will
	# succeed, and the shm_unlink will succeed, but it will not
	# remove the zero-length entry from /dev/shmem.  Remove it
	# using fileremove or else all other tests using an env
	# will immediately fail.
	#
	if { $is_qnx_test == 1 } {
		set region_files [glob -nocomplain /dev/shmem/$dir*]
		if { [llength $region_files] != 0 } {
			foreach f $region_files {
				fileremove -f $f
			}
		}
	}
	log_cleanup $dir
	cleanup $dir NULL
}

proc remote_cleanup { server dir localdir } {
	set home [file tail $dir]
	error_check_good cleanup:remove [berkdb envremove -home $home \
	    -server $server] 0
	catch {exec rsh $server rm -f $dir/*} ret
	cleanup $localdir NULL
}

proc help { cmd } {
	if { [info command $cmd] == $cmd } {
		set is_proc [lsearch [info procs $cmd] $cmd]
		if { $is_proc == -1 } {
			# Not a procedure; must be a C command
			# Let's hope that it takes some parameters
			# and that it prints out a message
			puts "Usage: [eval $cmd]"
		} else {
			# It is a tcl procedure
			puts -nonewline "Usage: $cmd"
			set args [info args $cmd]
			foreach a $args {
				set is_def [info default $cmd $a val]
				if { $is_def != 0 } {
					# Default value
					puts -nonewline " $a=$val"
				} elseif {$a == "args"} {
					# Print out flag values
					puts " options"
					args
				} else {
					# No default value
					puts -nonewline " $a"
				}
			}
			puts ""
		}
	} else {
		puts "$cmd is not a command"
	}
}

# Run a recovery test for a particular operation
# Notice that we catch the return from CP and do not do anything with it.
# This is because Solaris CP seems to exit non-zero on occasion, but
# everything else seems to run just fine.
#
# We split it into two functions so that the preparation and command
# could be executed in a different process than the recovery.
#
proc op_codeparse { encodedop op } {
	set op1 ""
	set op2 ""
	switch $encodedop {
	"abort" {
		set op1 $encodedop
		set op2 ""
	}
	"commit" {
		set op1 $encodedop
		set op2 ""
	}
	"prepare-abort" {
		set op1 "prepare"
		set op2 "abort"
	}
	"prepare-commit" {
		set op1 "prepare"
		set op2 "commit"
	}
	"prepare-discard" {
		set op1 "prepare"
		set op2 "discard"
	}
	}

	if { $op == "op" } {
		return $op1
	} else {
		return $op2
	}
}

proc op_recover { encodedop dir env_cmd dbfile cmd msg } {
	source ./include.tcl

	set op [op_codeparse $encodedop "op"]
	set op2 [op_codeparse $encodedop "sub"]
	puts "\t$msg $encodedop"
	set gidf ""
	if { $op == "prepare" } {
		sentinel_init

		# Fork off a child to run the cmd
		# We append the gid, so start here making sure
		# we don't have old gid's around.
		set outfile $testdir/childlog
		fileremove -f $testdir/gidfile
		set gidf $testdir/gidfile
		set pidlist {}
		# puts "$tclsh_path $test_path/recdscript.tcl $testdir/recdout \
		#    $op $dir $env_cmd $dbfile $gidf $cmd"
		set p [exec $tclsh_path $test_path/wrap.tcl recdscript.tcl \
		    $testdir/recdout $op $dir $env_cmd $dbfile $gidf $cmd &]
		lappend pidlist $p
		watch_procs $pidlist 5
		set f1 [open $testdir/recdout r]
		set r [read $f1]
		puts -nonewline $r
		close $f1
		fileremove -f $testdir/recdout
	} else {
		op_recover_prep $op $dir $env_cmd $dbfile $gidf $cmd
	}
	op_recover_rec $op $op2 $dir $env_cmd $dbfile $gidf
}

proc op_recover_prep { op dir env_cmd dbfile gidf cmd } {
	global log_log_record_types
	global recd_debug
	global recd_id
	global recd_op
	source ./include.tcl

	#puts "op_recover: $op $dir $env $dbfile $cmd"

	set init_file $dir/t1
	set afterop_file $dir/t2
	set final_file $dir/t3

	# Keep track of the log types we've seen
	if { $log_log_record_types == 1} {
		logtrack_read $dir
	}

	# Save the initial file and open the environment and the file
	catch { file copy -force $dir/$dbfile $dir/$dbfile.init } res
	copy_extent_file $dir $dbfile init

	convert_encrypt $env_cmd
	set env [eval $env_cmd]
	error_check_good envopen [is_valid_env $env] TRUE

	set db [berkdb open -auto_commit -env $env $dbfile]
	error_check_good dbopen [is_valid_db $db] TRUE

	# Dump out file contents for initial case
	open_and_dump_file $dbfile $env $init_file nop \
	    dump_file_direction "-first" "-next"

	set t [$env txn]
	error_check_bad txn_begin $t NULL
	error_check_good txn_begin [is_substr $t "txn"] 1

	# Now fill in the db, tmgr, and the txnid in the command
	set exec_cmd $cmd

	set i [lsearch $cmd ENV]
	if { $i != -1 } {
		set exec_cmd [lreplace $exec_cmd $i $i $env]
	}

	set i [lsearch $cmd TXNID]
	if { $i != -1 } {
		set exec_cmd [lreplace $exec_cmd $i $i $t]
	}

	set i [lsearch $exec_cmd DB]
	if { $i != -1 } {
		set exec_cmd [lreplace $exec_cmd $i $i $db]
	}

	# To test DB_CONSUME, we need to expect a record return, not "0".
	set i [lsearch $exec_cmd "-consume"]
	if { $i	!= -1 } {
		set record_exec_cmd_ret 1
	} else {
		set record_exec_cmd_ret 0
	}

	# For the DB_APPEND test, we need to expect a return other than
	# 0;  set this flag to be more lenient in the error_check_good.
	set i [lsearch $exec_cmd "-append"]
	if { $i != -1 } {
		set lenient_exec_cmd_ret 1
	} else {
		set lenient_exec_cmd_ret 0
	}

	# Execute command and commit/abort it.
	set ret [eval $exec_cmd]
	if { $record_exec_cmd_ret == 1 } {
		error_check_good "\"$exec_cmd\"" [llength [lindex $ret 0]] 2
	} elseif { $lenient_exec_cmd_ret == 1 } {
		error_check_good "\"$exec_cmd\"" [expr $ret > 0] 1
	} else {
		error_check_good "\"$exec_cmd\"" $ret 0
	}

	set record_exec_cmd_ret 0
	set lenient_exec_cmd_ret 0

	# Sync the file so that we can capture a snapshot to test recovery.
	error_check_good sync:$db [$db sync] 0

	catch { file copy -force $dir/$dbfile $dir/$dbfile.afterop } res
	copy_extent_file $dir $dbfile afterop
	open_and_dump_file $dir/$dbfile.afterop NULL \
		$afterop_file nop dump_file_direction "-first" "-next"

	#puts "\t\t\tExecuting txn_$op:$t"
	if { $op == "prepare" } {
		set gid [make_gid global:$t]
		set gfd [open $gidf w+]
		puts $gfd $gid
		close $gfd
		error_check_good txn_$op:$t [$t $op $gid] 0
	} else {
		error_check_good txn_$op:$t [$t $op] 0
	}

	switch $op {
		"commit" { puts "\t\tCommand executed and committed." }
		"abort" { puts "\t\tCommand executed and aborted." }
		"prepare" { puts "\t\tCommand executed and prepared." }
	}

	# Sync the file so that we can capture a snapshot to test recovery.
	error_check_good sync:$db [$db sync] 0

	catch { file copy -force $dir/$dbfile $dir/$dbfile.final } res
	copy_extent_file $dir $dbfile final
	open_and_dump_file $dir/$dbfile.final NULL \
	    $final_file nop dump_file_direction "-first" "-next"

	# If this is an abort or prepare-abort, it should match the
	#   original file.
	# If this was a commit or prepare-commit, then this file should
	#   match the afterop file.
	# If this was a prepare without an abort or commit, we still
	#   have transactions active, and peering at the database from
	#   another environment will show data from uncommitted transactions.
	#   Thus we just skip this in the prepare-only case;  what
	#   we care about are the results of a prepare followed by a
	#   recovery, which we test later.
	if { $op == "commit" } {
		filesort $afterop_file $afterop_file.sort
		filesort $final_file $final_file.sort
		error_check_good \
		    diff(post-$op,pre-commit):diff($afterop_file,$final_file) \
		    [filecmp $afterop_file.sort $final_file.sort] 0
	} elseif { $op == "abort" } {
		filesort $init_file $init_file.sort
		filesort $final_file $final_file.sort
		error_check_good \
		    diff(initial,post-$op):diff($init_file,$final_file) \
		    [filecmp $init_file.sort $final_file.sort] 0
	} else {
		# Make sure this really is one of the prepare tests
		error_check_good assert:prepare-test $op "prepare"
	}

	# Running recovery on this database should not do anything.
	# Flush all data to disk, close the environment and save the
	# file.
	# XXX DO NOT CLOSE FILE ON PREPARE -- if you are prepared,
	# you really have an active transaction and you're not allowed
	# to close files that are being acted upon by in-process
	# transactions.
	if { $op != "prepare" } {
		error_check_good close:$db [$db close] 0
	}

	#
	# If we are running 'prepare' don't close the env with an
	# active transaction.  Leave it alone so the close won't
	# quietly abort it on us.
	if { [is_substr $op "prepare"] != 1 } {
		error_check_good envclose [$env close] 0
	}
	return
}

proc op_recover_rec { op op2 dir env_cmd dbfile gidf} {
	global log_log_record_types
	global recd_debug
	global recd_id
	global recd_op
	global encrypt
	global passwd
	source ./include.tcl

	#puts "op_recover_rec: $op $op2 $dir $env_cmd $dbfile $gidf"

	set init_file $dir/t1
	set afterop_file $dir/t2
	set final_file $dir/t3

	# Keep track of the log types we've seen
	if { $log_log_record_types == 1} {
		logtrack_read $dir
	}

	berkdb debug_check
	puts -nonewline "\t\top_recover_rec: Running recovery ... "
	flush stdout

	set recargs "-h $dir -c "
	if { $encrypt > 0 } {
		append recargs " -P $passwd "
	}
	set stat [catch {eval exec $util_path/db_recover -e $recargs} result]
	if { $stat == 1 } {
		error "FAIL: Recovery error: $result."
	}
	puts -nonewline "complete ... "

	#
	# We cannot run db_recover here because that will open an env, run
	# recovery, then close it, which will abort the outstanding txns.
	# We want to do it ourselves.
	#
	set env [eval $env_cmd]
	error_check_good dbenv [is_valid_widget $env env] TRUE

	error_check_good db_verify [verify_dir $testdir "\t\t" 0 1] 0
	puts "verified"

	# If we left a txn as prepared, but not aborted or committed,
	# we need to do a txn_recover.  Make sure we have the same
	# number of txns we want.
	if { $op == "prepare"} {
		set txns [$env txn_recover]
		error_check_bad txnrecover [llength $txns] 0
		set gfd [open $gidf r]
		set origgid [read -nonewline $gfd]
		close $gfd
		set txnlist [lindex $txns 0]
		set t [lindex $txnlist 0]
		set gid [lindex $txnlist 1]
		error_check_good gidcompare $gid $origgid
		puts "\t\t\tExecuting txn_$op2:$t"
		error_check_good txn_$op2:$t [$t $op2] 0
		#
		# If we are testing discard, we do need to resolve
		# the txn, so get the list again and now abort it.
		#
		if { $op2 == "discard" } {
			set txns [$env txn_recover]
			error_check_bad txnrecover [llength $txns] 0
			set txnlist [lindex $txns 0]
			set t [lindex $txnlist 0]
			set gid [lindex $txnlist 1]
			error_check_good gidcompare $gid $origgid
			puts "\t\t\tExecuting txn_abort:$t"
			error_check_good disc_txn_abort:$t [$t abort] 0
		}
	}

	open_and_dump_file $dir/$dbfile NULL $final_file nop \
	    dump_file_direction "-first" "-next"
	if { $op == "commit" || $op2 == "commit" } {
		filesort $afterop_file $afterop_file.sort
		filesort $final_file $final_file.sort
		error_check_good \
		    diff(post-$op,pre-commit):diff($afterop_file,$final_file) \
		    [filecmp $afterop_file.sort $final_file.sort] 0
	} else {
		filesort $init_file $init_file.sort
		filesort $final_file $final_file.sort
		error_check_good \
		    diff(initial,post-$op):diff($init_file,$final_file) \
		    [filecmp $init_file.sort $final_file.sort] 0
	}

	# Now close the environment, substitute a file that will need
	# recovery and try running recovery again.
	reset_env $env
	if { $op == "commit" || $op2 == "commit" } {
		catch { file copy -force $dir/$dbfile.init $dir/$dbfile } res
		move_file_extent $dir $dbfile init copy
	} else {
		catch { file copy -force $dir/$dbfile.afterop $dir/$dbfile } res
		move_file_extent $dir $dbfile afterop copy
	}

	berkdb debug_check
	puts -nonewline "\t\tRunning recovery on pre-op database ... "
	flush stdout

	set stat [catch {eval exec $util_path/db_recover $recargs} result]
	if { $stat == 1 } {
		error "FAIL: Recovery error: $result."
	}
	puts -nonewline "complete ... "

	error_check_good db_verify_preop [verify_dir $testdir "\t\t" 0 1] 0

	puts "verified"

	set env [eval $env_cmd]

	open_and_dump_file $dir/$dbfile NULL $final_file nop \
	    dump_file_direction "-first" "-next"
	if { $op == "commit" || $op2 == "commit" } {
		filesort $final_file $final_file.sort
		filesort $afterop_file $afterop_file.sort
		error_check_good \
		    diff(post-$op,recovered):diff($afterop_file,$final_file) \
		    [filecmp $afterop_file.sort $final_file.sort] 0
	} else {
		filesort $init_file $init_file.sort
		filesort $final_file $final_file.sort
		error_check_good \
		    diff(initial,post-$op):diff($init_file,$final_file) \
		    [filecmp $init_file.sort $final_file.sort] 0
	}

	# This should just close the environment, not blow it away.
	reset_env $env
}

proc populate { db method txn n dups bigdata } {
	source ./include.tcl

	set did [open $dict]
	set count 0
	while { [gets $did str] != -1 && $count < $n } {
		if { [is_record_based $method] == 1 } {
			set key [expr $count + 1]
		} elseif { $dups == 1 } {
			set key duplicate_key
		} else {
			set key $str
		}
		if { $bigdata == 1 && [berkdb random_int 1 3] == 1} {
			set str [replicate $str 1000]
		}

		set ret [$db put -txn $txn $key $str]
		error_check_good db_put:$key $ret 0
		incr count
	}
	close $did
	return 0
}

proc big_populate { db txn n } {
	source ./include.tcl

	set did [open $dict]
	set count 0
	while { [gets $did str] != -1 && $count < $n } {
		set key [replicate $str 50]
		set ret [$db put -txn $txn $key $str]
		error_check_good db_put:$key $ret 0
		incr count
	}
	close $did
	return 0
}

proc unpopulate { db txn num } {
	source ./include.tcl

	set c [eval {$db cursor} "-txn $txn"]
	error_check_bad $db:cursor $c NULL
	error_check_good $db:cursor [is_substr $c $db] 1

	set i 0
	for {set d [$c get -first] } { [llength $d] != 0 } {
		set d [$c get -next] } {
		$c del
		incr i
		if { $num != 0 && $ >= $num } {
			break
		}
	}
	error_check_good cursor_close [$c close] 0
	return 0
}

proc reset_env { env } {
	error_check_good env_close [$env close] 0
}

proc minlocks { myenv locker_id obj_id num } {
	return [countlocks $myenv $locker_id $obj_id $num ]
}

proc maxlocks { myenv locker_id obj_id num } {
	return [countlocks $myenv $locker_id $obj_id $num ]
}

proc minwrites { myenv locker_id obj_id num } {
	return [countlocks $myenv $locker_id $obj_id $num ]
}

proc countlocks { myenv locker_id obj_id num } {
	set locklist ""
	for { set i 0} {$i < [expr $obj_id * 4]} { incr i } {
		set r [catch {$myenv lock_get read $locker_id \
		    [expr $obj_id * 1000 + $i]} l ]
		if { $r != 0 } {
			puts $l
			return ERROR
		} else {
			error_check_good lockget:$obj_id [is_substr $l $myenv] 1
			lappend locklist $l
		}
	}

	# Now acquire a write lock
	if { $obj_id != 1 } {
		set r [catch {$myenv lock_get write $locker_id \
		    [expr $obj_id * 1000 + 10]} l ]
		if { $r != 0 } {
			puts $l
			return ERROR
		} else {
			error_check_good lockget:$obj_id [is_substr $l $myenv] 1
			lappend locklist $l
		}
	}

	set ret [ring $myenv $locker_id $obj_id $num]

	foreach l $locklist {
		error_check_good lockput:$l [$l put] 0
	}

	return $ret
}

# This routine will let us obtain a ring of deadlocks.
# Each locker will get a lock on obj_id, then sleep, and
# then try to lock (obj_id + 1) % num.
# When the lock is finally granted, we release our locks and
# return 1 if we got both locks and DEADLOCK if we deadlocked.
# The results here should be that 1 locker deadlocks and the
# rest all finish successfully.
proc ring { myenv locker_id obj_id num } {
	source ./include.tcl

	if {[catch {$myenv lock_get write $locker_id $obj_id} lock1] != 0} {
		puts $lock1
		return ERROR
	} else {
		error_check_good lockget:$obj_id [is_substr $lock1 $myenv] 1
	}

	tclsleep 30
	set nextobj [expr ($obj_id + 1) % $num]
	set ret 1
	if {[catch {$myenv lock_get write $locker_id $nextobj} lock2] != 0} {
		if {[string match "*DEADLOCK*" $lock2] == 1} {
			set ret DEADLOCK
		} else {
			puts $lock2
			set ret ERROR
		}
	} else {
		error_check_good lockget:$obj_id [is_substr $lock2 $myenv] 1
	}

	# Now release the first lock
	error_check_good lockput:$lock1 [$lock1 put] 0

	if {$ret == 1} {
		error_check_bad lockget:$obj_id $lock2 NULL
		error_check_good lockget:$obj_id [is_substr $lock2 $myenv] 1
		error_check_good lockput:$lock2 [$lock2 put] 0
	}
	return $ret
}

# This routine will create massive deadlocks.
# Each locker will get a readlock on obj_id, then sleep, and
# then try to upgrade the readlock to a write lock.
# When the lock is finally granted, we release our first lock and
# return 1 if we got both locks and DEADLOCK if we deadlocked.
# The results here should be that 1 locker succeeds in getting all
# the locks and everyone else deadlocks.
proc clump { myenv locker_id obj_id num } {
	source ./include.tcl

	set obj_id 10
	if {[catch {$myenv lock_get read $locker_id $obj_id} lock1] != 0} {
		puts $lock1
		return ERROR
	} else {
		error_check_good lockget:$obj_id \
		    [is_valid_lock $lock1 $myenv] TRUE
	}

	tclsleep 30
	set ret 1
	if {[catch {$myenv lock_get write $locker_id $obj_id} lock2] != 0} {
		if {[string match "*DEADLOCK*" $lock2] == 1} {
			set ret DEADLOCK
		} else {
			set ret ERROR
		}
	} else {
		error_check_good \
		    lockget:$obj_id [is_valid_lock $lock2 $myenv] TRUE
	}

	# Now release the first lock
	error_check_good lockput:$lock1 [$lock1 put] 0

	if {$ret == 1} {
		error_check_good \
		    lockget:$obj_id [is_valid_lock $lock2 $myenv] TRUE
		error_check_good lockput:$lock2 [$lock2 put] 0
	}
	return $ret
 }

proc dead_check { t procs timeout dead clean other } {
	error_check_good $t:$procs:other $other 0
	switch $t {
		ring {
			# with timeouts the number of deadlocks is unpredictable
			if { $timeout != 0 && $dead > 1 } {
				set clean [ expr $clean + $dead - 1]
				set dead 1
			}
			error_check_good $t:$procs:deadlocks $dead 1
			error_check_good $t:$procs:success $clean \
			    [expr $procs - 1]
		}
		clump {
			error_check_good $t:$procs:deadlocks $dead \
			    [expr $procs - 1]
			error_check_good $t:$procs:success $clean 1
		}
		oldyoung {
			error_check_good $t:$procs:deadlocks $dead 1
			error_check_good $t:$procs:success $clean \
			    [expr $procs - 1]
		}
		minlocks {
			error_check_good $t:$procs:deadlocks $dead 1
			error_check_good $t:$procs:success $clean \
			    [expr $procs - 1]
		}
		maxlocks {
			error_check_good $t:$procs:deadlocks $dead 1
			error_check_good $t:$procs:success $clean \
			    [expr $procs - 1]
		}
		minwrites {
			error_check_good $t:$procs:deadlocks $dead 1
			error_check_good $t:$procs:success $clean \
			    [expr $procs - 1]
		}
		default {
			error "Test $t not implemented"
		}
	}
}

proc rdebug { id op where } {
	global recd_debug
	global recd_id
	global recd_op

	set recd_debug $where
	set recd_id $id
	set recd_op $op
}

proc rtag { msg id } {
	set tag [lindex $msg 0]
	set tail [expr [string length $tag] - 2]
	set tag [string range $tag $tail $tail]
	if { $id == $tag } {
		return 1
	} else {
		return 0
	}
}

proc zero_list { n } {
	set ret ""
	while { $n > 0 } {
		lappend ret 0
		incr n -1
	}
	return $ret
}

proc check_dump { k d } {
	puts "key: $k data: $d"
}

proc reverse { s } {
	set res ""
	for { set i 0 } { $i < [string length $s] } { incr i } {
		set res "[string index $s $i]$res"
	}

	return $res
}

#
# This is a internal only proc.  All tests should use 'is_valid_db' etc.
#
proc is_valid_widget { w expected } {
	# First N characters must match "expected"
	set l [string length $expected]
	incr l -1
	if { [string compare [string range $w 0 $l] $expected] != 0 } {
		return $w
	}

	# Remaining characters must be digits
	incr l 1
	for { set i $l } { $i < [string length $w] } { incr i} {
		set c [string index $w $i]
		if { $c < "0" || $c > "9" } {
			return $w
		}
	}

	return TRUE
}

proc is_valid_db { db } {
	return [is_valid_widget $db db]
}

proc is_valid_env { env } {
	return [is_valid_widget $env env]
}

proc is_valid_cursor { dbc db } {
	return [is_valid_widget $dbc $db.c]
}

proc is_valid_lock { lock env } {
	return [is_valid_widget $lock $env.lock]
}

proc is_valid_logc { logc env } {
	return [is_valid_widget $logc $env.logc]
}

proc is_valid_mpool { mpool env } {
	return [is_valid_widget $mpool $env.mp]
}

proc is_valid_page { page mpool } {
	return [is_valid_widget $page $mpool.pg]
}

proc is_valid_txn { txn env } {
	return [is_valid_widget $txn $env.txn]
}

proc is_valid_mutex { m env } {
	return [is_valid_widget $m $env.mutex]
}

proc is_valid_lock {l env} {
	return [is_valid_widget $l $env.lock]
}

proc is_valid_locker {l } {
	return [is_valid_widget $l ""]
}

proc send_cmd { fd cmd {sleep 2}} {
	source ./include.tcl

	puts $fd "if \[catch {set v \[$cmd\] ; puts \$v} ret\] { \
		puts \"FAIL: \$ret\" \
	}"
	puts $fd "flush stdout"
	flush $fd
	berkdb debug_check
	tclsleep $sleep

	set r [rcv_result $fd]
	return $r
}

proc rcv_result { fd } {
	set r [gets $fd result]
	error_check_bad remote_read $r -1

	return $result
}

proc send_timed_cmd { fd rcv_too cmd } {
	set c1 "set start \[timestamp -r\]; "
	set c2 "puts \[expr \[timestamp -r\] - \$start\]"
	set full_cmd [concat $c1 $cmd ";" $c2]

	puts $fd $full_cmd
	puts $fd "flush stdout"
	flush $fd
	return 0
}

#
# The rationale behind why we have *two* "data padding" routines is outlined
# below:
#
# Both pad_data and chop_data truncate data that is too long. However,
# pad_data also adds the pad character to pad data out to the fixed length
# record length.
#
# Which routine you call does not depend on the length of the data you're
# using, but on whether you're doing a put or a get. When we do a put, we
# have to make sure the data isn't longer than the size of a record because
# otherwise we'll get an error (use chop_data). When we do a get, we want to
# check that db padded everything correctly (use pad_data on the value against
# which we are comparing).
#
# We don't want to just use the pad_data routine for both purposes, because
# we want to be able to test whether or not db is padding correctly. For
# example, the queue access method had a bug where when a record was
# overwritten (*not* a partial put), only the first n bytes of the new entry
# were written, n being the new entry's (unpadded) length.  So, if we did
# a put with key,value pair (1, "abcdef") and then a put (1, "z"), we'd get
# back (1,"zbcdef"). If we had used pad_data instead of chop_data, we would
# have gotten the "correct" result, but we wouldn't have found this bug.
proc chop_data {method data} {
	global fixed_len

	if {[is_fixed_length $method] == 1 && \
	    [string length $data] > $fixed_len} {
		return [eval {binary format a$fixed_len $data}]
	} else {
		return $data
	}
}

proc pad_data {method data} {
	global fixed_len

	if {[is_fixed_length $method] == 1} {
		return [eval {binary format a$fixed_len $data}]
	} else {
		return $data
	}
}

proc make_fixed_length {method data {pad 0}} {
	global fixed_len
	global fixed_pad

	if {[is_fixed_length $method] == 1} {
		if {[string length $data] > $fixed_len } {
		    error_check_bad make_fixed_len:TOO_LONG 1 1
		}
		while { [string length $data] < $fixed_len } {
			set data [format $data%c $fixed_pad]
		}
	}
	return $data
}

proc make_gid {data} {
	while { [string length $data] < 127 } {
		set data [format ${data}0]
	}
	return $data
}

proc make_gid {data} {
	while { [string length $data] < 128 } {
		set data [format ${data}0]
	}
	return $data
}

# shift data for partial
# pad with fixed pad (which is NULL)
proc partial_shift { data offset direction} {
	global fixed_len

	set len [expr $fixed_len - 1]

	if { [string compare $direction "right"] == 0 } {
		for { set i 1} { $i <= $offset } {incr i} {
			set data [binary format x1a$len $data]
		}
	} elseif { [string compare $direction "left"] == 0 } {
		for { set i 1} { $i <= $offset } {incr i} {
			set data [string range $data 1 end]
			set data [binary format a$len $data]
		}
	}
	return $data
}

# string compare does not always work to compare
# this data, nor does expr (==)
# specialized routine for comparison
# (for use in fixed len recno and q)
proc binary_compare { data1 data2 } {
	if { [string length $data1] != [string length $data2] || \
	    [string compare -length \
	    [string length $data1] $data1 $data2] != 0 } {
		return 1
	} else {
		return 0
	}
}

proc convert_method { method } {
	switch -- $method {
		-btree -
		-dbtree -
		dbtree -
		-ddbtree -
		ddbtree -
		-rbtree -
		BTREE -
		DB_BTREE -
		DB_RBTREE -
		RBTREE -
		bt -
		btree -
		db_btree -
		db_rbtree -
		rbt -
		rbtree { return "-btree" }

		-dhash -
		-ddhash -
		-hash -
		DB_HASH -
		HASH -
		dhash -
		ddhash -
		db_hash -
		h -
		hash { return "-hash" }

		-queue -
		DB_QUEUE -
		QUEUE -
		db_queue -
		q -
		qam -
		queue { return "-queue" }

		-queueextent -
		QUEUEEXTENT -
		qe -
		qamext -
		-queueext -
		queueextent -
		queueext { return "-queue" }

		-frecno -
		-recno -
		-rrecno -
		DB_FRECNO -
		DB_RECNO -
		DB_RRECNO -
		FRECNO -
		RECNO -
		RRECNO -
		db_frecno -
		db_recno -
		db_rrecno -
		frec -
		frecno -
		rec -
		recno -
		rrec -
		rrecno { return "-recno" }

		default { error "FAIL:[timestamp] $method: unknown method" }
	}
}

proc split_encargs { largs encargsp } {
	global encrypt
	upvar $encargsp e
	set eindex [lsearch $largs "-encrypta*"]
	if { $eindex == -1 } {
		set e ""
		set newl $largs
	} else {
		set eend [expr $eindex + 1]
		set e [lrange $largs $eindex $eend]
		set newl [lreplace $largs $eindex $eend "-encrypt"]
	}
	return $newl
}

proc convert_encrypt { largs } {
	global encrypt
	global old_encrypt

	set old_encrypt $encrypt
	set encrypt 0
	if { [lsearch $largs "-encrypt*"] != -1 } {
		set encrypt 1
	}
}

# If recno-with-renumbering or btree-with-renumbering is specified, then
# fix the arguments to specify the DB_RENUMBER/DB_RECNUM option for the
# -flags argument.
proc convert_args { method {largs ""} } {
	global fixed_len
	global fixed_pad
	global gen_upgrade
	global upgrade_be
	source ./include.tcl

	if { [string first - $largs] == -1 &&\
	    [string compare $largs ""] != 0 &&\
	    [string compare $largs {{}}] != 0 } {
		set errstring "args must contain a hyphen; does this test\
		    have no numeric args?"
		puts "FAIL:[timestamp] $errstring (largs was $largs)"
		return -code return
	}

	convert_encrypt $largs
	if { $gen_upgrade == 1 && $upgrade_be == 1 } {
		append largs " -lorder 4321 "
	} elseif { $gen_upgrade == 1 && $upgrade_be != 1 } {
		append largs " -lorder 1234 "
	}

	if { [is_rrecno $method] == 1 } {
		append largs " -renumber "
	} elseif { [is_rbtree $method] == 1 } {
		append largs " -recnum "
	} elseif { [is_dbtree $method] == 1 } {
		append largs " -dup "
	} elseif { [is_ddbtree $method] == 1 } {
		append largs " -dup "
		append largs " -dupsort "
	} elseif { [is_dhash $method] == 1 } {
		append largs " -dup "
	} elseif { [is_ddhash $method] == 1 } {
		append largs " -dup "
		append largs " -dupsort "
	} elseif { [is_queueext $method] == 1 } {
		append largs " -extent 2 "
	}

	if {[is_fixed_length $method] == 1} {
		append largs " -len $fixed_len -pad $fixed_pad "
	}
	return $largs
}

proc is_btree { method } {
	set names { -btree BTREE DB_BTREE bt btree }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_dbtree { method } {
	set names { -dbtree dbtree }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_ddbtree { method } {
	set names { -ddbtree ddbtree }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_rbtree { method } {
	set names { -rbtree rbtree RBTREE db_rbtree DB_RBTREE rbt }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_recno { method } {
	set names { -recno DB_RECNO RECNO db_recno rec recno}
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_rrecno { method } {
	set names { -rrecno rrecno RRECNO db_rrecno DB_RRECNO rrec }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_frecno { method } {
	set names { -frecno frecno frec FRECNO db_frecno DB_FRECNO}
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_hash { method } {
	set names { -hash DB_HASH HASH db_hash h hash }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_dhash { method } {
	set names { -dhash dhash }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_ddhash { method } {
	set names { -ddhash ddhash }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_queue { method } {
	if { [is_queueext $method] == 1 } {
		return 1
	}

	set names { -queue DB_QUEUE QUEUE db_queue q queue qam }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_queueext { method } {
	set names { -queueextent queueextent QUEUEEXTENT qe qamext \
	    queueext -queueext }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_record_based { method } {
	if { [is_recno $method] || [is_frecno $method] ||
	    [is_rrecno $method] || [is_queue $method] } {
		return 1
	} else {
		return 0
	}
}

proc is_fixed_length { method } {
	if { [is_queue $method] || [is_frecno $method] } {
		return 1
	} else {
		return 0
	}
}

# Sort lines in file $in and write results to file $out.
# This is a more portable alternative to execing the sort command,
# which has assorted issues on NT [#1576].
# The addition of a "-n" argument will sort numerically.
proc filesort { in out { arg "" } } {
	set i [open $in r]

	set ilines {}
	while { [gets $i line] >= 0 } {
		lappend ilines $line
	}

	if { [string compare $arg "-n"] == 0 } {
		set olines [lsort -integer $ilines]
	} else {
		set olines [lsort $ilines]
	}

	close $i

	set o [open $out w]
	foreach line $olines {
		puts $o $line
	}

	close $o
}

# Print lines up to the nth line of infile out to outfile, inclusive.
# The optional beg argument tells us where to start.
proc filehead { n infile outfile { beg 0 } } {
	set in [open $infile r]
	set out [open $outfile w]

	# Sed uses 1-based line numbers, and so we do too.
	for { set i 1 } { $i < $beg } { incr i } {
		if { [gets $in junk] < 0 } {
			break
		}
	}

	for { } { $i <= $n } { incr i } {
		if { [gets $in line] < 0 } {
			break
		}
		puts $out $line
	}

	close $in
	close $out
}

# Remove file (this replaces $RM).
# Usage: fileremove filenames =~ rm;  fileremove -f filenames =~ rm -rf.
proc fileremove { args } {
	set forceflag ""
	foreach a $args {
		if { [string first - $a] == 0 } {
			# It's a flag.  Better be f.
			if { [string first f $a] != 1 } {
				return -code error "bad flag to fileremove"
			} else {
				set forceflag "-force"
			}
		} else {
			eval {file delete $forceflag $a}
		}
	}
}

proc findfail { args } {
	foreach a $args {
		if { [file exists $a] == 0 } {
			continue
		}
		set f [open $a r]
		while { [gets $f line] >= 0 } {
			if { [string first FAIL $line] == 0 } {
				close $f
				return 1
			}
		}
		close $f
	}
	return 0
}

# Sleep for s seconds.
proc tclsleep { s } {
	# On Windows, the system time-of-day clock may update as much
	# as 55 ms late due to interrupt timing.  Don't take any
	# chances;  sleep extra-long so that when tclsleep 1 returns,
	# it's guaranteed to be a new second.
	after [expr $s * 1000 + 56]
}

# Kill a process.
proc tclkill { id } {
	source ./include.tcl

	while { [ catch {exec $KILL -0 $id} ] == 0 } {
		catch {exec $KILL -9 $id}
		tclsleep 5
	}
}

# Compare two files, a la diff.  Returns 1 if non-identical, 0 if identical.
proc filecmp { file_a file_b } {
	set fda [open $file_a r]
	set fdb [open $file_b r]

	set nra 0
	set nrb 0

	# The gets can't be in the while condition because we'll
	# get short-circuit evaluated.
	while { $nra >= 0 && $nrb >= 0 } {
		set nra [gets $fda aline]
		set nrb [gets $fdb bline]

		if { $nra != $nrb || [string compare $aline $bline] != 0} {
			close $fda
			close $fdb
			return 1
		}
	}

	close $fda
	close $fdb
	return 0
}

# Give two SORTED files, one of which is a complete superset of the other,
# extract out the unique portions of the superset and put them in
# the given outfile.
proc fileextract { superset subset outfile } {
	set sup [open $superset r]
	set sub [open $subset r]
	set outf [open $outfile w]

	# The gets can't be in the while condition because we'll
	# get short-circuit evaluated.
	set nrp [gets $sup pline]
	set nrb [gets $sub bline]
	while { $nrp >= 0 } {
		if { $nrp != $nrb || [string compare $pline $bline] != 0} {
			puts $outf $pline
		} else {
			set nrb [gets $sub bline]
		}
		set nrp [gets $sup pline]
	}

	close $sup
	close $sub
	close $outf
	return 0
}

# Verify all .db files in the specified directory.
proc verify_dir { {directory $testdir} \
    { pref "" } { noredo 0 } { quiet 0 } { nodump 0 } { cachesize 0 } } {
	global encrypt
	global passwd

	# If we're doing database verification between tests, we don't
	# want to do verification twice without an intervening cleanup--some
	# test was skipped.  Always verify by default (noredo == 0) so
	# that explicit calls to verify_dir during tests don't require
	# cleanup commands.
	if { $noredo == 1 } {
		if { [file exists $directory/NOREVERIFY] == 1 } {
			if { $quiet == 0 } {
				puts "Skipping verification."
			}
			return
		}
		set f [open $directory/NOREVERIFY w]
		close $f
	}

	if { [catch {glob $directory/*.db} dbs] != 0 } {
		# No files matched
		return
	}
	if { [file exists /dev/stderr] == 1 } {
		set errfilearg "-errfile /dev/stderr "
	} else {
		set errfilearg ""
	}
	set errpfxarg {-errpfx "FAIL: verify" }
	set errarg $errfilearg$errpfxarg
	set ret 0

	# Open an env, so that we have a large enough cache.  Pick
	# a fairly generous default if we haven't specified something else.

	if { $cachesize == 0 } {
		set cachesize [expr 1024 * 1024]
	}
	set encarg ""
	if { $encrypt != 0 } {
		set encarg "-encryptaes $passwd"
	}

	set env [eval {berkdb_env -create -private} $encarg \
	    {-cachesize [list 0 $cachesize 0]}]
	set earg " -env $env $errarg "

	foreach db $dbs {
		if { [catch {eval {berkdb dbverify} $earg $db} res] != 0 } {
			puts $res
			puts "FAIL:[timestamp] Verification of $db failed."
			set ret 1
			continue
		} else {
			error_check_good verify:$db $res 0
			if { $quiet == 0 } {
				puts "${pref}Verification of $db succeeded."
			}
		}

		# Skip the dump if it's dangerous to do it.
		if { $nodump == 0 } {
			if { [catch {eval dumploadtest $db} res] != 0 } {
				puts $res
				puts "FAIL:[timestamp] Dump/load of $db failed."
				set ret 1
				continue
			} else {
				error_check_good dumpload:$db $res 0
				if { $quiet == 0 } {
					puts \
					    "${pref}Dump/load of $db succeeded."
				}
			}
		}
	}

	error_check_good vrfyenv_close [$env close] 0

	return $ret
}

# Is the database handle in $db a master database containing subdbs?
proc check_for_subdbs { db } {
	set stat [$db stat]
	for { set i 0 } { [string length [lindex $stat $i]] > 0 } { incr i } {
		set elem [lindex $stat $i]
		if { [string compare [lindex $elem 0] Flags] == 0 } {
			# This is the list of flags;  look for
			# "subdatabases".
			if { [is_substr [lindex $elem 1] subdatabases] } {
				return 1
			}
		}
	}
	return 0
}

proc dumploadtest { db {subdb ""} } {
	global util_path
	global encrypt
	global passwd

	set newdbname $db-dumpload.db

	# Open original database, or subdb if we have one.
	set dbarg ""
	set utilflag ""
	if { $encrypt != 0 } {
		set dbarg "-encryptany $passwd"
		set utilflag "-P $passwd"
	}
	set max_size [expr 15 * 1024]
	if { [string length $subdb] == 0 } {
		set olddb [eval {berkdb_open -rdonly} $dbarg $db]
		error_check_good olddb($db) [is_valid_db $olddb] TRUE

		if { [check_for_subdbs $olddb] } {
			# If $db has subdatabases, dumploadtest each one
			# separately.
			set oc [$olddb cursor]
			error_check_good orig_cursor($db) \
	    		    [is_valid_cursor $oc $olddb] TRUE

			for { set dbt [$oc get -first] } \
			    { [llength $dbt] > 0 } \
			    { set dbt [$oc get -next] } {
				set subdb [lindex [lindex $dbt 0] 0]

            	   		# Skip any files over this size. The problem is
            	   		# that when when we dump/load it, files that are
            	   		# too big result in E2BIG errors because the
            	   		# arguments to db_dump are too long.  64K seems
            	   		# to be the limit (on FreeBSD), cut it to 32K
            	   		# just to be safe.
				if {[string length $subdb] < $max_size && \
				    [string length $subdb] != 0} {
					dumploadtest $db $subdb
				}
			}
			error_check_good oldcclose [$oc close] 0
			error_check_good olddbclose [$olddb close] 0
			return 0
		}
		# No subdatabase
		set have_subdb 0
	} else {
		set olddb [eval {berkdb_open -rdonly} $dbarg {$db $subdb}]
		error_check_good olddb($db) [is_valid_db $olddb] TRUE

		set have_subdb 1
	}

	# Do a db_dump test.  Dump/load each file.
	if { $have_subdb } {
		set rval [catch {eval {exec $util_path/db_dump} $utilflag -k \
		    -s {$subdb} $db | \
		    $util_path/db_load $utilflag $newdbname} res]
	} else {
		set rval [catch {eval {exec $util_path/db_dump} $utilflag -k \
		    $db | $util_path/db_load $utilflag $newdbname} res]
	}
	error_check_good db_dump/db_load($db:$res) $rval 0

	# Now open new database.
	set newdb [eval {berkdb_open -rdonly} $dbarg $newdbname]
	error_check_good newdb($db) [is_valid_db $newdb] TRUE

	# Walk through olddb and newdb and make sure their contents
	# are identical.
	set oc [$olddb cursor]
	set nc [$newdb cursor]
	error_check_good orig_cursor($db) \
	    [is_valid_cursor $oc $olddb] TRUE
	error_check_good new_cursor($db) \
	    [is_valid_cursor $nc $newdb] TRUE

	for { set odbt [$oc get -first] } { [llength $odbt] > 0 } \
	    { set odbt [$oc get -next] } {
		set ndbt [$nc get -get_both \
		    [lindex [lindex $odbt 0] 0] [lindex [lindex $odbt 0] 1]]
		error_check_good db_compare($db/$newdbname) $ndbt $odbt
	}

	for { set ndbt [$nc get -first] } { [llength $ndbt] > 0 } \
	    { set ndbt [$nc get -next] } {
		set odbt [$oc get -get_both \
		    [lindex [lindex $ndbt 0] 0] [lindex [lindex $ndbt 0] 1]]
		error_check_good db_compare_back($db) $odbt $ndbt
	}

	error_check_good orig_cursor_close($db) [$oc close] 0
	error_check_good new_cursor_close($db) [$nc close] 0

	error_check_good orig_db_close($db) [$olddb close] 0
	error_check_good new_db_close($db) [$newdb close] 0

	eval berkdb dbremove $dbarg $newdbname

	return 0
}

# Generate randomly ordered, guaranteed-unique four-character strings that can
# be used to differentiate duplicates without creating duplicate duplicates.
# (test031 & test032) randstring_init is required before the first call to
# randstring and initializes things for up to $i distinct strings;  randstring
# gets the next string.
proc randstring_init { i } {
	global rs_int_list alphabet

	# Fail if we can't generate sufficient unique strings.
	if { $i > [expr 26 * 26 * 26 * 26] } {
		set errstring\
		    "Duplicate set too large for random string generator"
		puts "FAIL:[timestamp] $errstring"
		return -code return $errstring
	}

	set rs_int_list {}

	# generate alphabet array
	for { set j 0 } { $j < 26 } { incr j } {
		set a($j) [string index $alphabet $j]
	}

	# Generate a list with $i elements, { aaaa, aaab, ... aaaz, aaba ...}
	for { set d1 0 ; set j 0 } { $d1 < 26 && $j < $i } { incr d1 } {
		for { set d2 0 } { $d2 < 26 && $j < $i } { incr d2 } {
			for { set d3 0 } { $d3 < 26 && $j < $i } { incr d3 } {
				for { set d4 0 } { $d4 < 26 && $j < $i } \
				    { incr d4 } {
					lappend rs_int_list \
						$a($d1)$a($d2)$a($d3)$a($d4)
					incr j
				}
			}
		}
	}

	# Randomize the list.
	set rs_int_list [randomize_list $rs_int_list]
}

# Randomize a list.  Returns a randomly-reordered copy of l.
proc randomize_list { l } {
	set i [llength $l]

	for { set j 0 } { $j < $i } { incr j } {
		# Pick a random element from $j to the end
		set k [berkdb random_int $j [expr $i - 1]]

		# Swap it with element $j
		set t1 [lindex $l $j]
		set t2 [lindex $l $k]

		set l [lreplace $l $j $j $t2]
		set l [lreplace $l $k $k $t1]
	}

	return $l
}

proc randstring {} {
	global rs_int_list

	if { [info exists rs_int_list] == 0 || [llength $rs_int_list] == 0 } {
		set errstring "randstring uninitialized or used too often"
		puts "FAIL:[timestamp] $errstring"
		return -code return $errstring
	}

	set item [lindex $rs_int_list 0]
	set rs_int_list [lreplace $rs_int_list 0 0]

	return $item
}

# Takes a variable-length arg list, and returns a list containing the list of
# the non-hyphenated-flag arguments, followed by a list of each alphanumeric
# flag it finds.
proc extractflags { args } {
	set inflags 1
	set flags {}
	while { $inflags == 1 } {
		set curarg [lindex $args 0]
		if { [string first "-" $curarg] == 0 } {
			set i 1
			while {[string length [set f \
			    [string index $curarg $i]]] > 0 } {
				incr i
				if { [string compare $f "-"] == 0 } {
					set inflags 0
					break
				} else {
					lappend flags $f
				}
			}
			set args [lrange $args 1 end]
		} else {
			set inflags 0
		}
	}
	return [list $args $flags]
}

# Wrapper for berkdb open, used throughout the test suite so that we can
# set an errfile/errpfx as appropriate.
proc berkdb_open { args } {
	global is_envmethod

	if { [info exists is_envmethod] == 0 } {
		set is_envmethod 0
	}

	set errargs {}
	if { $is_envmethod == 0 && [file exists /dev/stderr] == 1 } {
		append errargs " -errfile /dev/stderr "
		append errargs " -errpfx \\F\\A\\I\\L"
	}

	eval {berkdb open} $errargs $args
}

# Version without errpfx/errfile, used when we're expecting a failure.
proc berkdb_open_noerr { args } {
	eval {berkdb open} $args
}

# Wrapper for berkdb env, used throughout the test suite so that we can
# set an errfile/errpfx as appropriate.
proc berkdb_env { args } {
	global is_envmethod

	if { [info exists is_envmethod] == 0 } {
		set is_envmethod 0
	}

	set errargs {}
	if { $is_envmethod == 0 && [file exists /dev/stderr] == 1 } {
		append errargs " -errfile /dev/stderr "
		append errargs " -errpfx \\F\\A\\I\\L"
	}

	eval {berkdb env} $errargs $args
}

# Version without errpfx/errfile, used when we're expecting a failure.
proc berkdb_env_noerr { args } {
	eval {berkdb env} $args
}

proc check_handles { {outf stdout} } {
	global ohandles

	set handles [berkdb handles]
	if {[llength $handles] != [llength $ohandles]} {
		puts $outf "WARNING: Open handles during cleanup: $handles"
	}
	set ohandles $handles
}

proc open_handles { } {
	return [llength [berkdb handles]]
}

proc move_file_extent { dir dbfile tag op } {
	set curfiles [get_extfiles $dir $dbfile ""]
	set tagfiles [get_extfiles $dir $dbfile $tag]
	#
	# We want to copy or rename only those that have been saved,
	# so delete all the current extent files so that we don't
	# end up with extra ones we didn't restore from our saved ones.
	foreach extfile $curfiles {
		file delete -force $extfile
	}
	foreach extfile $tagfiles {
		set i [string last "." $extfile]
		incr i
		set extnum [string range $extfile $i end]
		set dbq [make_ext_filename $dir $dbfile $extnum]
		#
		# We can either copy or rename
		#
		file $op -force $extfile $dbq
	}
}

proc copy_extent_file { dir dbfile tag { op copy } } {
	set files [get_extfiles $dir $dbfile ""]
	foreach extfile $files {
		set i [string last "." $extfile]
		incr i
		set extnum [string range $extfile $i end]
		file $op -force $extfile $dir/__dbq.$dbfile.$tag.$extnum
	}
}

proc get_extfiles { dir dbfile tag } {
	if { $tag == "" } {
		set filepat $dir/__dbq.$dbfile.\[0-9\]*
	} else {
		set filepat $dir/__dbq.$dbfile.$tag.\[0-9\]*
	}
	return [glob -nocomplain -- $filepat]
}

proc make_ext_filename { dir dbfile extnum } {
	return $dir/__dbq.$dbfile.$extnum
}

# All pids for Windows 9X are negative values.  When we want to have
# unsigned int values, unique to the process, we'll take the absolute
# value of the pid.  This avoids unsigned/signed mistakes, yet
# guarantees uniqueness, since each system has pids that are all
# either positive or negative.
#
proc sanitized_pid { } {
	set mypid [pid]
	if { $mypid < 0 } {
		set mypid [expr - $mypid]
	}
	puts "PID: [pid] $mypid\n"
	return $mypid
}

#
# Extract the page size field from a stat record.  Return -1 if
# none is found.
#
proc get_pagesize { stat } {
	foreach field $stat {
		set title [lindex $field 0]
		if {[string compare $title "Page size"] == 0} {
			return [lindex $field 1]
		}
	}
	return -1
}

# Get a globbed list of source files and executables to use as large
# data items in overflow page tests.
proc get_file_list { {small 0} } {
	global is_windows_test
	global is_qnx_test
	global src_root

	if { $is_qnx_test } {
		set small 1
	}
	if { $small && $is_windows_test } {
		return [glob $src_root/*/*.c */env*.obj]
	} elseif { $small } {
		return [glob $src_root/*/*.c ./env*.o]
	} elseif { $is_windows_test } {
		return \
		    [glob $src_root/*/*.c */*.obj */libdb??.dll */libdb??d.dll]
	} else {
		return [glob $src_root/*/*.c ./*.o ./.libs/libdb-?.?.s?]
	}
}

proc is_cdbenv { env } {
	set sys [$env attributes]
	if { [lsearch $sys -cdb] != -1 } {
		return 1
	} else {
		return 0
	}
}

proc is_lockenv { env } {
	set sys [$env attributes]
	if { [lsearch $sys -lock] != -1 } {
		return 1
	} else {
		return 0
	}
}

proc is_logenv { env } {
	set sys [$env attributes]
	if { [lsearch $sys -log] != -1 } {
		return 1
	} else {
		return 0
	}
}

proc is_mpoolenv { env } {
	set sys [$env attributes]
	if { [lsearch $sys -mpool] != -1 } {
		return 1
	} else {
		return 0
	}
}

proc is_rpcenv { env } {
	set sys [$env attributes]
	if { [lsearch $sys -rpc] != -1 } {
		return 1
	} else {
		return 0
	}
}

proc is_secenv { env } {
	set sys [$env attributes]
	if { [lsearch $sys -crypto] != -1 } {
		return 1
	} else {
		return 0
	}
}

proc is_txnenv { env } {
	set sys [$env attributes]
	if { [lsearch $sys -txn] != -1 } {
		return 1
	} else {
		return 0
	}
}

proc get_home { env } {
	set sys [$env attributes]
	set h [lsearch $sys -home]
	if { $h == -1 } {
		return NULL
	}
	incr h
	return [lindex $sys $h]
}

proc reduce_dups { nent ndp } {
	upvar $nent nentries
	upvar $ndp ndups

	# If we are using a txnenv, assume it is using
	# the default maximum number of locks, cut back
	# so that we don't run out of locks.  Reduce
	# by 25% until we fit.
	#
	while { [expr $nentries * $ndups] > 5000 } {
		set nentries [expr ($nentries / 4) * 3]
		set ndups [expr ($ndups / 4) * 3]
	}
}

proc getstats { statlist field } {
	foreach pair $statlist {
		set txt [lindex $pair 0]
		if { [string equal $txt $field] == 1 } {
			return [lindex $pair 1]
		}
	}
	return -1
}

proc big_endian { } {
	global tcl_platform
	set e $tcl_platform(byteOrder)
	if { [string compare $e littleEndian] == 0 } {
		return 0
	} elseif { [string compare $e bigEndian] == 0 } {
		return 1
	} else {
		error "FAIL: Unknown endianness $e"
	}
}
