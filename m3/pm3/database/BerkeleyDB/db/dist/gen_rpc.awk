#
# $Id$
# Awk script for generating client/server RPC code.
#
# This awk script generates most of the RPC routines for DB client/server
# use. It also generates a template for server and client procedures.  These
# functions must still be edited, but are highly stylized and the initial
# template gets you a fair way along the path).
#
# This awk script requires that these variables be set when it is called:
#
#	major		-- Major version number
#	minor		-- Minor version number
#	xidsize		-- size of GIDs
#	client_file	-- the C source file being created for client code
#	ctmpl_file	-- the C template file being created for client code
#	sed_file	-- the sed file created to alter server proc code
#	server_file	-- the C source file being created for server code
#	stmpl_file	-- the C template file being created for server code
#	xdr_file	-- the XDR message file created
#
# And stdin must be the input file that defines the RPC setup.
BEGIN {
	if (major == "" || minor == "" || xidsize == "" ||
	    client_file == "" || ctmpl_file == "" ||
	    sed_file == "" || server_file == "" ||
	    stmpl_file == "" || xdr_file == "") {
		print "Usage: gen_rpc.awk requires these variables be set:"
		print "\tmajor\t-- Major version number"
		print "\tminor\t-- Minor version number"
		print "\txidsize\t-- GID size"
		print "\tclient_file\t-- the client C source file being created"
		print "\tctmpl_file\t-- the client template file being created"
		print "\tsed_file\t-- the sed command file being created"
		print "\tserver_file\t-- the server C source file being created"
		print "\tstmpl_file\t-- the server template file being created"
		print "\txdr_file\t-- the XDR message file being created"
		error = 1; exit
	}

	FS="\t\t*"
	CFILE=client_file
	printf("/* Do not edit: automatically built by gen_rpc.awk. */\n") \
	    > CFILE

	TFILE = ctmpl_file
	printf("/* Do not edit: automatically built by gen_rpc.awk. */\n") \
	    > TFILE

	SFILE = server_file
	printf("/* Do not edit: automatically built by gen_rpc.awk. */\n") \
	    > SFILE

	# Server procedure template and a sed file to massage an existing
	# template source file to change args.
	# SEDFILE should be same name as PFILE but .c
	#
	PFILE = stmpl_file
	SEDFILE = sed_file
	printf("") > SEDFILE
	printf("/* Do not edit: automatically built by gen_rpc.awk. */\n") \
	    > PFILE

	XFILE = xdr_file
	printf("/* Do not edit: automatically built by gen_rpc.awk. */\n") \
	    > XFILE
	nendlist = 1;
}
END {
	printf("#endif /* HAVE_RPC */\n") >> CFILE
	printf("#endif /* HAVE_RPC */\n") >> TFILE
	printf("program DB_RPC_SERVERPROG {\n") >> XFILE
	printf("\tversion DB_RPC_SERVERVERS {\n") >> XFILE

	for (i = 1; i < nendlist; ++i)
		printf("\t\t%s;\n", endlist[i]) >> XFILE

	printf("\t} = %d%03d;\n", major, minor) >> XFILE
	printf("} = 351457;\n") >> XFILE
}

/^[	 ]*BEGIN/ {
	name = $2;
	nofunc_code = 0;
	funcvars = 0;
	ret_code = 0;
	if ($3 == "NOFUNC")
		nofunc_code = 1;
	if ($3 == "RETCODE")
		ret_code = 1;

	nvars = 0;
	rvars = 0;
	newvars = 0;
	db_handle = 0;
	env_handle = 0;
	dbc_handle = 0;
	txn_handle = 0;
	mp_handle = 0;
	dbt_handle = 0;
	xdr_free = 0;
}
/^[	 ]*ARG/ {
	rpc_type[nvars] = $2;
	c_type[nvars] = $3;
	pr_type[nvars] = $3;
	args[nvars] = $4;
	func_arg[nvars] = 0;
	if (rpc_type[nvars] == "LIST") {
		list_type[nvars] = $5;
	} else
		list_type[nvars] = 0;

	if (c_type[nvars] == "DBT *")
		dbt_handle = 1;

	if (c_type[nvars] == "DB_ENV *") {
		ctp_type[nvars] = "CT_ENV";
		env_handle = 1;
		env_idx = nvars;
	}

	if (c_type[nvars] == "DB *") {
		ctp_type[nvars] = "CT_DB";
		if (db_handle != 1) {
			db_handle = 1;
			db_idx = nvars;
		}
	}

	if (c_type[nvars] == "DBC *") {
		ctp_type[nvars] = "CT_CURSOR";
		dbc_handle = 1;
		dbc_idx = nvars;
	}

	if (c_type[nvars] == "DB_TXN *") {
		ctp_type[nvars] = "CT_TXN";
		txn_handle = 1;
		txn_idx = nvars;
	}

	if (c_type[nvars] == "DB_MPOOLFILE *") {
		mp_handle = 1;
		mp_idx = nvars;
	}

	++nvars;
}
/^[	 ]*FUNCPROT/ {
	pr_type[nvars] = $2;
}
/^[	 ]*FUNCARG/ {
	rpc_type[nvars] = "IGNORE";
	c_type[nvars] = $2;
	args[nvars] = sprintf("func%d", funcvars);
	func_arg[nvars] = 1;
	++funcvars;
	++nvars;
}
/^[	 ]*RET/ {
	ret_type[rvars] = $2;
	retc_type[rvars] = $3;
	retargs[rvars] = $4;
	if (ret_type[rvars] == "LIST" || ret_type[rvars] == "DBT") {
		xdr_free = 1;
	}
	if (ret_type[rvars] == "LIST") {
		retlist_type[rvars] = $5;
	} else
		retlist_type[rvars] = 0;

	++rvars;
}
/^[	 ]*END/ {
	#
	# =====================================================
	# File headers, if necessary.
	#
	if (first == 0) {
		printf("#include \"db_config.h\"\n") >> CFILE
		printf("\n") >> CFILE
		printf("#ifdef HAVE_RPC\n") >> CFILE
		printf("#ifndef NO_SYSTEM_INCLUDES\n") >> CFILE
		printf("#include <sys/types.h>\n\n") >> CFILE
		printf("#include <rpc/rpc.h>\n") >> CFILE
		printf("#include <rpc/xdr.h>\n") >> CFILE
		printf("\n") >> CFILE
		printf("#include <string.h>\n") >> CFILE
		printf("#endif\n") >> CFILE
		printf("\n") >> CFILE
		printf("#include \"db_int.h\"\n") >> CFILE
		printf("#include \"dbinc/txn.h\"\n") >> CFILE
		printf("\n") >> CFILE
		printf("#include \"dbinc_auto/db_server.h\"\n") >> CFILE
		printf("#include \"dbinc_auto/rpc_client_ext.h\"\n") >> CFILE
		printf("\n") >> CFILE

		printf("#include \"db_config.h\"\n") >> TFILE
		printf("\n") >> TFILE
		printf("#ifdef HAVE_RPC\n") >> TFILE
		printf("#ifndef NO_SYSTEM_INCLUDES\n") >> TFILE
		printf("#include <sys/types.h>\n") >> TFILE
		printf("#include <rpc/rpc.h>\n") >> TFILE
		printf("\n") >> TFILE
		printf("#include <string.h>\n") >> TFILE
		printf("#endif\n") >> TFILE
		printf("#include \"db_int.h\"\n") >> TFILE
		printf("#include \"dbinc_auto/db_server.h\"\n") >> TFILE
		printf("#include \"dbinc/txn.h\"\n") >> TFILE
		printf("\n") >> TFILE

		printf("#include \"db_config.h\"\n") >> SFILE
		printf("\n") >> SFILE
		printf("#ifndef NO_SYSTEM_INCLUDES\n") >> SFILE
		printf("#include <sys/types.h>\n") >> SFILE
		printf("\n") >> SFILE
		printf("#include <rpc/rpc.h>\n") >> SFILE
		printf("#include <rpc/xdr.h>\n") >> SFILE
		printf("\n") >> SFILE
		printf("#include <string.h>\n") >> SFILE
		printf("#endif\n") >> SFILE
		printf("\n") >> SFILE
		printf("#include \"db_int.h\"\n") >> SFILE
		printf("#include \"dbinc_auto/db_server.h\"\n") >> SFILE
		printf("#include \"dbinc/db_server_int.h\"\n") >> SFILE
		printf("#include \"dbinc_auto/rpc_server_ext.h\"\n") >> SFILE
		printf("\n") >> SFILE

		printf("#include \"db_config.h\"\n") >> PFILE
		printf("\n") >> PFILE
		printf("#ifndef NO_SYSTEM_INCLUDES\n") >> PFILE
		printf("#include <sys/types.h>\n") >> PFILE
		printf("\n") >> PFILE
		printf("#include <rpc/rpc.h>\n") >> PFILE
		printf("\n") >> PFILE
		printf("#include <string.h>\n") >> PFILE
		printf("#endif\n") >> PFILE
		printf("\n") >> PFILE
		printf("#include \"db_int.h\"\n") >> PFILE
		printf("#include \"dbinc_auto/db_server.h\"\n") >> PFILE
		printf("#include \"dbinc/db_server_int.h\"\n") >> PFILE
		printf("#include \"dbinc_auto/rpc_server_ext.h\"\n") >> PFILE
		printf("\n") >> PFILE

		first = 1;
	}
	#
	# =====================================================
	# Generate Client Nofunc code first if necessary
	# NOTE:  This code must be first, because we don't want any
	# other code other than this function, so before we write
	# out to the XDR and server files, we just generate this
	# and move on if this is all we are doing.
	#
	if (nofunc_code == 1) {
		#
		# First time through, put out the general no server and
		# illegal functions.
		#
		if (first_nofunc == 0) {
			printf("static int __dbcl_noserver ") >> CFILE
			printf("__P((DB_ENV *));\n\n") >> CFILE
			printf("static int\n") >> CFILE
			printf("__dbcl_noserver(dbenv)\n") >> CFILE
			printf("\tDB_ENV *dbenv;\n") >> CFILE
			printf("{\n\t__db_err(dbenv,") >> CFILE
			printf(" \"No server environment\");\n") >> CFILE
			printf("\treturn (DB_NOSERVER);\n") >> CFILE
			printf("}\n\n") >> CFILE

			printf("static int __dbcl_rpc_illegal ") >> CFILE
			printf("__P((DB_ENV *, char *));\n\n") >> CFILE
			printf("static int\n") >> CFILE
			printf("__dbcl_rpc_illegal(dbenv, name)\n") >> CFILE
			printf("\tDB_ENV *dbenv;\n\tchar *name;\n") >> CFILE
			printf("{\n\t__db_err(dbenv,") >> CFILE
			printf(" \"%%s method meaningless in an RPC") >> CFILE
			printf(" environment\", name);\n") >> CFILE
			printf("\treturn (__db_eopnotsup(dbenv));\n") >> CFILE
			printf("}\n\n") >> CFILE

			first_nofunc = 1
		}
		#
		# Spit out PUBLIC prototypes.
		#
		pi = 1;
		p[pi++] = sprintf("int __dbcl_%s __P((", name);
		p[pi++] = "";
		for (i = 0; i < nvars; ++i) {
			p[pi++] = pr_type[i];
			p[pi++] = ", ";
		}
		p[pi - 1] = "";
		p[pi++] = "));";
		p[pi] = "";
		proto_format(p, 0, CFILE);

		#
		# Spit out function name/args.
		#
		printf("int\n") >> CFILE
		printf("__dbcl_%s(", name) >> CFILE
		sep = "";
		for (i = 0; i < nvars; ++i) {
			printf("%s%s", sep, args[i]) >> CFILE
			sep = ", ";
		}
		printf(")\n") >> CFILE

		for (i = 0; i < nvars; ++i)
			if (func_arg[i] == 0)
				printf("\t%s %s;\n", c_type[i], args[i]) \
				    >> CFILE
			else
				printf("\t%s;\n", c_type[i]) >> CFILE

		#
		# Call error function and return EINVAL
		#
		printf("{\n") >> CFILE

		#
		# If we don't have a local env, set one.
		#
		if (env_handle == 0) {
			printf("\tDB_ENV *dbenv;\n\n") >> CFILE
			if (db_handle)
				printf("\tdbenv = %s->dbenv;\n", \
				    args[db_idx]) >> CFILE
			else if (dbc_handle)
				printf("\tdbenv = %s->dbp->dbenv;\n", \
				    args[dbc_idx]) >> CFILE
			else if (txn_handle)
				printf("\tdbenv = %s->mgrp->dbenv;\n", \
				    args[txn_idx]) >> CFILE
			else if (mp_handle)
				printf("\tdbenv = %s->dbmp->dbenv;\n", \
				    args[mp_idx]) >> CFILE
			else
				printf("\tdbenv = NULL;\n") >> CFILE
		}
		#
		# Quiet the compiler for all variables.
		#
		# NOTE:  Index 'i' starts at 1, not 0.  Our first arg is
		# the handle we need to get to the env, and we do not want
		# to COMPQUIET that one.
		for (i = 1; i < nvars; ++i) {
			if (rpc_type[i] == "CONST" || rpc_type[i] == "DBT" ||
			    rpc_type[i] == "LIST" || rpc_type[i] == "STRING" ||
			    rpc_type[i] == "GID") {
				printf("\tCOMPQUIET(%s, NULL);\n", args[i]) \
				    >> CFILE
			}
			if (rpc_type[i] == "INT" || rpc_type[i] == "IGNORE" ||
			    rpc_type[i] == "ID") {
				printf("\tCOMPQUIET(%s, 0);\n", args[i]) \
				    >> CFILE
			}
		}

		if (!env_handle) {
			printf("\treturn (__dbcl_rpc_illegal(dbenv, ") >> CFILE
			printf("\"%s\"));\n", name) >> CFILE
		} else
			printf("\treturn (__dbcl_rpc_illegal(%s, \"%s\"));\n", \
			    args[env_idx], name) >> CFILE
		printf("}\n\n") >> CFILE

		next;
	}

	#
	# =====================================================
	# XDR messages.
	#
	printf("\n") >> XFILE
	printf("struct __%s_msg {\n", name) >> XFILE
	for (i = 0; i < nvars; ++i) {
		if (rpc_type[i] == "LIST") {
			if (list_type[i] == "GID") {
				printf("\topaque %s<>;\n", args[i]) >> XFILE
			} else {
				printf("\tunsigned int %s<>;\n", args[i]) >> XFILE
			}
		}
		if (rpc_type[i] == "ID") {
			printf("\tunsigned int %scl_id;\n", args[i]) >> XFILE
		}
		if (rpc_type[i] == "STRING") {
			printf("\tstring %s<>;\n", args[i]) >> XFILE
		}
		if (rpc_type[i] == "GID") {
			printf("\topaque %s[%d];\n", args[i], xidsize) >> XFILE
		}
		if (rpc_type[i] == "INT") {
			printf("\tunsigned int %s;\n", args[i]) >> XFILE
		}
		if (rpc_type[i] == "DBT") {
			printf("\tunsigned int %sdlen;\n", args[i]) >> XFILE
			printf("\tunsigned int %sdoff;\n", args[i]) >> XFILE
			printf("\tunsigned int %sulen;\n", args[i]) >> XFILE
			printf("\tunsigned int %sflags;\n", args[i]) >> XFILE
			printf("\topaque %sdata<>;\n", args[i]) >> XFILE
		}
	}
	printf("};\n") >> XFILE

	printf("\n") >> XFILE
	#
	# Generate the reply message
	#
	printf("struct __%s_reply {\n", name) >> XFILE
	printf("\tint status;\n") >> XFILE
	for (i = 0; i < rvars; ++i) {
		if (ret_type[i] == "ID") {
			printf("\tunsigned int %scl_id;\n", retargs[i]) >> XFILE
		}
		if (ret_type[i] == "STRING") {
			printf("\tstring %s<>;\n", retargs[i]) >> XFILE
		}
		if (ret_type[i] == "INT") {
			printf("\tunsigned int %s;\n", retargs[i]) >> XFILE
		}
		if (ret_type[i] == "DBL") {
			printf("\tdouble %s;\n", retargs[i]) >> XFILE
		}
		if (ret_type[i] == "DBT") {
			printf("\topaque %sdata<>;\n", retargs[i]) >> XFILE
		}
		if (ret_type[i] == "LIST") {
			if (retlist_type[i] == "GID") {
				printf("\topaque %s<>;\n", retargs[i]) >> XFILE
			} else {
				printf("\tunsigned int %s<>;\n", retargs[i]) >> XFILE
			}
		}
	}
	printf("};\n") >> XFILE

	endlist[nendlist] = \
	    sprintf("__%s_reply __DB_%s(__%s_msg) = %d", \
		name, name, name, nendlist);
	nendlist++;
	#
	# =====================================================
	# Server functions.
	#
	# First spit out PUBLIC prototypes for server functions.
	#
	p[1] = sprintf("__%s_reply *__db_%s_%d%03d __P((__%s_msg *, struct svc_req *));",
	    name, name, major, minor, name);
	p[2] = "";
	proto_format(p, 0, SFILE);

	printf("__%s_reply *\n", name) >> SFILE
	printf("__db_%s_%d%03d(msg, req)\n", name, major, minor) >> SFILE
	printf("\t__%s_msg *msg;\n", name) >> SFILE;
	printf("\tstruct svc_req *req;\n", name) >> SFILE;
	printf("{\n") >> SFILE
	printf("\tstatic __%s_reply reply; /* must be static */\n", \
	    name) >> SFILE
	if (xdr_free) {
		printf("\tstatic int __%s_free = 0; /* must be static */\n\n", \
		    name) >> SFILE
	}
	printf("\tCOMPQUIET(req, NULL);\n", name) >> SFILE
	if (xdr_free) {
		printf("\tif (__%s_free)\n", name) >> SFILE
		printf("\t\txdr_free((xdrproc_t)xdr___%s_reply, (void *)&reply);\n", \
		    name) >> SFILE
		printf("\t__%s_free = 0;\n", name) >> SFILE
		printf("\n\t/* Reinitialize allocated fields */\n") >> SFILE
		for (i = 0; i < rvars; ++i) {
			if (ret_type[i] == "LIST") {
				printf("\treply.%s.%s_val = NULL;\n", \
				    retargs[i], retargs[i]) >> SFILE
			}
			if (ret_type[i] == "DBT") {
				printf("\treply.%sdata.%sdata_val = NULL;\n", \
				    retargs[i], retargs[i]) >> SFILE
			}
		}
	}

	need_out = 0;
	#
	# Compose server proc to call.  Decompose message components as args.
	#
	printf("\n\t__%s_proc(", name) >> SFILE
	sep = "";
	for (i = 0; i < nvars; ++i) {
		if (rpc_type[i] == "IGNORE") {
			continue;
		}
		if (rpc_type[i] == "ID") {
			printf("%smsg->%scl_id", sep, args[i]) >> SFILE
		}
		if (rpc_type[i] == "STRING") {
			printf("%s(*msg->%s == '\\0') ? NULL : msg->%s", \
			    sep, args[i], args[i]) >> SFILE
		}
		if (rpc_type[i] == "GID") {
			printf("%smsg->%s", sep, args[i]) >> SFILE
		}
		if (rpc_type[i] == "INT") {
			printf("%smsg->%s", sep, args[i]) >> SFILE
		}
		if (rpc_type[i] == "LIST") {
			printf("%smsg->%s.%s_val", \
			    sep, args[i], args[i]) >> SFILE
			printf("%smsg->%s.%s_len", \
			    sep, args[i], args[i]) >> SFILE
		}
		if (rpc_type[i] == "DBT") {
			printf("%smsg->%sdlen", sep, args[i]) >> SFILE
			sep = ",\n\t    ";
			printf("%smsg->%sdoff", sep, args[i]) >> SFILE
			printf("%smsg->%sulen", sep, args[i]) >> SFILE
			printf("%smsg->%sflags", sep, args[i]) >> SFILE
			printf("%smsg->%sdata.%sdata_val", \
			    sep, args[i], args[i]) >> SFILE
			printf("%smsg->%sdata.%sdata_len", \
			    sep, args[i], args[i]) >> SFILE
		}
		sep = ",\n\t    ";
	}
	printf("%s&reply", sep) >> SFILE
	if (xdr_free)
		printf("%s&__%s_free);\n", sep, name) >> SFILE
	else
		printf(");\n\n") >> SFILE
	if (need_out) {
		printf("\nout:\n") >> SFILE
	}
	printf("\treturn (&reply);\n") >> SFILE
	printf("}\n\n") >> SFILE

	#
	# =====================================================
	# Generate Procedure Template Server code
	#
	# Produce SED file commands if needed at the same time
	#
	# Spit out comment, prototype, function name and arg list.
	#
	printf("/^\\/\\* BEGIN __%s_proc/,/^\\/\\* END __%s_proc/c\\\n", \
	    name, name) >> SEDFILE

	printf("/* BEGIN __%s_proc */\n", name) >> PFILE
	printf("/* BEGIN __%s_proc */\\\n", name) >> SEDFILE

	pi = 1;
	p[pi++] = sprintf("void __%s_proc __P((", name);
	p[pi++] = "";
	for (i = 0; i < nvars; ++i) {
		if (rpc_type[i] == "IGNORE")
			continue;
		if (rpc_type[i] == "ID") {
			p[pi++] = "long";
			p[pi++] = ", ";
		}
		if (rpc_type[i] == "STRING") {
			p[pi++] = "char *";
			p[pi++] = ", ";
		}
		if (rpc_type[i] == "GID") {
			p[pi++] = "u_int8_t *";
			p[pi++] = ", ";
		}
		if (rpc_type[i] == "INT") {
			p[pi++] = "u_int32_t";
			p[pi++] = ", ";
		}
		if (rpc_type[i] == "LIST" && list_type[i] == "GID") {
			p[pi++] = "u_int8_t *";
			p[pi++] = ", ";
			p[pi++] = "u_int32_t";
			p[pi++] = ", ";
		}
		if (rpc_type[i] == "LIST" && list_type[i] == "INT") {
			p[pi++] = "u_int32_t *";
			p[pi++] = ", ";
			p[pi++] = "u_int32_t";
			p[pi++] = ", ";
		}
		if (rpc_type[i] == "LIST" && list_type[i] == "ID") {
			p[pi++] = "u_int32_t *";
			p[pi++] = ", ";
			p[pi++] = "u_int32_t";
			p[pi++] = ", ";
		}
		if (rpc_type[i] == "DBT") {
			p[pi++] = "u_int32_t";
			p[pi++] = ", ";
			p[pi++] = "u_int32_t";
			p[pi++] = ", ";
			p[pi++] = "u_int32_t";
			p[pi++] = ", ";
			p[pi++] = "u_int32_t";
			p[pi++] = ", ";
			p[pi++] = "void *";
			p[pi++] = ", ";
			p[pi++] = "u_int32_t";
			p[pi++] = ", ";
		}
	}
	p[pi++] = sprintf("__%s_reply *", name);
	if (xdr_free) {
		p[pi++] = ", ";
		p[pi++] = "int *));";
	} else {
		p[pi++] = "";
		p[pi++] = "));";
	}
	p[pi++] = "";
	proto_format(p, 1, SEDFILE);

	printf("void\n") >> PFILE
	printf("void\\\n") >> SEDFILE
	printf("__%s_proc(", name) >> PFILE
	printf("__%s_proc(", name) >> SEDFILE
	sep = "";
	argcount = 0;
	for (i = 0; i < nvars; ++i) {
		argcount++;
		split_lines();
		if (argcount == 0) {
			sep = "";
		}
		if (rpc_type[i] == "IGNORE")
			continue;
		if (rpc_type[i] == "ID") {
			printf("%s%scl_id", sep, args[i]) >> PFILE
			printf("%s%scl_id", sep, args[i]) >> SEDFILE
		}
		if (rpc_type[i] == "STRING") {
			printf("%s%s", sep, args[i]) >> PFILE
			printf("%s%s", sep, args[i]) >> SEDFILE
		}
		if (rpc_type[i] == "GID") {
			printf("%s%s", sep, args[i]) >> PFILE
			printf("%s%s", sep, args[i]) >> SEDFILE
		}
		if (rpc_type[i] == "INT") {
			printf("%s%s", sep, args[i]) >> PFILE
			printf("%s%s", sep, args[i]) >> SEDFILE
		}
		if (rpc_type[i] == "LIST") {
			printf("%s%s", sep, args[i]) >> PFILE
			printf("%s%s", sep, args[i]) >> SEDFILE
			argcount++;
			split_lines();
			if (argcount == 0) {
				sep = "";
			} else {
				sep = ", ";
			}
			printf("%s%slen", sep, args[i]) >> PFILE
			printf("%s%slen", sep, args[i]) >> SEDFILE
		}
		if (rpc_type[i] == "DBT") {
			printf("%s%sdlen", sep, args[i]) >> PFILE
			printf("%s%sdlen", sep, args[i]) >> SEDFILE
			sep = ", ";
			argcount++;
			split_lines();
			if (argcount == 0) {
				sep = "";
			} else {
				sep = ", ";
			}
			printf("%s%sdoff", sep, args[i]) >> PFILE
			printf("%s%sdoff", sep, args[i]) >> SEDFILE
			argcount++;
			split_lines();
			if (argcount == 0) {
				sep = "";
			} else {
				sep = ", ";
			}
			printf("%s%sulen", sep, args[i]) >> PFILE
			printf("%s%sulen", sep, args[i]) >> SEDFILE
			argcount++;
			split_lines();
			if (argcount == 0) {
				sep = "";
			} else {
				sep = ", ";
			}
			printf("%s%sflags", sep, args[i]) >> PFILE
			printf("%s%sflags", sep, args[i]) >> SEDFILE
			argcount++;
			split_lines();
			if (argcount == 0) {
				sep = "";
			} else {
				sep = ", ";
			}
			printf("%s%sdata", sep, args[i]) >> PFILE
			printf("%s%sdata", sep, args[i]) >> SEDFILE
			argcount++;
			split_lines();
			if (argcount == 0) {
				sep = "";
			} else {
				sep = ", ";
			}
			printf("%s%ssize", sep, args[i]) >> PFILE
			printf("%s%ssize", sep, args[i]) >> SEDFILE
		}
		sep = ", ";
	}
	printf("%sreplyp",sep) >> PFILE
	printf("%sreplyp",sep) >> SEDFILE
	if (xdr_free) {
		printf("%sfreep)\n",sep) >> PFILE
		printf("%sfreep)\\\n",sep) >> SEDFILE
	} else {
		printf(")\n") >> PFILE
		printf(")\\\n") >> SEDFILE
	}
	#
	# Spit out arg types/names;
	#
	for (i = 0; i < nvars; ++i) {
		if (rpc_type[i] == "ID") {
			printf("\tlong %scl_id;\n", args[i]) >> PFILE
			printf("\\\tlong %scl_id;\\\n", args[i]) >> SEDFILE
		}
		if (rpc_type[i] == "STRING") {
			printf("\tchar *%s;\n", args[i]) >> PFILE
			printf("\\\tchar *%s;\\\n", args[i]) >> SEDFILE
		}
		if (rpc_type[i] == "GID") {
			printf("\tu_int8_t *%s;\n", args[i]) >> PFILE
			printf("\\\tu_int8_t *%s;\\\n", args[i]) >> SEDFILE
		}
		if (rpc_type[i] == "INT") {
			printf("\tu_int32_t %s;\n", args[i]) >> PFILE
			printf("\\\tu_int32_t %s;\\\n", args[i]) >> SEDFILE
		}
		if (rpc_type[i] == "LIST" && list_type[i] == "GID") {
			printf("\tu_int8_t * %s;\n", args[i]) >> PFILE
			printf("\\\tu_int8_t * %s;\\\n", args[i]) >> SEDFILE
		}
		if (rpc_type[i] == "LIST" && list_type[i] == "INT") {
			printf("\tu_int32_t * %s;\n", args[i]) >> PFILE
			printf("\\\tu_int32_t * %s;\\\n", \
			    args[i]) >> SEDFILE
			printf("\tu_int32_t %ssize;\n", args[i]) >> PFILE
			printf("\\\tu_int32_t %ssize;\\\n", args[i]) >> SEDFILE
		}
		if (rpc_type[i] == "LIST" && list_type[i] == "ID") {
			printf("\tu_int32_t * %s;\n", args[i]) >> PFILE
			printf("\\\tu_int32_t * %s;\\\n", args[i]) \
			    >> SEDFILE
		}
		if (rpc_type[i] == "LIST") {
			printf("\tu_int32_t %slen;\n", args[i]) >> PFILE
			printf("\\\tu_int32_t %slen;\\\n", args[i]) \
			    >> SEDFILE
		}
		if (rpc_type[i] == "DBT") {
			printf("\tu_int32_t %sdlen;\n", args[i]) >> PFILE
			printf("\\\tu_int32_t %sdlen;\\\n", args[i]) >> SEDFILE
			printf("\tu_int32_t %sdoff;\n", args[i]) >> PFILE
			printf("\\\tu_int32_t %sdoff;\\\n", args[i]) >> SEDFILE
			printf("\tu_int32_t %sulen;\n", args[i]) >> PFILE
			printf("\\\tu_int32_t %sulen;\\\n", args[i]) >> SEDFILE
			printf("\tu_int32_t %sflags;\n", args[i]) >> PFILE
			printf("\\\tu_int32_t %sflags;\\\n", args[i]) >> SEDFILE
			printf("\tvoid *%sdata;\n", args[i]) >> PFILE
			printf("\\\tvoid *%sdata;\\\n", args[i]) >> SEDFILE
			printf("\tu_int32_t %ssize;\n", args[i]) >> PFILE
			printf("\\\tu_int32_t %ssize;\\\n", args[i]) >> SEDFILE
		}
	}
	printf("\t__%s_reply *replyp;\n",name) >> PFILE
	printf("\\\t__%s_reply *replyp;\\\n",name) >> SEDFILE
	if (xdr_free) {
		printf("\tint * freep;\n") >> PFILE
		printf("\\\tint * freep;\\\n") >> SEDFILE
	}

	printf("/* END __%s_proc */\n", name) >> PFILE
	printf("/* END __%s_proc */\n", name) >> SEDFILE

	#
	# Function body
	#
	printf("{\n") >> PFILE
	printf("\tint ret;\n") >> PFILE
	for (i = 0; i < nvars; ++i) {
		if (rpc_type[i] == "ID") {
			printf("\t%s %s;\n", c_type[i], args[i]) >> PFILE
			printf("\tct_entry *%s_ctp;\n", args[i]) >> PFILE
		}
	}
	printf("\n") >> PFILE
	for (i = 0; i < nvars; ++i) {
		if (rpc_type[i] == "ID") {
			printf("\tACTIVATE_CTP(%s_ctp, %scl_id, %s);\n", \
			    args[i], args[i], ctp_type[i]) >> PFILE
			printf("\t%s = (%s)%s_ctp->ct_anyp;\n", \
			    args[i], c_type[i], args[i]) >> PFILE
		}
	}
	printf("\n\t/*\n\t * XXX Code goes here\n\t */\n\n") >> PFILE
	printf("\treplyp->status = ret;\n") >> PFILE
	printf("\treturn;\n") >> PFILE
	printf("}\n\n") >> PFILE

	#
	# =====================================================
	# Generate Client code
	#
	# Spit out PUBLIC prototypes.
	#
	pi = 1;
	p[pi++] = sprintf("int __dbcl_%s __P((", name);
	p[pi++] = "";
	for (i = 0; i < nvars; ++i) {
		p[pi++] = pr_type[i];
		p[pi++] = ", ";
	}
	p[pi - 1] = "";
	p[pi++] = "));";
	p[pi] = "";
	proto_format(p, 0, CFILE);

	#
	# Spit out function name/args.
	#
	printf("int\n") >> CFILE
	printf("__dbcl_%s(", name) >> CFILE
	sep = "";
	for (i = 0; i < nvars; ++i) {
		printf("%s%s", sep, args[i]) >> CFILE
		sep = ", ";
	}
	printf(")\n") >> CFILE

	for (i = 0; i < nvars; ++i)
		if (func_arg[i] == 0)
			printf("\t%s %s;\n", c_type[i], args[i]) >> CFILE
		else
			printf("\t%s;\n", c_type[i]) >> CFILE

	printf("{\n") >> CFILE
	printf("\tCLIENT *cl;\n") >> CFILE
	printf("\t__%s_msg msg;\n", name) >> CFILE
	printf("\t__%s_reply *replyp = NULL;\n", name) >> CFILE;
	printf("\tint ret;\n") >> CFILE
	if (!env_handle)
		printf("\tDB_ENV *dbenv;\n") >> CFILE
	#
	# If we are managing a list, we need a few more vars.
	#
	for (i = 0; i < nvars; ++i) {
		if (rpc_type[i] == "LIST") {
			printf("\t%s %sp;\n", c_type[i], args[i]) >> CFILE
			printf("\tint %si;\n", args[i]) >> CFILE
			if (list_type[i] == "GID")
				printf("\tu_int8_t ** %sq;\n", args[i]) >> CFILE
			else
				printf("\tu_int32_t * %sq;\n", args[i]) >> CFILE
		}
	}

	printf("\n") >> CFILE
	printf("\tret = 0;\n") >> CFILE
	if (!env_handle) {
		if (db_handle)
			printf("\tdbenv = %s->dbenv;\n", args[db_idx]) >> CFILE
		else if (dbc_handle)
			printf("\tdbenv = %s->dbp->dbenv;\n", \
			    args[dbc_idx]) >> CFILE
		else if (txn_handle)
			printf("\tdbenv = %s->mgrp->dbenv;\n", \
			    args[txn_idx]) >> CFILE
		else
			printf("\tdbenv = NULL;\n") >> CFILE
		printf("\tif (dbenv == NULL || !RPC_ON(dbenv))\n") \
		    >> CFILE
		printf("\t\treturn (__dbcl_noserver(NULL));\n") >> CFILE
	} else {
		printf("\tif (%s == NULL || !RPC_ON(%s))\n", \
		    args[env_idx], args[env_idx]) >> CFILE
		printf("\t\treturn (__dbcl_noserver(%s));\n", \
		    args[env_idx]) >> CFILE
	}
	printf("\n") >> CFILE

	if (!env_handle)
		printf("\tcl = (CLIENT *)dbenv->cl_handle;\n") >> CFILE
	else
		printf("\tcl = (CLIENT *)%s->cl_handle;\n", \
		    args[env_idx]) >> CFILE

	printf("\n") >> CFILE

	#
	# If there is a function arg, check that it is NULL
	#
	for (i = 0; i < nvars; ++i) {
		if (func_arg[i] != 1)
			continue;
		printf("\tif (%s != NULL) {\n", args[i]) >> CFILE
		if (!env_handle) {
			printf("\t\t__db_err(dbenv, ") >> CFILE
		} else {
			printf("\t\t__db_err(%s, ", args[env_idx]) >> CFILE
		}
		printf("\"User functions not supported in RPC\");\n") >> CFILE
		printf("\t\treturn (EINVAL);\n\t}\n") >> CFILE
	}

	#
	# Compose message components
	#
	for (i = 0; i < nvars; ++i) {
		if (rpc_type[i] == "ID") {
			printf("\tif (%s == NULL)\n", args[i]) >> CFILE
			printf("\t\tmsg.%scl_id = 0;\n\telse\n", \
			    args[i]) >> CFILE
			if (c_type[i] == "DB_TXN *") {
				printf("\t\tmsg.%scl_id = %s->txnid;\n", \
				    args[i], args[i]) >> CFILE
			} else {
				printf("\t\tmsg.%scl_id = %s->cl_id;\n", \
				    args[i], args[i]) >> CFILE
			}
		}
		if (rpc_type[i] == "GID") {
			printf("\tmemcpy(msg.%s, %s, %d);\n", \
			    args[i], args[i], xidsize) >> CFILE
		}
		if (rpc_type[i] == "INT") {
			printf("\tmsg.%s = %s;\n", args[i], args[i]) >> CFILE
		}
		if (rpc_type[i] == "STRING") {
			printf("\tif (%s == NULL)\n", args[i]) >> CFILE
			printf("\t\tmsg.%s = \"\";\n", args[i]) >> CFILE
			printf("\telse\n") >> CFILE
			printf("\t\tmsg.%s = (char *)%s;\n", \
			    args[i], args[i]) >> CFILE
		}
		if (rpc_type[i] == "DBT") {
			printf("\tmsg.%sdlen = %s->dlen;\n", \
			    args[i], args[i]) >> CFILE
			printf("\tmsg.%sdoff = %s->doff;\n", \
			    args[i], args[i]) >> CFILE
			printf("\tmsg.%sulen = %s->ulen;\n", \
			    args[i], args[i]) >> CFILE
			printf("\tmsg.%sflags = %s->flags;\n", \
			    args[i], args[i]) >> CFILE
			printf("\tmsg.%sdata.%sdata_val = %s->data;\n", \
			    args[i], args[i], args[i]) >> CFILE
			printf("\tmsg.%sdata.%sdata_len = %s->size;\n", \
			    args[i], args[i], args[i]) >> CFILE
		}
		if (rpc_type[i] == "LIST") {
			printf("\tfor (%si = 0, %sp = %s; *%sp != 0; ", \
			    args[i], args[i], args[i], args[i]) >> CFILE
			printf(" %si++, %sp++)\n\t\t;\n", args[i], args[i]) \
			    >> CFILE

			#
			# If we are an array of ints, *_len is how many
			# elements.  If we are a GID, *_len is total bytes.
			#
			printf("\tmsg.%s.%s_len = %si",args[i], args[i], \
			    args[i]) >> CFILE
			if (list_type[i] == "GID")
				printf(" * %d;\n", xidsize) >> CFILE
			else
				printf(";\n") >> CFILE
			printf("\tif ((ret = __os_calloc(") >> CFILE
			if (!env_handle)
				printf("dbenv,\n") >> CFILE
			else
				printf("%s,\n", args[env_idx]) >> CFILE
			printf("\t    msg.%s.%s_len,", \
			    args[i], args[i]) >> CFILE
			if (list_type[i] == "GID")
				printf(" 1,") >> CFILE
			else
				printf(" sizeof(u_int32_t),") >> CFILE
			printf(" &msg.%s.%s_val)) != 0)\n",\
			    args[i], args[i], args[i], args[i]) >> CFILE
			printf("\t\treturn (ret);\n") >> CFILE
			printf("\tfor (%sq = msg.%s.%s_val, %sp = %s; ", \
			    args[i], args[i], args[i], \
			    args[i], args[i]) >> CFILE
			printf("%si--; %sq++, %sp++)\n", \
			    args[i], args[i], args[i]) >> CFILE
			printf("\t\t*%sq = ", args[i]) >> CFILE
			if (list_type[i] == "GID")
				printf("*%sp;\n", args[i]) >> CFILE
			if (list_type[i] == "ID")
				printf("(*%sp)->cl_id;\n", args[i]) >> CFILE
			if (list_type[i] == "INT")
				printf("*%sp;\n", args[i]) >> CFILE
		}
	}

	printf("\n") >> CFILE
	printf("\treplyp = __db_%s_%d%03d(&msg, cl);\n", name, major, minor) \
	    >> CFILE
	for (i = 0; i < nvars; ++i) {
		if (rpc_type[i] == "LIST") {
			printf("\t__os_free(") >> CFILE
			if (!env_handle)
				printf("dbenv, ") >> CFILE
			else
				printf("%s, ", args[env_idx]) >> CFILE
			printf("msg.%s.%s_val);\n", args[i], args[i]) >> CFILE
		}
	}
	printf("\tif (replyp == NULL) {\n") >> CFILE
	if (!env_handle) {
		printf("\t\t__db_err(dbenv, ") >> CFILE
		printf("clnt_sperror(cl, \"Berkeley DB\"));\n") >> CFILE
	} else {
		printf("\t\t__db_err(%s, ", args[env_idx]) >> CFILE
		printf("clnt_sperror(cl, \"Berkeley DB\"));\n") >> CFILE
	}
	printf("\t\tret = DB_NOSERVER;\n") >> CFILE
	printf("\t\tgoto out;\n") >> CFILE
	printf("\t}\n") >> CFILE

	if (ret_code == 0) {
		printf("\tret = replyp->status;\n") >> CFILE
	} else {
		printf("\tret = __dbcl_%s_ret(", name) >> CFILE
		sep = "";
		for (i = 0; i < nvars; ++i) {
			printf("%s%s", sep, args[i]) >> CFILE
			sep = ", ";
		}
		printf("%sreplyp);\n", sep) >> CFILE
	}
	printf("out:\n") >> CFILE
	#
	# Free reply if there was one.
	#
	printf("\tif (replyp != NULL)\n") >> CFILE
	printf("\t\txdr_free((xdrproc_t)xdr___%s_reply,",name) >> CFILE
	printf(" (void *)replyp);\n") >> CFILE
	printf("\treturn (ret);\n") >> CFILE
	printf("}\n\n") >> CFILE

	#
	# Generate Client Template code
	#
	if (ret_code) {
		#
		# If we are doing a list, write prototypes
		#
		pi = 1;
		p[pi++] = sprintf("int __dbcl_%s_ret __P((", name);
		p[pi++] = "";
		for (i = 0; i < nvars; ++i) {
			p[pi++] = pr_type[i];
			p[pi++] = ", ";
		}
		p[pi++] = sprintf("__%s_reply *));", name);
		p[pi++] = "";
		proto_format(p, 0, TFILE);

		printf("int\n") >> TFILE
		printf("__dbcl_%s_ret(", name) >> TFILE
		sep = "";
		for (i = 0; i < nvars; ++i) {
			printf("%s%s", sep, args[i]) >> TFILE
			sep = ", ";
		}
		printf("%sreplyp)\n",sep) >> TFILE

		for (i = 0; i < nvars; ++i)
			if (func_arg[i] == 0)
				printf("\t%s %s;\n", c_type[i], args[i]) \
				    >> TFILE
			else
				printf("\t%s;\n", c_type[i]) >> TFILE
		printf("\t__%s_reply *replyp;\n", name) >> TFILE;
		printf("{\n") >> TFILE
		printf("\tint ret;\n") >> TFILE
		#
		# Local vars in template
		#
		for (i = 0; i < rvars; ++i) {
			if (ret_type[i] == "ID" || ret_type[i] == "STRING" ||
			    ret_type[i] == "INT" || ret_type[i] == "DBL") {
				printf("\t%s %s;\n", \
				    retc_type[i], retargs[i]) >> TFILE
			} else if (ret_type[i] == "LIST") {
				if (retlist_type[i] == "GID")
					printf("\tu_int8_t *__db_%s;\n", \
					    retargs[i]) >> TFILE
				if (retlist_type[i] == "ID" ||
				    retlist_type[i] == "INT")
					printf("\tu_int32_t *__db_%s;\n", \
					    retargs[i]) >> TFILE
			} else {
				printf("\t/* %s %s; */\n", \
				    ret_type[i], retargs[i]) >> TFILE
			}
		}
		#
		# Client return code
		#
		printf("\n") >> TFILE
		printf("\tif (replyp->status != 0)\n") >> TFILE
		printf("\t\treturn (replyp->status);\n") >> TFILE
		for (i = 0; i < rvars; ++i) {
			varname = "";
			if (ret_type[i] == "ID") {
				varname = sprintf("%scl_id", retargs[i]);
			}
			if (ret_type[i] == "STRING") {
				varname =  retargs[i];
			}
			if (ret_type[i] == "INT" || ret_type[i] == "DBL") {
				varname =  retargs[i];
			}
			if (ret_type[i] == "DBT") {
				varname = sprintf("%sdata", retargs[i]);
			}
			if (ret_type[i] == "ID" || ret_type[i] == "STRING" ||
			    ret_type[i] == "INT" || ret_type[i] == "DBL") {
				printf("\t%s = replyp->%s;\n", \
				    retargs[i], varname) >> TFILE
			} else if (ret_type[i] == "LIST") {
				printf("\n\t/*\n") >> TFILE
				printf("\t * XXX Handle list\n") >> TFILE
				printf("\t */\n\n") >> TFILE
			} else {
				printf("\t/* Handle replyp->%s; */\n", \
				    varname) >> TFILE
			}
		}
		printf("\n\t/*\n\t * XXX Code goes here\n\t */\n\n") >> TFILE
		printf("\treturn (replyp->status);\n") >> TFILE
		printf("}\n\n") >> TFILE
	}
}

#
# split_lines --
#	Add line separators to pretty-print the output.
function split_lines() {
	if (argcount > 3) {
		# Reset the counter, remove any trailing whitespace from
		# the separator.
		argcount = 0;
		sub("[ 	]$", "", sep)

		printf("%s\n\t\t", sep) >> PFILE
		printf("%s\\\n\\\t\\\t", sep) >> SEDFILE
	}
}

# proto_format --
#	Pretty-print a function prototype.
function proto_format(p, sedfile, OUTPUT)
{
	if (sedfile)
		printf("/*\\\n") >> OUTPUT;
	else
		printf("/*\n") >> OUTPUT;

	s = "";
	for (i = 1; i in p; ++i)
		s = s p[i];

	if (sedfile)
		t = "\\ * PUBLIC: "
	else
		t = " * PUBLIC: "
	if (length(s) + length(t) < 80)
		if (sedfile)
			printf("%s%s", t, s) >> OUTPUT;
		else
			printf("%s%s", t, s) >> OUTPUT;
	else {
		split(s, p, "__P");
		len = length(t) + length(p[1]);
		printf("%s%s", t, p[1]) >> OUTPUT

		n = split(p[2], comma, ",");
		comma[1] = "__P" comma[1];
		for (i = 1; i <= n; i++) {
			if (len + length(comma[i]) > 75) {
				if (sedfile)
					printf(\
					    "\\\n\\ * PUBLIC:     ") >> OUTPUT;
				else
					printf("\n * PUBLIC:     ") >> OUTPUT;
				len = 0;
			}
			printf("%s%s", comma[i], i == n ? "" : ",") >> OUTPUT;
			len += length(comma[i]);
		}
	}
	if (sedfile)
		printf("\\\n\\ */\\\n") >> OUTPUT;
	else
		printf("\n */\n") >> OUTPUT;
	delete p;
}
