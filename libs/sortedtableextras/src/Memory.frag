(* Created on Mon Jan 19 10:14:06 PST 1998 by heydon *)
(* Last modified on Fri Jan 23 10:05:34 PST 1998 by heydon *)

(*
This section describes the memory use of the "SortedTable.Default",
"RedBlackTbl.T", and "SkipListTbl.T" implementations. The results are
summarized in Table~\ref{tbl:mem-use}. For each implementation, we
distinguish the per-element memory size from that of the object
itself. These values do not include standard heap-allocated object
overheads such as typecodes and method tables, but they do include an
extra 1-word cost for storing the number of elements in each
heap-allocated array.

The exact sizes of the data structures used in each implementation are
architecture dependent. For example, the size of a reference (pointer)
is architecture dependent, as are any alignment constraints imposed on
the way records are layed out in memory. Another factor that
contributes to each table's memory usage is the variable size of the
key and value types with which the table is parameterized.

The data in Table~\ref{tbl:mem-use} has thus been parameterized by the
declared maximum number of elements $N$ in the table, the sizes $K$
and $V$ of the key and value types, the size $R$ of a reference, and
the size $W$ of a machine word. For example, on a 64-bit Digital Alpha
machine, $R$ and $W$ are both 8, while on a DECStation, they are both 4.

\begin{table}[htb]
\begin{center}
\begin{tabular}{l|c|c}
Implementation & Per-Element Size & Object Size \\
\hline
"SortedTable.Default" & $K + V + 2 R + W$ & $5 R + W + (55 W + 1)$ \\
"RedBlackTbl.T" & $K + V + 3 R + 1$ & $2 R + W$ \\
"SkipListTbl.T" & $K + V + 3.33 R + W$
& $(3 + \lceil \log_4 N \rceil) R + 6 W + (55 W + 1)$ 
\end{tabular}
\end{center}
\caption{Memory usage of the various implementations in bytes. In this
table, $K$ and $V$ denote the space required to store the table key
and value, respectively, $R$ denotes the size of a reference, $W$
denotes the word size "BYTESIZE(Word.T)", and $N$ denotes the declared
maximum number of nodes in the table.}
\label{tbl:mem-use}
\end{table}

A couple of points about this data are worth mentioning. In the
"RedBlackTbl.T" node size, the $+ 1$ term is the cost of recording
the color (red or black) of the node. In the "SkipListTbl.T" node
size, the $3.33 R$ term is the \emph{expected} number of references
per node: one for a back pointer, one for the reference to an array of
forward pointers, and 1.33 for the expected size of the forward
pointer array. The $W$ term in that expression is the cost of storing
the number of elements in the dynamic forward pointer array. In the
"SkipListTbl.T" object size, the $\lceil \log_4 N \rceil R$ term is
the size of the object's main forward pointer array; again, one word
of cost has been added to account for the dynamic size of the
array. Finally, the $55 W + 1$ term in the sizes of the
"SortedTable.Default" and "SkipListTbl.T" objects is the size of a
nested "Random.Default" object required by those implementations.

\begin{table}[htb]
\begin{center}
\begin{tabular}{l|cc|cc}
& \multicolumn{2}{c|}{Per-Node Size} & \multicolumn{2}{c}{Object Size} \\
Implementation & Theory & Actual & Theory & Actual \\
\hline
"SortedTable.Default" & 40 & 40 & 489 & 496 \\
"RedBlackTbl.T" & 41 & 48 & 24 & 24 \\
"SkipListTbl.T" & 50.6 & 50.6 & 553 & 560
\end{tabular}
\end{center}
\caption{Theoretical and actual sizes of the three implementations in
bytes on a Digital Alpha workstation for sorted tables mapping
integers to integers.}
\label{tbl:real-sizes}
\end{table}

Table~\ref{tbl:mem-use} reports the minimum number of bytes required
for each data structure, ignoring alignment constraints. However, due
to alignment constraints, the actual memory requirements may be
somewhat higher. Table~\ref{tbl:real-sizes} shows the theoretical and
real sizes of the three implementations on a Digital Alpha workstation
assuming a maximum table size of $N = 1000$ and using integer keys and
values (so $K = V = 8$).
*)
