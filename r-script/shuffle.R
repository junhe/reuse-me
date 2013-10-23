
generate_trace_bypattern <- function()
{
	base = 0 # where (offset) this pattern strats
	segsize = 100 # size of the largest segment shared by all processes
	nseg = 2
	len = 1 # length of each write, in this version, all lengths are identical
	np = 8
	# iterators: pid and seg_i(which segment it is)
	# i want: $offset $length $pid
	
	# generate on pid by one pid
	for ( pid in seq(0, np-1) ) {
		firstbyte = base+pid*len
		offs = seq(firstbyte, by=segsize, length.out=nseg)
		print (offs)
	}
	
}

# the input df is a segment
extend_pattern <- function(df)
{
	segsize = sum(df$Length)
}

