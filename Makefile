main: main.icl _ArraySlice.icl _ArraySlice.dcl QuickSort.icl QuickSort.dcl
#	env CLEANLIB=/usr/lib64/clean/exe/ clm -d  -PABC -I /usr/lib64/clean/StdEnv/ main -o main
	env CLEANLIB=/usr/lib64/clean/exe/ clm -lat -lset -I /usr/lib64/clean/StdEnv/ main -o main
