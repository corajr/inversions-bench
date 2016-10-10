# inversions-bench

Shows several versions of a function to count inversions.

See the [benchmarks](https://corajr.github.io/inversions-bench/inv.html) for comparison.

## Usage

To run the benchmarks:

```sh
stack build && stack exec inversions -- --output inv.html
```

To profile:

```sh
stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" && stack exec inversions-prof -- +RTS -p
```

Excerpted profiling results:

```
	Mon Oct 10 13:17 2016 Time and Allocation Profiling Report  (Final)

	   inversions-prof +RTS -N -p -RTS

	total time  =        0.29 secs   (293 ticks @ 1000 us, 1 processor)
	total alloc =  86,413,928 bytes  (excludes profiling overheads)

COST CENTRE                   MODULE      %time %alloc

length_list                   Inversions   78.8    1.6
countSplitInvSeq.go           Inversions    3.4   31.0
countSplitInvListAndLength.go Inversions    3.1   11.5
countSplitInvList.go          Inversions    2.7    6.8
inversionsSeq.(...)           Inversions    1.7    4.1
inversionsList.(...)          Inversions    0.7    8.6
inversionsListAndLength       Inversions    0.7    4.5
inversionsListAndLength.(...) Inversions    0.7    8.6
append_list                   Inversions    0.3    4.4
length_seq                    Inversions    0.3    1.1
append_seq                    Inversions    0.3    1.9
inversionsSeq                 Inversions    0.3    3.3
reverse_list                  Inversions    0.0    1.8
inversionsList                Inversions    0.0    3.1
countSplitInvList             Inversions    0.0    1.5
```

It is clear that finding the length of the list, rather than passing it in,
dominates the cost of the `inversionsList` function.
