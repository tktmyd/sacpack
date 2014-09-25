# sacpack

A Fortran 2003 module and related utilities on reading/writing SAC-formatted seismograms


## Usage

### sac2asci

Convert SAC file to two-column ascii data. Result will be exported to standard output.

    usage: sac2ascii.x sacfile > output.txt


### sac2bin

Convert sac data in double-precision binary format. Output binary file can be used as input of psxy (GMT) with -bi option.

    usage: sac2bin.x sacfile outputfille

### ascii2sac

Create sac-formatted file from two-column ascii file. This is inverse of sac2ascii, 

    usage: ascii2sac.x  infile outfile
    infile  : input two-column ascii file. First column is treated as time, second is amplitude
    outfile : output sac filename 


### sacdiff

Take diff between two SAC files. The result is stored also in the SAC format. 

     usage: sacdiff.x sacfile1 sacfile2 diffsac
     Two input sac files must have same sampling interval and same number of data samples in common. 


### rsachead

Read SAC header and display to standard output. 

    usage: rsachead.x sacfile header_name (-v)
    header_name: SAC header name or "ALL" to display all header information
    -v: show variables only. Default is formatted (header_name = var)


### stacksac

Stack SAC files

     usage: stacksac.x (-n) [sacfile list] (-o outfile)
     -n: Normalize all sacfiles by their maximum amplitude
     -o: Specify output sac filename. Default is stack.sac!

### catsac

Concatnate SAC files

     usage: catsac.x [sacfile list] (-o outfile)
     

## Copyright and License

Copyright (C) 2014, Takuto Maeda, All rights reserved. All source codes included in this archive are released under the MIT License. 