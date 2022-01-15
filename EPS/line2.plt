#!/usr/bin/gnuplot

set label ARGV[1] at ARGV[2]*ang,graph 0.98 center font "Helvetica,8"; set arrow from ARGV[2]*ang,graph 0 rto 0,graph 1 nohead lt 1 lw 0.5 lc "gray"

