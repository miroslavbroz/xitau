#!/usr/bin/env gnuplot

#set term x11
set colors classic

set xl "iter"
set yl "val/val_0"

set zeroaxis

p \
  "<awk '(FNR==1){ i= 1; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "mtot",\
  "<awk '(FNR==1){ i= 2; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "q1",\
  "<awk '(FNR==1){ i= 3; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "q2",\
  "<awk '(FNR==1){ i= 4; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "q3",\
  "<awk '(FNR==1){ i= 5; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "P1",\
  "<awk '(FNR==1){ i= 6; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "loge1",\
  "<awk '(FNR==1){ i= 7; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "i1",\
  "<awk '(FNR==1){ i= 8; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "Omega1",\
  "<awk '(FNR==1){ i= 9; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "varpi1",\
  "<awk '(FNR==1){ i=10; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "lambda1",\
  "<awk '(FNR==1){ i=11; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "P2",\
  "<awk '(FNR==1){ i=12; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "loge2",\
  "<awk '(FNR==1){ i=13; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "i2",\
  "<awk '(FNR==1){ i=14; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "Omega2",\
  "<awk '(FNR==1){ i=15; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "varpi2",\
  "<awk '(FNR==1){ i=16; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "lambda2",\
  "<awk '(FNR==1){ i=17; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "P3",\
  "<awk '(FNR==1){ i=18; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "loge3",\
  "<awk '(FNR==1){ i=19; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "i3",\
  "<awk '(FNR==1){ i=20; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "Omega3",\
  "<awk '(FNR==1){ i=21; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "varpi3",\
  "<awk '(FNR==1){ i=22; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "lambda3",\
  "<awk '(FNR==1){ i=23; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "T1",\
  "<awk '(FNR==1){ i=24; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "T2",\
  "<awk '(FNR==1){ i=25; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "T3",\
  "<awk '(FNR==1){ i=26; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "T4",\
  "<awk '(FNR==1){ i=27; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "logg1",\
  "<awk '(FNR==1){ i=28; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "logg2",\
  "<awk '(FNR==1){ i=29; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "logg3",\
  "<awk '(FNR==1){ i=30; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "logg4",\
  "<awk '(FNR==1){ i=55; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "gamma",\
  "<awk '(FNR==1){ i=56; p=$i; }{ print NR,$i/p; }' chi2_func.tmp" u 1:2 w lp t "dpc",\
  1.0 w l lt 0

pa -1

set term png small
set out "chi2_param.png"
rep


