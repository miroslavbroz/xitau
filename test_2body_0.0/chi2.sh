#!/bin/sh

date > date.out
./chi2 < chi2.in > chi2.out
date >> date.out

#./tmp_XYZ_HELIOCENTRIC.plt

exit

sort -k8 -g -r < chi2_SKY.dat > chi2_SKY.srt
sort -k6 -g -r < chi2_TTV.dat > chi2_TTV.srt
sort -k5 -g -r < chi2_RV.dat > chi2_RV.srt
sort -k5 -g -r < chi2_ECL.dat > chi2_ECL.srt
sort -k8 -g -r < chi2_VIS.dat > chi2_VIS.srt
sort -k12 -g -r < chi2_CLO.dat > chi2_CLO.srt
sort -k5 -g -r < chi2_LC.dat > chi2_LC.srt
sort -k6 -g -r < chi2_SYN.dat > chi2_SYN.srt

#./chi2_SKY.plt
#./chi2_SKY_ZOOM.plt
#./chi2_TTV.plt
#./chi2_RV.plt
#./chi2_ECL.plt
#./chi2_VIS.plt
#./chi2_CLO.plt
#./chi2_T3.plt
#./chi2_LC.plt
#./chi2_SYN.plt


