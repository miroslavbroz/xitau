! test_sofa.f90
! Test some Standards Of Fundamental Astronomy subroutines.
! Miroslav Broz (miroslav.broz@email.cz), Jun 10th 2022

! Bulletin A .. https://datacenter.iers.org/data/latestVersion/6_BULLETIN_A_V2013_016.txt
! Bulletin B .. https://hpiers.obspm.fr/iers/bul/bulb_new/bulletinb.dat
! UTC-TAI    .. https://hpiers.obspm.fr/eoppc/bul/bulc/bulletinc.dat
! UT1-UTC    .. https://hpiers.obspm.fr/eoppc/bul/buld/bulletind.dat
! TT-UT1     .. https://maia.usno.navy.mil/ser7/deltat.data

subroutine test_sofa(UTC1, UTC2)

use hhms_module

implicit none
double precision, parameter :: pi = 4.d0*atan(1.d0)
integer :: j
double precision :: UTC1, UTC2
double precision :: TAI1, TAI2
double precision :: UT11, UT12, DUT1
double precision :: DAT, DTA
double precision :: TT1, TT2
double precision :: TDB1, TDB2, DTDB, u, v, elong, iau_dtdb
double precision :: TCB1, TCB2
double precision :: TCG1, TCG2
double precision :: ERA, iau_era00
double precision :: GMST, iau_gmst06
double precision :: GST, iau_gst06a
double precision :: h, m, s
integer :: IY, IM, ID
double precision :: FD
character(128) :: fmt

DUT1 = 0.4d0 ! UT1-UTC; https://datacenter.iers.org/data/17/bulletind-081.txt

elong = 0.d0
u = 0.d0
v = 0.d0

call iau_utctai(UTC1, UTC2, TAI1, TAI2, j)
call iau_taiut1(UTC1, UTC2, DUT1, UT11, UT12, j)
call iau_taitt(TAI1, TAI2, TT1, TT2, j)
DTDB = iau_dtdb(TT1, TT2, UT11, elong, u, v)  ! TDB instead of TT should be used here?!
call iau_tttdb(TT1, TT2, DTDB, TDB1, TDB2, j)
call iau_tdbtcb(TDB1, TDB2, TCB1, TCB2, j)
call iau_tttcg(TT1, TT2, TCG1, TCG2, j)
ERA = iau_era00(UT11, UT12)
GMST = iau_gmst06(UT11, UT12, TT1, TT2)
GST = iau_gst06a(UT11, UT12, TT1, TT2)

call iau_jd2cal(UTC1, UTC2, IY, IM, ID, FD, j)
call iau_dat(IY, IM, ID, FD, DAT, j)
DTA = DUT1-DAT

write(*,*) '----'
write(*,*) 'DUT1', DUT1/86400.d0, ' d', DUT1, ' s .. UT1-UTC'
write(*,*) 'DTA ', DTA/86400.d0, ' d', DTA, ' s .. UT1-TAI'
write(*,*) 'DAT ', DAT/86400.d0, ' d', DAT, ' s .. TAI-UTC'
write(*,*) 'DTDB', DTDB/86400.d0, ' d', DTDB, ' s .. TDB-TT'
write(*,*)

write(*,*) 'UTC ', UTC1+UTC2, (UTC1-UTC1+UTC2-UTC2)*86400.d0, ' s', j
write(*,*) 'UT1 ', UT11+UT12, (UT11-UTC1+UT12-UTC2)*86400.d0, ' s', j
write(*,*) 'TAI ', TAI1+TAI2, (TAI1-UTC1+TAI2-UTC2)*86400.d0, ' s', j
write(*,*) 'TT  ', TT1 +TT2 , (TT1 -UTC1+TT2 -UTC2)*86400.d0, ' s', j
write(*,*) 'TDB ', TDB1+TDB2, (TDB1-UTC1+TDB2-UTC2)*86400.d0, ' s', j
write(*,*) 'TCG ', TCG1+TCG2, (TCG1-UTC1+TCG2-UTC2)*86400.d0, ' s', j
write(*,*) 'TCB ', TCB1+TCB2, (TCB1-UTC1+TCB2-UTC2)*86400.d0, ' s', j
write(*,*)

fmt = '(1x,a,f22.16,a,3x,i2,a,i2,a,f9.6)'
call hhms(ERA/pi*12.d0, h, m, s)
write(*,fmt) 'ERA ', ERA, ' rad', int(h),':',int(m),':',s
call hhms(GMST/pi*12.d0, h, m, s)
write(*,fmt) 'GMST', GMST, ' rad', int(h),':',int(m),':',s
call hhms(GST/pi*12.d0, h, m, s)
write(*,fmt) 'GST ', GST, ' rad', int(h),':',int(m),':',s
write(*,*) '----'

end subroutine test_sofa

program main

implicit none

call test_sofa(2451545.0d0, 0.d0)  ! J2000
call test_sofa(2453371.5d0, 0.d0)  ! Jan 1st 2005
call test_sofa(2459875.589308d0, 0.d0)  ! Oct 23rd 2022

stop

end program main


