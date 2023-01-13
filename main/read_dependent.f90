! read_dependent.f90
! Read dependent parameters.
! Miroslav Broz (miroslav.broz@email.cz), Jun 22nd 2022

module read_dependent_module

contains

subroutine read_dependent()

implicit none
include "../filters/filters.inc"
include "../chi2/chi2.inc"
include "../chi2/dependent.inc"

integer i

write(*,*) "# T0 : "
read(*,*,err=990,end=990) T0
write(*,*) "# T0 = ", T0

write(*,*) "# nshp: "
read(*,*,err=990,end=990) nshp
write(*,*) "# nshp = ", nshp

if (nshp.gt.NDIMMAX) then
  write(*,*) "Error nshp = ", nshp, ".gt. NDIMMAX = ", NDIMMAX
  stop
endif

write(*,*) "# nbod : "
read(*,*,err=990,end=990) nbod
write(*,*) "# nbod = ", nbod

if (nbod.gt.NBODMAX) then
  write(*,*) "Error nbod = ", nbod, ".gt. NBODMAX = ", NBODMAX
  stop
endif

do i = 1, nbod
  write(*,*) "# file_SKY(", i, ") : "
  read(*,10,err=990,end=990) file_SKY(i)
10 format(a)
  write(*,*) "# file_SKY(", i, ") = ", trim(file_SKY(i))
enddo

do i = 1, nbod
  write(*,*) "# file_RV(", i, ") : "
  read(*,10,err=990,end=990) file_RV(i)
  write(*,*) "# file_RV(", i, ") = ", trim(file_RV(i))
enddo

write(*,*) "# file_TTV : "
read(*,10,err=990,end=990) file_TTV
write(*,*) "# file_TTV = ", trim(file_TTV)

write(*,*) "# file_ECL : "
read(*,10,err=990,end=990) file_ECL
write(*,*) "# file_ECL = ", trim(file_ECL)

write(*,*) "# file_VIS : "
read(*,10,err=990,end=990) file_VIS
write(*,*) "# file_VIS = ", trim(file_VIS)

write(*,*) "# file_CLO : "
read(*,10,err=990,end=990) file_CLO
write(*,*) "# file_CLO = ", trim(file_CLO)

write(*,*) "# file_SYN : "
read(*,10,err=990,end=990) file_SYN
write(*,*) "# file_SYN = ", trim(file_SYN)

do i = 1, nbod
  write(*,*) "# file_synth(", i, ") : "
  read(*,10,err=990,end=990) file_synth(i)
  write(*,*) "# file_synth(", i, ") = ", trim(file_synth(i))
enddo

write(*,*) "# file_SED : "
read(*,10,err=990,end=990) file_SED
write(*,*) "# file_SED = ", trim(file_SED)

do i = 1, nbod
  write(*,*) "# file_absol(", i, ") : "
  read(*,10,err=990,end=990) file_absol(i)
  write(*,*) "# file_absol(", i, ") = ", trim(file_absol(i))
enddo

write(*,*) "# file_AO : "
read(*,10,err=990,end=990) file_AO
write(*,*) "# file_AO = ", trim(file_AO)

write(*,*) "# file_AO2 : "
read(*,10,err=990,end=990) file_AO2
write(*,*) "# file_AO2 = ", trim(file_AO2)

write(*,*) "# file_SKY2 : "
read(*,10,err=990,end=990) file_SKY2
write(*,*) "# file_SKY2 = ", trim(file_SKY2)

write(*,*) "# file_SKY3 : "
read(*,10,err=990,end=990) file_SKY3
write(*,*) "# file_SKY3 = ", trim(file_SKY3)

write(*,*) "# file_OCC : "
read(*,10,err=990,end=990) file_OCC
write(*,*) "# file_OCC = ", trim(file_OCC)

write(*,*) "# nband : "
read(*,*,err=990,end=990) nband
if (nband.gt.BANDMAX) then
  write(*,*) "Error nband = ", nband, ".gt. BANDMAX = ", BANDMAX
  stop
endif
write(*,*) "# nband = ", nband

do i = 1, nband
  write(*,*) "# iband_LC(", i, ") file_LC(", i, ") : "
  read(*,*,err=990,end=990) iband_LC(i), file_LC(i)
  write(*,*) "# iband_LC(", i, ") = ", iband_LC(i)
  write(*,*) "# file_LC(", i, ") = ", trim(file_LC(i))
  if (iband_LC(i).gt.WDBANDS) then
    write(*,*) "Error iband = ", iband_LC(i), ".gt. WDBANDS = ", WDBANDS
    stop
  endif
enddo

write(*,*) "# geometry : "
read(*,*,err=990,end=990) geometry
write(*,*) "# geometry = ", geometry

write(*,*) "# m_min() : "
read(*,*,err=990,end=990) (m_min(i), i = 1,nbod)
do i = 1,nbod
  write(*,*) "# m_min(", i, ") = ", m_min(i), " M_S"
enddo

write(*,*) "# m_max() : "
read(*,*,err=990,end=990) (m_max(i), i = 1,nbod)
do i = 1,nbod
  write(*,*) "# m_max(", i, ") = ", m_max(i), " M_S"
enddo

write(*,*) "# use_hec88() : "
read(*,*,err=990,end=990) (use_hec88(i), i = 1,nbod)
do i = 1, nbod
  write(*,*) "# use_hec88(", i, ") = ", use_hec88(i)
enddo

write(*,*) "# lightcurve_timestep : "
read(*,*,err=990,end=990) lightcurve_timestep
write(*,*) "# lightcurve_timestep = ", lightcurve_timestep

write(*,*) "# approx_eclipse_duration : "
read(*,*,err=990,end=990) approx_eclipse_duration
write(*,*) "# approx_eclipse_duration = ", approx_eclipse_duration

write(*,*) "# lambda1 lambda2 : "
read(*,*,err=990,end=990) lambda1, lambda2
write(*,*) "# lambda1 = ", lambda1, " m = ", lambda1/1.d-9," nm"
write(*,*) "# lambda2 = ", lambda2, " m = ", lambda2/1.d-9," nm"

write(*,*) "# lambda3 lambda4 : "
read(*,*,err=990,end=990) lambda3, lambda4
write(*,*) "# lambda3 = ", lambda3, " m = ", lambda3/1.d-9," nm"
write(*,*) "# lambda4 = ", lambda4, " m = ", lambda4/1.d-9," nm"

write(*,*) "# pyterpol_Delta(4) : "
read(*,*,err=990,end=990) (pyterpol_Delta(i), i = 1,4)
do i = 1, 4
  write(*,*) "# pyterpol_Delta(", i, ") = ", pyterpol_Delta(i)
enddo

write(*,*) "# silh_factor : "
read(*,*,err=990,end=990) silh_factor
write(*,*) "# silh_factor = ", silh_factor

write(*,*) "# spectral_slope : "
read(*,*,err=990,end=990) spectral_slope
write(*,*) "# spectral_slope = ", spectral_slope

write(*,*) "# nsub : "
read(*,*,err=990,end=990) nsub
write(*,*) "# nsub = ", nsub

write(*,*) "# use_planck : "
read(*,*,err=990,end=990) use_planck
write(*,*) "# use_planck = ", use_planck

write(*,*) "# use_filters : "
read(*,*,err=990,end=990) use_filters
write(*,*) "# use_filters = ", use_filters

write(*,*) "# use_limbdark : "
read(*,*,err=990,end=990) use_limbdark
write(*,*) "# use_limbdark = ", use_limbdark

write(*,*) "# use_pyterpol : "
read(*,*,err=990,end=990) use_pyterpol
write(*,*) "# use_pyterpol = ", use_pyterpol

write(*,*) "# use_vardist : "
read(*,*,err=990,end=990) use_vardist
write(*,*) "# use_vardist = ", use_vardist

write(*,*) "# use_varpole : "
read(*,*,err=990,end=990) use_varpole
write(*,*) "# use_varpole = ", use_varpole

write(*,*) "# use_multipole : "
read(*,*,err=990,end=990) use_multipole
write(*,*) "# use_multipole = ", use_multipole

write(*,*) "# use_bruteforce : "
read(*,*,err=990,end=990) use_bruteforce
write(*,*) "# use_bruteforce = ", use_bruteforce

if ((use_multipole).and.(use_bruteforce)) then
  write(*,*) "Error: use_multipole = ", use_multipole, " and use_bruteforce = ", use_bruteforce
  stop
endif

read(*,*,err=990,end=990) use_oblat
write(*,*) "# use_oblat = ", use_oblat

read(*,*,err=990,end=990) use_tides
write(*,*) "# use_tides = ", use_tides

read(*,*,err=990,end=990) use_tides2
write(*,*) "# use_tides2 = ", use_tides2

read(*,*,err=990,end=990) use_ppn
write(*,*) "# use_ppn = ", use_ppn

read(*,*,err=990,end=990) use_polygon
write(*,*) "# use_polygon = ", use_polygon

read(*,*,err=990,end=990) use_zero
write(*,*) "# use_zero = ", use_zero

read(*,*,err=990,end=990) use_adam
write(*,*) "# use_adam = ", use_adam

read(*,*,err=990,end=990) use_stellar
write(*,*) "# use_stellar = ", use_stellar

read(*,*,err=990,end=990) use_cliptrace
write(*,*) "# use_cliptrace = ", use_cliptrace

write(*,*) "# w_SKY w_RV w_TTV w_ECL w_VIS w_CLO w_T3 w_LC w_SYN w_SED w_AO w_AO2, w_SKY2 w_SKY3 w_OCC : "
read(*,*,err=990,end=990) w_SKY, w_RV, w_TTV, w_ECL, w_VIS, w_CLO, w_T3, w_LC, w_SYN, w_SED, w_AO, w_AO2, w_SKY2, w_SKY3, w_OCC
write(*,*) "# w_SKY = ", w_SKY
write(*,*) "# w_RV = ", w_RV
write(*,*) "# w_TTV = ", w_TTV
write(*,*) "# w_ECL = ", w_ECL
write(*,*) "# w_VIS = ", w_VIS
write(*,*) "# w_CLO = ", w_CLO
write(*,*) "# w_T3 = ", w_T3
write(*,*) "# w_LC = ", w_LC
write(*,*) "# w_SYN = ", w_SYN
write(*,*) "# w_SED = ", w_SED
write(*,*) "# w_AO = ", w_AO
write(*,*) "# w_AO2 = ", w_AO2
write(*,*) "# w_SKY2 = ", w_SKY2
write(*,*) "# w_SKY3 = ", w_SKY3
write(*,*) "# w_OCC = ", w_OCC

write(*,*) "# eps_BS : "
read(*,*,err=990,end=990) eps_BS
write(*,*) "# eps_BS = ", eps_BS

write(*,*) "# debug : "
read(*,*,err=990,end=990) debug
write(*,*) "# debug = ", debug

write(*,*) "# debug_swift : "
read(*,*,err=990,end=990) debug_swift
write(*,*) "# debug_swift = ", debug_swift

return

990 continue
write(*,*) 'read_dependent: Error reading dependent parameters.'
stop

end subroutine read_dependent

end module read_dependent_module


