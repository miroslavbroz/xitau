c chi2.f
c Calculate chi^2 for given orbit of xi Tau.
c Miroslav Broz (miroslav.broz@email.cz), Jul 20th 2015

      program chi2_

      implicit none
      include '../simplex/simplex.inc'
      include '../simplex/dependent.inc'

      integer i
      real*8 x(NDIMMAX), chi2, probp, probq, nu
      character*80 str

      real*8 chi2_func, gammp, gammq
c
c print version
c     
      call util_version
c
c  read input parameters (and write input to terminal)
c
      write(*,*) "# nparam : "
5     continue
        read(*,10,err=990,end=990) str
10      format(a)
      if (str(1:1).eq.'#') goto 5

      read(str,*,err=990,end=990) nparam
      if (nparam.gt.NDIMMAX) then
        write(*,*) 'Error: nparam > NDIMMAX = ', NDIMMAX
        stop
      endif
      write(*,*) "# nparam = ", nparam

      write(*,*) "# x_param() : "
      read(*,*,err=990,end=990) (x_param(i), i=1,nparam)
      do i = 1, nparam
        write(*,*) "# x_param(", i, ") = ", x_param(i)
      enddo

      write(*,*) "# T0 : "
      read(*,*,err=990,end=990) T0
      write(*,*) "# T0 = ", T0

      write(*,*) "# nbod : "
      read(*,*,err=990,end=990) nbod
      write(*,*) "# nbod = ", nbod

      if (nbod.gt.NBODMAX) then
        write(*,*) "# Error nbod = ", nbod, ".gt. NBODMAX = ", NBODMAX
        stop
      endif

      do i = 1, nbod
        write(*,*) "# file_SKY(", i, ") : "
        read(*,10,err=990,end=990) file_SKY(i)
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

      write(*,*) "# nband : "
      read(*,*,err=990,end=990) nband
      if (nband.gt.BANDMAX) then
        write(*,*) "# Error nband = ", nband, ".gt. BANDMAX = ", BANDMAX
        stop
      endif
      write(*,*) "# nband = ", nband

      do i = 1, nband
        write(*,*) "# iband_LC(", i, ") file_LC(", i, ") : "
        read(*,*,err=990,end=990) iband_LC(i), file_LC(i)
        write(*,*) "# iband_LC(", i, ") = ", iband_LC(i)
        write(*,*) "# file_LC(", i, ") = ", trim(file_LC(i))
      enddo

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

      write(*,*) "# geometry : "
      read(*,*,err=990,end=990) geometry
      write(*,*) "# geometry = ", geometry

      write(*,*) "# m_min() : "
      read(*,*,err=990,end=990) (m_min(i), i = 1,nbod)
      do i = 1, nbod
        write(*,*) "# m_min(", i, ") = ", m_min(i), " M_S"
      enddo

      write(*,*) "# m_max() : "
      read(*,*,err=990,end=990) (m_max(i), i = 1,nbod)
      do i = 1, nbod
        write(*,*) "# m_max(", i, ") = ", m_max(i), " M_S"
      enddo

      write(*,*) "# metal() : "
      read(*,*,err=990,end=990) (metal(i), i = 1,nbod)
      do i = 1, nbod
        write(*,*) "# metal(", i, ") = ", metal(i)
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

      write(*,*) "# w_SKY w_RV w_TTV w_ECL w_VIS w_CLO w_T3 w_LC ",
     :  "w_SYN w_SED : "
      read(*,*,err=990,end=990) w_SKY, w_RV, w_TTV, w_ECL, w_VIS, w_CLO,
     :  w_T3, w_LC, w_SYN, w_SED
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

      write(*,*) "# eps_BS : "
      read(*,*,err=990,end=990) eps_BS
      write(*,*) "# eps_BS = ", eps_BS

      write(*,*) "# debug : "
      read(*,*,err=990,end=990) debug
      write(*,*) "# debug = ", debug

      write(*,*) "# debug_swift : "
      read(*,*,err=990,end=990) debug_swift
      write(*,*) "# debug_swift = ", debug_swift

c gnuplot
      if (debug) then
        open(unit=10,file="T0.plt",status="unknown")
        write(10,*) "T0 = ", T0
        close(10)
      endif

c  calculate chi^2 and the corresponding probability
      ndim = nparam
      do i = 1, nparam
        x(i) = x_param(i)
        variable(i) = .TRUE.
      enddo

      chi2 = chi2_func(x)
      nu = n_fit - nparam
      probp = gammp(nu/2.d0,chi2/2.d0)
      probq = gammq(nu/2.d0,chi2/2.d0)

c  write output
      write(*,*) '# chi2 & probp & probq & ',
     :  'n_SKY & n_RV & n_TTV & n_ECL & n_VIS & n_CLO & n_T3 & n_LC & ',
     :  'n_SYN & n_SED & chi2_SKY & chi2_RV & chi2_TTV & chi2_ECL & ',
     :  'chi2_VIS & chi2_CLO & chi2_T3 & chi2_LC & chi2_SYN & ',
     :  'chi2_SED & chi2_MASS'
      write(*,*) chi2, probp, probq,
     :  n_SKY, n_RV, n_TTV, n_ECL, n_VIS, n_CLO, n_T3, n_LC, n_SYN,
     :  n_SED, chi2_SKY, chi2_RV, chi2_TTV, chi2_ECL, chi2_VIS,
     :  chi2_CLO, chi2_T3, chi2_LC, chi2_SYN, chi2_SED, chi2_MASS

      stop

c  error handlers
990   continue
      write(*,*) 'chi2: Error reading standard input.'

      end


