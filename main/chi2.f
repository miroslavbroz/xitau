c chi2.f
c Calculate chi^2 for given orbit of xi Tau.
c Miroslav Broz (miroslav.broz@email.cz), May 24th 2017

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

      write(*,*) "# file_AO : "
      read(*,10,err=990,end=990) file_AO
      write(*,*) "# file_AO = ", trim(file_AO)

      write(*,*) "# file_SKY2 : "
      read(*,10,err=990,end=990) file_SKY2
      write(*,*) "# file_SKY2 = ", trim(file_SKY2)

      write(*,*) "# file_SKY3 : "
      read(*,10,err=990,end=990) file_SKY3
      write(*,*) "# file_SKY3 = ", trim(file_SKY3)

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

      pyterpol_Delta(1) = 0.d0
      pyterpol_Delta(2) = 0.d0
      pyterpol_Delta(3) = 0.d0
      pyterpol_Delta(4) = 0.d0

      write(*,*) "# silh_factor : "
      read(*,*,err=990,end=990) silh_factor
      write(*,*) "# silh_factor = ", silh_factor

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
        write(*,*) "Error: use_multipole = ", use_multipole,
     :    " and use_bruteforce = ", use_bruteforce
        stop
      endif

      write(*,*) "# w_SKY w_RV w_TTV w_ECL w_VIS w_CLO w_T3 w_LC ",
     :  "w_SYN w_SED w_AO w_SKY2 w_SKY3 : "
      read(*,*,err=990,end=990) w_SKY, w_RV, w_TTV, w_ECL, w_VIS, w_CLO,
     :  w_T3, w_LC, w_SYN, w_SED, w_AO, w_SKY2, w_SKY3
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
      write(*,*) "# w_SKY2 = ", w_SKY2
      write(*,*) "# w_SKY3 = ", w_SKY3

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
        do i = 1, nparam
          write(str,*) i
          write(10,*) "x_param", adjustl(trim(str)) ," = ", x_param(i)
        enddo
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
      write(*,*) '# chi2_SKY  = ', chi2_SKY
      write(*,*) '# chi2_RV   = ', chi2_RV
      write(*,*) '# chi2_TTV  = ', chi2_TTV
      write(*,*) '# chi2_ECL  = ', chi2_ECL
      write(*,*) '# chi2_VIS  = ', chi2_VIS
      write(*,*) '# chi2_CLO  = ', chi2_CLO
      write(*,*) '# chi2_T3   = ', chi2_T3
      write(*,*) '# chi2_LC   = ', chi2_LC
      write(*,*) '# chi2_SYN  = ', chi2_SYN
      write(*,*) '# chi2_SED  = ', chi2_SED
      write(*,*) '# chi2_AO   = ', chi2_AO
      write(*,*) '# chi2_SKY2 = ', chi2_SKY2
      write(*,*) '# chi2_SKY3 = ', chi2_SKY3
      write(*,*) '# chi2_MASS = ', chi2_MASS
      write(*,*) '# chi2 = ', chi2
      write(*,*) '# n_fit = ', n_fit
      write(*,*) '# nparam = ', nparam
      write(*,*) '# nu = ', int(nu)
      write(*,*) '# probp = ', probp
      write(*,*) '# probq = ', probq

      stop

c  error handlers
990   continue
      write(*,*) 'chi2: Error reading standard input.'

      end


