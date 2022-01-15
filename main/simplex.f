c simplex.f
c Simplex for the xi Tau problem.
c Miroslav Broz (miroslav.broz@email.cz), May 24th 2017

      program simplex

      implicit none
      include '../simplex/simplex.inc'
      include '../simplex/dependent.inc'
      include '../simplex/cb_itmax.inc'

      integer i, j, iter, mp, np, id(NDIMMAX+1)
      real*8 x(NDIMMAX), e(NDIMMAX), p(NDIMMAX+1,NDIMMAX), y(NDIMMAX+1),
     :  ftol, xtry(NDIMMAX), e_param(NDIMMAX)
      character*80 str

      integer length
      real*8 chi2_func
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

      write(*,*) "# e_param() : "
      read(*,*,err=990,end=990) (e_param(i), i=1,nparam)
      do i = 1,nparam
        write(*,*) "# e_param(", i, ") = ", e_param(i)
      enddo

      write(*,*) "# variable() : "
      read(*,*,err=990,end=990) (variable(i), i=1,nparam)
      do i = 1,nparam
        write(*,*) "# variable(", i, ") = ", variable(i)
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

      write(*,*) "# ftol : "
      read(*,*,err=990,end=990) ftol
      write(*,*) "# ftol = ", ftol

      write(*,*) "# ITMAX : "
      read(*,*,err=990,end=990) ITMAX
      write(*,*) "# ITMAX = ", ITMAX

      write(*,*) "# debug : "
      read(*,*,err=990,end=990) debug
      write(*,*) "# debug = ", debug

      write(*,*) "# debug_swift : "
      read(*,*,err=990,end=990) debug_swift
      write(*,*) "# debug_swift = ", debug_swift

c  resolve variable/fixed parameters
      ndim = 0
      do i = 1,nparam
        if (variable(i)) then
          ndim = ndim + 1
          x(ndim) = x_param(i)
          e(ndim) = e_param(i)
        endif
      enddo

      if (debug) then
        write(*,*) '# ndim = ', ndim
      endif

c  pass the rest in common block /dependent/ 

c  initialise the simplex (p array)

      do i = 1,ndim+1
        do j = 1,ndim
          if (j.eq.i) then
            p(i,j) = x(j) + e(i)
          else
            p(i,j) = x(j)
          endif
        enddo
      enddo

      if (debug) then
        write(*,*) "# initial p() array:"
        do i = 1,ndim+1
          do j = 1,ndim
            write(*,20) p(i,j)
20          format(f16.8,1x,$)
          enddo
          write(*,*)
        enddo
        write(*,*)
      endif

c  y() array
      do i = 1,ndim+1
        do j = 1,ndim
          xtry(j) = p(i,j)
        enddo
        y(i) = chi2_func(xtry)
      enddo

      if (debug) then
        write(*,*) "# initial y() array:"
        do i = 1,ndim+1
          write(*,*) y(i)
        enddo
        write(*,*)
      endif

      mp = NDIMMAX+1
      np = NDIMMAX
      iter = 0

c  run it!
      call amoeba(p,y,mp,np,ndim,ftol,chi2_func,iter)

c  sort the output according to chi^2
      call srtidx(ndim+1,y,id)

c  write the result of minimalisation

      if (debug) then
        write(*,*) "# iter = ", iter

        write(*,*) "# p() array:"
        do i = 1,ndim+1
          do j = 1,ndim
            write(*,20) p(id(i),j)
          enddo
          write(*,*)
        enddo

        write(*,*) "# y() array:"
        do i = 1,ndim+1
          write(*,*) y(id(i))
        enddo
        write(*,*)
      endif

      write(*,*) (p(id(1),j), j = 1,ndim), y(id(1))

      stop

c  error handlers
990   continue
      write(*,*) 'simplex: Error reading standard input.'

      end


