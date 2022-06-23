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

      call read_dependent()

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
      write(*,*) '# chi2_OCC  = ', chi2_OCC
      write(*,*) '# chi2_MASS = ', chi2_MASS
      write(*,*) '# chi2 = ', chi2
      write(*,*) '# lns = ', lns
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


