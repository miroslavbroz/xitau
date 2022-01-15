c chi2_func_SYN.f
c Calculate chi^2 for synthetic spectra.
c Miroslav Broz (miroslav.broz@email.cz), May 23rd 2016

      subroutine chi2_func_SYN(NOUT, tout, vb, chi2, n)

      implicit none
      include '../misc/const.inc'
      include 'simplex.inc'
      include 'dependent.inc'

c input
      integer NOUT
      real*8 tout(OUTMAX)
      real*8 vb(OUTMAX,NBODMAX,3)

c output
      real*8 chi2
      integer n

c observational data
      integer m_OBS
      real*8 t_OBS(OBSMAX), lambda_OBS(OBSMAX), Int_OBS(OBSMAX),
     :  sigma_Int_OBS(OBSMAX)
      integer dataset_OBS(OBSMAX)

c synthetic spectra
      integer n_synth(NBODMAX)
      real*8 lambda_synth(OBSMAX,NBODMAX), Int_synth(OBSMAX,NBODMAX)

c internal variables
      integer i,j,k,l,i1st,iua,iub,ier,nbod2
      real*8 chi2_
      real*8 vzb_interp, lambda_interp, Int_interp, Int_Sum(OBSMAX)
      real*8 Lum_, Lumtot(OBSMAX)
      real*8 Lum__(OBSMAX,NBODMAX), Int__(OBSMAX,NBODMAX)
      character*80 str

c external functions
      real*8 interp

c parameters for Lum_func are passed in a common block (due to integration in luminosity.f)
      real*8 Lum_func
      real*8 T, R
      common /cb_Lum/ T, R

      data i1st /0/
      data m_OBS /0/
      data iua,iub /10,20/

      save i1st,
     :  m_OBS, t_OBS, lambda_OBS, Int_OBS, sigma_Int_OBS, dataset_OBS,
     :  n_synth, lambda_synth, Int_synth

c-----------------------------------------------------------------------
c
c read synthetic and observed spectra (only 1st time!)
c

      if (i1st.eq.0) then

        call read_SYN(file_SYN, m_OBS, t_OBS, lambda_OBS, Int_OBS,
     :    sigma_Int_OBS, dataset_OBS)

        if (debug) then
          write(*,*) "# m_SYN = ", m_OBS
        endif

        if (.not.use_pyterpol) then
          do j = 1, nbod
         
            call read_synth(file_synth(j), n_synth(j),
     :        lambda_synth(1,j), Int_synth(1,j))
         
            if (debug) then
              write(*,*) "# n_synth(", j, ") = ", n_synth(j)
            endif
         
          enddo
        endif

        i1st = 1
      endif  ! i1st

c-----------------------------------------------------------------------
c
c generate synthetic spectra using Pyterpol (Nemravova et al. 2016)
c
      if (use_pyterpol.and.(m_OBS.gt.0)) then

        call pyterpol_(nbod, T_eff, log_g, v_rot, metal,
     :    pyterpol_Delta, lambda1, lambda2, .false., nbod2)

c read them back
        if (nbod2.gt.0) then
          do j = 1, nbod
            write(str,10) j
10          format(i1, ".syn")
         
            call read_synth(str, n_synth(j), lambda_synth(1,j),
     :        Int_synth(1,j))
         
            if (debug) then
              write(*,*) "# n_synth(", j, ") = ", n_synth(j)
            endif
          enddo
        endif

      endif  ! use_pyterpol

c-----------------------------------------------------------------------
c
c compute luminosity-weigthed synthetic spectrum
c
      do i = 1, m_OBS
        Int_sum(i) = 0.d0
        Lumtot(i) = 0.d0
      enddo

      do k = 1, nbod
        if (n_synth(k).ge.2) then

          j = 2

          do i = 1, m_OBS
            if ((lambda_OBS(i).gt.lambda1).and.
     :        (lambda_OBS(i).lt.lambda2)) then
    
              do while ((j.lt.NOUT).and.(tout(j).le.t_OBS(i)))
                j = j+1
              enddo

c linear interpolation of integrated data to a given position in time

              if ((i.eq.1).or.(t_OBS(i).ne.t_OBS(i-1))) then

                vzb_interp = interp(tout(j-1), tout(j), vb(j-1,k,3),
     :            vb(j,k,3), t_OBS(i))  ! k-th body, vb_z coordinate

                vzb_interp = vzb_interp*AU/day
              endif

c interpolate intensities to observed lambda using Hermite cubic spline

              lambda_interp = lambda_OBS(i) * (1.d0 - vzb_interp/c)  ! Doppler shift

              if ((i.eq.1).or.(lambda_OBS(i).lt.lambda_OBS(i-1))) then
                l = 3
              endif

              do while ( (l.lt.n_synth(k))
     :          .and.(lambda_synth(l,k).le.lambda_interp) )
                l = l+1
              enddo

              call hermite(lambda_interp, Int_interp,
     :          lambda_synth(l-2,k), Int_synth(l-2,k), n_synth(k)-l+2,
     :          ier)
           
              if (ier.eq.2) then
                write(*,*) "# Warning: IER.eq.2 in hermite()"
                write(*,*) "# lambda_OBS(", i, ") = ", lambda_OBS(i),
     :            " m"
                write(*,*) "# lambda_interp = ", lambda_interp, " m"
                write(*,*) "# lambda_synth(", l-2, ",", k, ") = ",
     :            lambda_synth(l-2,k), " m"
                write(*,*) "# lambda_synth(", l-2+n_synth(k)-l+2, ",",
     :            k, ") = ", lambda_synth(l-2+n_synth(k)-l+2,k), " m"
              endif

c compute monochromatic "luminosity" at given lambda
              T = T_eff(k)
              R = R_star(k)*R_S
              Lum_ = Lum_func(lambda_interp)
              Lumtot(i) = Lumtot(i) + Lum_
 
              Int_sum(i) = Int_sum(i) + Lum_*Int_interp

              Int__(i,k) = Int_interp
              Lum__(i,k) = Lum_

            endif  ! lambda
          enddo  ! m_OBS

        endif  ! n_synth
      enddo  ! nbod

c a very detailed output of all intensities and luminosities...

      if (debug_swift) then
        open(unit=iub,file="synthetic2.dat",status="unknown")
        write(iub,*) "# t & lambda [m] & Int_lambda []",
     :    " & Lum_lambda [W m^-1] & Lum_tot [W m^-1] & ibod & dataset"

        do i = 1, m_OBS
          if ((lambda_OBS(i).gt.lambda1).and.
     :      (lambda_OBS(i).lt.lambda2)) then

            if ((i.gt.1).and.(dataset_OBS(i).ne.dataset_OBS(i-1))) then
              write(iub,*)
              write(iub,*)
            endif

            do k = 1, nbod
              if (n_synth(k).ge.2) then

                write(iub,*) t_OBS(i), lambda_OBS(i), Int__(i,k),
     :            Lum__(i,k), Lumtot(i), k, dataset_OBS(i)

              endif
            enddo
          endif
        enddo
    
        close(iub)
      endif

c-----------------------------------------------------------------------
c
c chi^2 for synthetic spectrum
c
      chi2 = 0.d0
      n = 0

      if (debug) then
        open(unit=iua,file="chi2_SYN.dat",status="unknown")
        write(iua,*) "# t_OBS & lambda & Int_interp & sigma_OBS",
     :    " & dataset & chi^2"
        write(iua,*) "# t_OBS & lambda & Int_OBS    & sigma_OBS",
     :    " & dataset & chi^2"
      endif

      if (debug) then
        open(unit=iub,file="synthetic.dat",status="unknown")
        write(iub,*) "# t & lambda [m] & Int [] & dataset"
      endif

      do i = 1, m_OBS

c normalize the intensity again
        Int_sum(i) = Int_sum(i)/Lumtot(i)

c add to chi^2
        chi2_ = (Int_OBS(i) - Int_sum(i))**2/sigma_Int_OBS(i)**2
        chi2 = chi2 + chi2_
        n = n + 1

        if (debug) then
          write(iua,*) t_OBS(i), lambda_OBS(i), Int_sum(i),
     :      sigma_Int_OBS(i), dataset_OBS(i), chi2_
          write(iua,*) t_OBS(i), lambda_OBS(i), Int_OBS(i),
     :      sigma_Int_OBS(i), dataset_OBS(i), chi2_
          write(iua,*)
        endif

        if (debug) then
          if ((i.gt.1).and.(dataset_OBS(i).ne.dataset_OBS(i-1))) then
            write(iub,*)
            write(iub,*)
          endif
          write(iub,*) t_OBS(i), lambda_OBS(i), Int_sum(i),
     :      dataset_OBS(i)
        endif

      enddo

      if (debug) then
        close(iua)
      endif

      if (debug_swift) then
        close(iub)
      endif

      return
      end


