c chi2_func_T3.f
c Calculate chi^2 for interferometric triple product amplitude |T_3|.
c Mirolav Broz (miroslav.broz@email.cz), Apr 1st 2016

      subroutine chi2_func_T3(chi2, n)

      implicit none
      include '../misc/const.inc'
      include 'chi2.inc'
      include 'dependent.inc'
      include 'cb_t3amp.inc'

c output
      real*8 chi2
      integer n

c internal variables
      integer i, iu
      real*8 lambda, band, chi2_

      data iu /10/

c observations were already read in chi2_func_CLO.f

c chi^2 for |T_3| data

      if (debug) then
        open(unit=iu,file="chi2_T3.dat",status="unknown")
        write(iu,*) "# t_OBS & u1 [m] & v1 [m] & u2 [m] & v2 [m]",
     :    " & lambda [m] & band [m]",
     :    " & t3amp_interp [] & sigma_t3amp_OBS [rad]",
     :    " & t3phi_interp [rad] & sigma_t3phi_OBS [rad] & dataset",
     :    " & chi^2"
        write(iu,*) "# t_OBS & u1 [m] & v1 [m] & u2 [m] & v2 [m]",
     :    " & lambda [m] & band [m]",
     :    " & t3amp_OBS    [] & sigma_t3amp_OBS [rad]",
     :    " & t3phi_OBS    [rad] & sigma_t3phi_OBS [rad] & dataset",
     :    " & chi^2"
      endif

      chi2 = 0.d0
      n = 0

      do i = 1, m_OBS

        lambda = lambda_eff_OBS(i)
        band = band_eff_OBS(i)
        chi2_ = ((t3amp(i)-t3amp_OBS(i))/sigma_t3amp_OBS(i))**2
        lns = lns + log(sigma_t3amp_OBS(i))
        chi2 = chi2 + chi2_
        n = n + 1

        if (debug) then
          write(iu,*) t_OBS(i), u1_OBS(i), v1_OBS(i), u2_OBS(i),
     :      v2_OBS(i), lambda, band, t3amp(i), sigma_t3amp_OBS(i),
     :      t3phi(i), sigma_t3phi_OBS(i), dataset_OBS(i), chi2_
          write(iu,*) t_OBS(i), u1_OBS(i), v1_OBS(i), u2_OBS(i),
     :      v2_OBS(i), lambda, band, t3amp_OBS(i), sigma_t3amp_OBS(i),
     :      t3phi_OBS(i), sigma_t3phi_OBS(i), dataset_OBS(i), chi2_
          write(iu,*)
        endif

      enddo

      if (debug) then
        close(iu)
      endif

      return
      end


