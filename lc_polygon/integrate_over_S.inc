! integrate_over_S.inc
! Integrate over the surface.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

! Note: This is suitable for include '...', which is faster!

tot = 0.d0
!$omp parallel do reduction(+:tot) private(i) shared(mu_i,mu_e,surf,f_L,f,Phi_i,Phi_e,I_lambda,polys5)
do i = 1, size(surf,1)
  if (polys5(i)%c.eq.0) then
    f(i) = 0.d0
    Phi_i(i) = 0.d0
    Phi_e(i) = 0.d0
    I_lambda(i) = 0.d0
    cycle
  endif
  f(i) = f_ptr(f_L(i), mu_i(i), mu_e(i), alpha)
  Phi_i(i) = Phi_lambda*mu_i(i)
  I_lambda(i) = f(i)*Phi_i(i)
  Phi_e(i) = I_lambda(i)*mu_e(i)
  tot = tot + Phi_e(i)*surf(i)
enddo
!$omp end parallel do


