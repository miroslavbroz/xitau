      SUBROUTINE trapzd(func,a,b,s,n)
      implicit none
      INTEGER n
      REAL*8 a,b,s,func
      EXTERNAL func
c          This routine computes the nth stage of refinement of an extended trapezoidal rule. func is
c          input as the name of the function to be integrated between limits a and b, also input. When
c          called with n=1, the routine returns as s the crudest estimate of a f (x)dx. Subsequent
c          calls with n=2,3,... (in that sequential order) will improve the accuracy of s by adding 2n-2
c          additional interior points. s should not be modified between sequential calls.
      INTEGER it,j
      REAL*8 del,sum,tnm,x
      if (n.eq.1) then
           s=0.5d0*(b-a)*(func(a)+func(b))
      else
           it=2**(n-2)
           tnm=it
           del=(b-a)/tnm                     ! This is the spacing of the points to be added.
           x=a+0.5d0*del
           sum=0.d0
           do 11 j=1,it
                sum=sum+func(x)
                x=x+del
11         enddo
           s=0.5d0*(s+(b-a)*sum/tnm)         ! This replaces s by its refined value.
      endif
      return
      END

