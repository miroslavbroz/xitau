c*************************************************************************
c                        BS.INC
c*************************************************************************
C Include file for the bs integrator
c
c Author:  Hal Levison  
c Date:    5/17/93
c Last revision: 

c...   Size of internal arrays
       integer N6DBS
       parameter(N6DBS=(6*(NTPMAX+NPLMAX)))
       integer NTEMP
       parameter(NTEMP=(72*(NTPMAX+NPLMAX)))

c...   maximum numbers of internations
       integer NTRYS
       parameter(NTRYS=30)
  
      real*8 time_from_bs_step
      common /cb_time/ time_from_bs_step

c------------------------------------------------------------------------
