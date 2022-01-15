c filters.inc
c Effective wavelengths and bandpasses for photometric filters.
c Miroslav Broz (miroslav.broz@email.cz), Oct 13th 2016

c photometric bands
      integer WDBANDS
      parameter (WDBANDS=17)
      character*8 filters(WDBANDS)
      real*8 lambda_eff(WDBANDS), band_eff(WDBANDS), calib(WDBANDS)

c data from: http://hea.iki.rssi.ru/AZT22/RUS/eaa_bessel.pdf (with a correction of K-band)
c Johnson R and I passbands from Sterken & Manfroid (1992), p. 234

      data filters /
     :  'u',
     :  'v',
     :  'b',
     :  'y',
     :  'U',
     :  'B',
     :  'V',
     :  'R',
     :  'I',
     :  'J',
     :  'H',
     :  'K',
     :  'L',
     :  'M',
     :  'N',
     :  'Rc',
     :  'Ic'/

      data lambda_eff /
     :   349.d-9,
     :   411.d-9,
     :   467.d-9,
     :   547.d-9,
     :   367.d-9,
     :   436.d-9,
     :   545.d-9,
     :   720.d-9,
     :   900.d-9,
     :  1220.d-9,
     :  1630.d-9,
     :  2190.d-9,
     :  3450.d-9,
     :  4750.d-9,
     : 10500.d-9,
     :   638.d-9,
     :   797.d-9/

      data band_eff /
     :    30.d-9,
     :    19.d-9,
     :    18.d-9,
     :    23.d-9,
     :    66.d-9,
     :    94.d-9,
     :    85.d-9,
     :   220.d-9,
     :   240.d-9,
     :   213.d-9,
     :   307.d-9,
     :   390.d-9,
     :   472.d-9,
     :   460.d-9,
     :  2500.d-9,
     :   160.d-9,
     :   149.d-9/

c flux calibrations F_lambda [in J s^−1 m^-2 m^-1] for a star with 0.0 magnitude;
c data from ebdoc.13july2015.pdf (Wilson, Van Hamme, & Terrell 2010),
c converted from erg s^-1 cm^-2 cm^-1 (i.e. divided by 10);

      data calib /
     : 0.11799d0,
     : 0.08420d0,
     : 0.058895d0,
     : 0.037415d0,
     : 0.04306d0,
     : 0.06266d0,
     : 0.036895d0,
     : 0.017813d0,
     : 0.008643d0,
     : 0.003147d0,
     : 0.001133d0,
     : 0.0003961d0,
     : 0.0000708d0,
     : 0.000022d0,
     : 0.00000124d0,
     : 0.02177d0,
     : 0.01126d0/

c from lc_2005-10-28.f:
c
c     band(1)=' u '
c     band(2)=' v '
c     band(3)=' b '
c     band(4)=' y '
c     band(5)=' U '
c     band(6)=' B '
c     band(7)=' V '
c     band(8)=' R '
c     band(9)=' I '
c     band(10)=' J '
c     band(11)=' K '
c     band(12)=' L '
c     band(13)=' M '
c     band(14)=' N '
c     band(15)=' Rc'
c     band(16)=' Ic'
c     band(17)='230'
c     band(18)='250'
c     band(19)='270'
c     band(20)='290'
c     band(21)='310'
c     band(22)='330'
c     band(23)='TyB'
c     band(24)='TyV'
c     band(25)='HIP'


