! filters.inc
! Effective wavelengths and bandpasses for photometric filters.
! Miroslav Broz (miroslav.broz@email.cz), Oct 1st 2022

! photometric bands
      integer WDBANDS
      parameter (WDBANDS=69)
      character*12 filters(WDBANDS)
      real*8 lambda_eff(WDBANDS), band_eff(WDBANDS), calib(WDBANDS)

! data from: http://hea.iki.rssi.ru/AZT22/RUS/eaa_bessel.pdf (with a correction of K-band)
! Johnson R and I passbands from Sterken & Manfroid (1992), p. 234
! MOST passband from Walker etal. (2003)

      data filters /                                                    &
     &  'u',                                                            &
     &  'v',                                                            &
     &  'b',                                                            &
     &  'y',                                                            &
     &  'U',                                                            &
     &  'B',                                                            &
     &  'V',                                                            &
     &  'R',                                                            &
     &  'I',                                                            &
     &  'J',                                                            &
     &  'K',                                                            &
     &  'L',                                                            &
     &  'M',                                                            &
     &  'N',                                                            &
     &  'Rc',                                                           &
     &  'Ic',                                                           &
     &  '230',                                                          &
     &  '250',                                                          &
     &  '270',                                                          &
     &  '290',                                                          &
     &  '310',                                                          &
     &  '330',                                                          &
     &  'TyB',                                                          &
     &  'TyV',                                                          &
     &  'HIP',                                                          &
     &  'CoRoT-exo',                                                    &
     &  'CoRoT-sis',                                                    &
     &  'H',                                                            &
     &  'Geneva U',                                                     &
     &  'Geneva B',                                                     &
     &  'Geneva B1',                                                    &
     &  'Geneva B2',                                                    &
     &  'Geneva V',                                                     &
     &  'Geneva V1',                                                    &
     &  'Geneva G',                                                     &
     &  'Kepler',                                                       &
     &  'SDSS u',                                                       &
     &  'SDSS g',                                                       &
     &  'SDSS r',                                                       &
     &  'SDSS i',                                                       &
     &  'SDSS z',                                                       &
     &  'LSST u',                                                       &
     &  'LSST g',                                                       &
     &  'LSST r',                                                       &
     &  'LSST i',                                                       &
     &  'LSST z',                                                       &
     &  'LSST y3',                                                      &
     &  'LSST y4',                                                      &
     &  'Walraven w',                                                   &
     &  'Walraven u',                                                   &
     &  'Walraven l',                                                   &
     &  'Walraven b',                                                   &
     &  'Walraven v',                                                   &
     &  'MOST',                                                         &
     &  'SWASP 2004',                                                   &
     &  'SWASP 2006',                                                   &
     &  'BRITE blue',                                                   &
     &  'BRITE red',                                                    &
     &  'TESS',                                                         &
     &  'Spitzer b1',                                                   &
     &  'Spitzer b1',                                                   &
     &  'Spitzer b3',                                                   &
     &  'Spitzer b3',                                                   &
     &  'STEREO HI-1A',                                                 &
     &  'STEREO HI-1A',                                                 &
     &  'STEREO HI-1B',                                                 &
     &  'STEREO HI-1B',                                                 &
     &  'SMEI',                                                         &
     &  'Gaia G'                                                        &
     &  /

      data lambda_eff /                                                 &
     &   349.d-9,                                                       &
     &   411.d-9,                                                       &
     &   467.d-9,                                                       &
     &   547.d-9,                                                       &
     &   367.d-9,                                                       &  ! U 
     &   436.d-9,                                                       &  ! B 
     &   545.d-9,                                                       &  ! V 
     &   720.d-9,                                                       &  ! R 
     &   900.d-9,                                                       &  ! I 
     &  1220.d-9,                                                       &  ! J 
     &  2190.d-9,                                                       &  ! K 
     &  3450.d-9,                                                       &  ! L 
     &  4750.d-9,                                                       &  ! M 
     & 10500.d-9,                                                       &  ! N 
     &   638.d-9,                                                       &  ! Rc
     &   797.d-9,                                                       &  ! Ic
     &   230.d-9,                                                       &
     &   250.d-9,                                                       &
     &   270.d-9,                                                       &
     &   290.d-9,                                                       &
     &   310.d-9,                                                       &
     &   330.d-9,                                                       &
     &   430.d-9,                                                       &
     &   520.d-9,                                                       &
     &   500.d-9,                                                       &
     &   640.d-9,                                                       &
     &   640.d-9,                                                       &
     &  1630.d-9,                                                       &  ! H
     &   345.6d-9,                                                      &
     &   424.5d-9,                                                      &
     &   402.4d-9,                                                      &
     &   448.d-9,                                                       &
     &   550.d-9,                                                       &
     &   540.5d-9,                                                      &
     &   580.5d-9,                                                      &
     &   592.0d-9,                                                      &
     &   355.7d-9,                                                      &
     &   482.5d-9,                                                      &
     &   626.1d-9,                                                      &
     &   767.2d-9,                                                      &
     &   909.7d-9,                                                      &
     &   367.d-9,                                                       &
     &   485.d-9,                                                       &
     &   624.d-9,                                                       &
     &   752.d-9,                                                       &
     &   867.d-9,                                                       &
     &   963.d-9,                                                       &
     &   963.d-9,                                                       &
     &   326.d-9,                                                       &
     &   362.d-9,                                                       &
     &   382.d-9,                                                       &
     &   431.d-9,                                                       &
     &   540.d-9,                                                       &
     &   525.d-9,                                                       &  ! MOST      
     &   550.d-9,                                                       &                             
     &   550.d-9,                                                       &                             
     &   426.d-9,                                                       &  ! BRITE blue
     &   621.d-9,                                                       &  ! BRITE red 
     &   797.d-9,                                                       &  ! TESS      
     &  3600.d-9,                                                       &
     &  4500.d-9,                                                       &
     &  5800.d-9,                                                       &
     &  8000.d-9,                                                       &
     &   690.d-9,                                                       &
     &   690.d-9,                                                       &
     &   690.d-9,                                                       &
     &   690.d-9,                                                       &
     &   700.d-9,                                                       &
     &   639.02d-9                                                      &  ! Gaia
     &  /       

      data band_eff /                                                   &
     &    30.d-9,                                                       &
     &    19.d-9,                                                       &
     &    18.d-9,                                                       &
     &    23.d-9,                                                       &
     &    66.d-9,                                                       &  ! U 
     &    94.d-9,                                                       &  ! B 
     &    85.d-9,                                                       &  ! V 
     &   220.d-9,                                                       &  ! R 
     &   240.d-9,                                                       &  ! I 
     &   213.d-9,                                                       &  ! J 
     &   390.d-9,                                                       &  ! K 
     &   472.d-9,                                                       &  ! L 
     &   460.d-9,                                                       &  ! M 
     &  2500.d-9,                                                       &  ! N 
     &   160.d-9,                                                       &  ! Rc
     &   149.d-9,                                                       &  ! Ic
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &   307.d-9,                                                       &  ! H
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &   300.d-9,                                                       &  ! MOST      
     &     0.d-9,                                                       &                              
     &     0.d-9,                                                       &                              
     &    61.d-9,                                                       &  ! BRITE blue
     &   145.d-9,                                                       &  ! BRITE red 
     &   400.d-9,                                                       &  ! TESS      
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &     0.d-9,                                                       &
     &   317.32d-9                                                      &  ! Gaia
     &  /

! flux calibrations F_lambda [in J s^−1 m^-2 m^-1] for a star with 0.0 magnitude;
! data from ebdoc.13july2015.pdf (Wilson, Van Hamme, & Terrell 2010),
! converted from erg s^-1 cm^-2 cm^-1 (i.e. divided by 10);

      data calib /                                                      &
     &  0.11799d0,                                                      &
     &  0.08420d0,                                                      &
     &  0.058895d0,                                                     &
     &  0.037415d0,                                                     &
     &  0.04306d0,                                                      &  ! U 
     &  0.06266d0,                                                      &  ! B 
     &  0.036895d0,                                                     &  ! V 
     &  0.017813d0,                                                     &  ! R 
     &  0.008643d0,                                                     &  ! I 
     &  0.003147d0,                                                     &  ! J 
     &  0.0003961d0,                                                    &  ! K 
     &  0.0000708d0,                                                    &  ! L 
     &  0.000022d0,                                                     &  ! M 
     &  0.00000124d0,                                                   &  ! N 
     &  0.02177d0,                                                      &  ! Rc
     &  0.01126d0,                                                      &  ! Ic
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.001133d0,                                                     &  ! H
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &  ! MOST 
     &  0.0d0,                                                          &                         
     &  0.0d0,                                                          &                         
     &  0.0d0,                                                          &  ! BRITE
     &  0.0d0,                                                          &  ! BRITE
     &  0.0d0,                                                          &  ! TESS 
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0d0,                                                          &
     &  0.0184503582426421d0                                            &  ! Gaia; see vypocty3_gaia.plt
     &  /

