C ------------------------------------------------------------------------------
C                             PROGRAM SHARP
C  SIMULATION OF FRESHWATER AND SALTWATER FLOW ASSUMING AN ABRUPT INTERFACE
C           A QUASI-THREE DIMENSIONAL FINITE DIFFERENCE MODEL
C                     VERSION AS OF SEPT. 12, 1990
C
C  SEE WRIR 90-4130, ATTACHMENT A FOR INPUT FORMATS, ATTACHMENT B FOR DEFINITION
C                          OF PROGRAM VARIABLES
C ------------------------------------------------------------------------------
      BLOCK DATA
      COMMON /IUNITS/IUN(32),IOP(12)                                      MA 390
C                                                                         MA 400
      DATA IUN/22*5,2*6,6*7,8,9/                                          MA 410
C     DATA IOP/0,1,1,1,1,1,0,0,1,1,0,0/                                   MA 420
C                                                                         MA 421
C  TO OPEN FILES INTERNALLY, REPLACE THE ABOVE DATA STATEMENT WITH THE    MA 422
C  FOLLOWING DATA STATEMENT                                               MA 423
      DATA IOP/1,1,1,1,1,1,0,0,1,1,0,0/                                   MA 424
      END
C
C  SPECIFICATIONS                                                         MA  10
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 MA  20
      PARAMETER (NR=40,NC=47,NL=5)                                        MA  30
      CHARACTER ICODE,IC,FR,M,S,TITLE*70                                  MA  40
C                                                                         MA  50
      COMMON /INTPAR/ NNR,NNC,NNL,NTSP,NP,NITP,NIT,NPUMP,IPUMP            MA  60
     1,ITMAX,NT,IWT(NR,NC),NFLAG,NCONT,NINT,                              MA  70
     2LW,IW(10),JW(10),KW(10),NUP,NLK                                     MA  80
C                                                                         MA  90
      COMMON /CHAR/ICODE(NR,NC,NL),IC(10,200),FR,M,S                      MA 100
C                                                                         MA 110
      COMMON /VALUES/ DT,TFAC,ERR,RFAC,SGF,SGS,VIF,VIS,DEL,TSEC,TMAX      MA 120
     1,TPUMP,STST,WFAC,WITER                                              MA 130
C                                                                         MA 140
      COMMON /ARRAYS/ FKX(NR,NC,NL),FKY(NR,NC,NL),                        MA 150
     1STORF(NR,NC,NL),STORS(NR,NC,NL),POR(NR,NC,NL),THCK(NR,NC,NL),       MA 160
     2ZBOT(NR,NC,NL),ZINT(NR,NC,NL),AQL(NR,NC,NL),RECH(NR,NC),            MA 170
     3PUMP(NR,NC,NL),PHIF(NR,NC,NL),PHIS(NR,NC,NL),DX(NC),DY(NR),         MA 180
     4WOPTF(10),TTFX(NR,NC,NL),TTFY(NR,NC,NL),TTSX(NR,NC,NL),             MA 190
     5TTSY(NR,NC,NL),DMAXF(200),DMAXS(200),THF(NR,NC,NL),                 MA 200
     6THS(NR,NC,NL),HEAD(NR,NC),PHIFP(NR,NC,NL),PHISP(NR,NC,NL),          MA 210
     7WTOP(NR,NC,NL),WBOT(NR,NC,NL),PUMPF(NR,NC,NL),FAREA(NR,NC,NL),      MA 220
     8SAREA(NR,NC,NL),ZTOP(NR,NC,NL),BATH(NR,NC),                         MA 230
     9FX(NR,NC,NL),FY(NR,NC,NL),SX(NR,NC,NL),SY(NR,NC,NL)                 MA 240
C                                                                         MA 250
      COMMON /XNEW/FXN(NR,NC,NL),FYN(NR,NC,NL),SXN(NR,NC,NL),             MA 260
     1SYN(NR,NC,NL)                                                       MA 270
C                                                                         MA 280
      COMMON /COEF/DPHIF(NR,NC,NL),DPHIS(NR,NC,NL),VF(NR,NC,NL),          MA 290
     1VS(NR,NC,NL),E(NR,NC,NL,4),F(NR,NC,NL,4),G(NR,NC,NL,4),A(4),        MA 300
     2B(4),C(4),D(4),AA(4),BB(4),CC(4),DD(4),EE(4),FF(4),GG(4),HH(4),     MA 310
     3SS(4),TT(4),UU(4),WW(4),ZZ(4),BBB(4),ZZZ(4),ARRAY(4),EK1(4),        MA 320
     4EI1(4),FJ1(4),GJ1(4),FK1(4),GI1(4),EIJK(4),FIJK(4),GIJK(4),         MA 330
     5TFX(NR,NC,NL),TFY(NR,NC,NL),TIME(200),FWH(10,200),SWH(10,200),      MA 340
     6ZI(10,200),FAR(10,200),SAR(10,200)                                  MA 350
C                                                                         MA 360
      COMMON /WHEAD/WPHIF(NR,NC),WPHIS(NR,NC),WZINT(NR,NC)                MA 370
C                                                                         MA 380
      COMMON /IUNITS/IUN(32),IOP(12)                                      MA 390
C                                                                         MA 430
      IF(IOP(1).EQ.1)THEN                                                 MA 440
        OPEN(UNIT=5,FILE='INPUT',STATUS='OLD')                            MA 450
        OPEN(UNIT=6,FILE='OUTPUT',STATUS='NEW')                           MA 460
        OPEN(UNIT=7,FILE='RESULTS',STATUS='NEW')                          MA 470
        OPEN(UNIT=8,FILE='HEADS',STATUS='NEW')                            MA 480
        OPEN(UNIT=9,FILE='LEAK',STATUS='NEW')                             MA 490
      END IF                                                              MA 500
C                                                                         MA 510
C  READ AND WRITE MODEL INPUT                                             MA 520
      CALL INPUT                                                          MA 530
C                                                                         MA 540
C  SET VALUES EQUAL TO ZERO AT INACTIVE NODES                             MA 550
      DO 10 K=1,NNL                                                       MA 560
      DO 10 I=1,NNR                                                       MA 570
      DO 10 J=1,NNC                                                       MA 580
        TTFX(I,J,K)=0.0                                                   MA 581
        TTFY(I,J,K)=0.0                                                   MA 582
        TTSX(I,J,K)=0.0                                                   MA 583
        TTSY(I,J,K)=0.0                                                   MA 584
        THF(I,J,K)=0.0                                                    MA 585
        THS(I,J,K)=0.0                                                    MA 586
        IF(FKX(I,J,K).NE.0.) GO TO 10                                     MA 590
        FKY(I,J,K)=0.0                                                    MA 600
        POR(I,J,K)=0.0                                                    MA 610
        THCK(I,J,K)=0.0                                                   MA 620
        ZINT(I,J,K)=0.0                                                   MA 630
        AQL(I,J,K)=0.0                                                    MA 640
        FX(I,J,K)=0.0                                                     MA 641
        FY(I,J,K)=0.0                                                     MA 642
        SX(I,J,K)=0.0                                                     MA 643
        SY(I,J,K)=0.0                                                     MA 644
  10  CONTINUE                                                            MA 650
C                                                                         MA 660
      FR='F'                                                              MA 670
      M='M'                                                               MA 680
      S='S'                                                               MA 690
      NFLAG=1                                                             MA 700
      DEL=SGF/(SGS-SGF)                                                   MA 710
      C2=SGF/SGS                                                          MA 720
      C3=1./(1.+DEL)                                                      MA 730
C                                                                         MA 740
C  CALCULATE INITIAL SALTWATER POTENTIAL AND TYPE OF NODE:                MA 750
C    F-FRESHWATER ONLY, S-SALTWATER ONLY, M-FRESH AND SALTWATER           MA 760
      DO 20 K=1,NNL                                                       MA 770
      DO 20 I=1,NNR                                                       MA 780
      DO 20 J=1,NNC                                                       MA 790
        FAREA(I,J,K)=0.                                                   MA 810
        SAREA(I,J,K)=0.                                                   MA 820
        ICODE(I,J,K)='.'                                                  MA 830
        FXN(I,J,K)=0.                                                     MA 840
        FYN(I,J,K)=0.                                                     MA 850
        SXN(I,J,K)=0.                                                     MA 860
        SYN(I,J,K)=0.                                                     MA 870
        ZTOP(I,J,K)=ZBOT(I,J,K)+THCK(I,J,K)                               MA 880
        IF(FKX(I,J,K).EQ.0.)GO TO 20                                      MA 890
        IF(IWT(I,J).EQ.1.AND.K.EQ.NNL)THEN                                MA 900
          ZTOP(I,J,K)=PHIF(I,J,K)                                         MA 901
          IF(ZTOP(I,J,K).GT.(ZBOT(I,J,K)+THCK(I,J,K)))WRITE(IUN(23),170)  MA 902
     &      I,J,K                                                         MA 903
        END IF                                                            MA 904
        TOP=ZTOP(I,J,K)                                                   MA 910
        THICK=TOP-ZBOT(I,J,K)                                             MA 920
        IF(ZINT(I,J,K).LT.ZBOT(I,J,K))ZINT(I,J,K)=ZBOT(I,J,K)             MA 930
        IF(ZINT(I,J,K).GT.TOP)ZINT(I,J,K)=TOP                             MA 940
        THS(I,J,K)=ZINT(I,J,K)-ZBOT(I,J,K)                                MA 950
        THF(I,J,K)=ZTOP(I,J,K)-ZINT(I,J,K)                                MA 960
        IF(NCONT.GT.0)GO TO 20                                            MA 970
        FX(I,J,K)=0.0                                                     MA 971
        FY(I,J,K)=0.0                                                     MA 972
        SX(I,J,K)=0.0                                                     MA 973
        SY(I,J,K)=0.0                                                     MA 974
        IF(DABS(ZINT(I,J,K)-ZBOT(I,J,K)).LE.(0.001*THICK))THEN            MA 980
          ICODE(I,J,K)=FR                                                 MA 990
          FAREA(I,J,K)=1.                                                 MA1000
          FX(I,J,K)=1.                                                    MA1010
          FXN(I,J,K)=1.                                                   MA1020
          FY(I,J,K)=1.                                                    MA1030
          FYN(I,J,K)=1.                                                   MA1040
       ELSE IF(DABS(ZINT(I,J,K)-TOP).LE.(0.001*THICK))THEN                MA1050
          ICODE(I,J,K)=S                                                  MA1060
          SAREA(I,J,K)=1.                                                 MA1070
          SX(I,J,K)=1.                                                    MA1080
          SXN(I,J,K)=1.                                                   MA1090
          SY(I,J,K)=1.                                                    MA1100
          SYN(I,J,K)=1.                                                   MA1110
        ELSE                                                              MA1120
          ICODE(I,J,K)=M                                                  MA1130
          FAREA(I,J,K)=1.                                                 MA1140
          SAREA(I,J,K)=1.                                                 MA1150
          FX(I,J,K)=1.                                                    MA1160
          FXN(I,J,K)=1.                                                   MA1170
          FY(I,J,K)=1.                                                    MA1180
          FYN(I,J,K)=1.                                                   MA1190
          SX(I,J,K)=1.                                                    MA1200
          SXN(I,J,K)=1.                                                   MA1210
          SY(I,J,K)=1.                                                    MA1220
          SYN(I,J,K)=1.                                                   MA1230
        END IF                                                            MA1240
  20  CONTINUE                                                            MA1250
C                                                                         MA1260
C  CALCULATE ITERATION PARAMETERS                                         MA1270
      CALL IPARAM                                                         MA1280
C                                                                         MA1290
C  CALCULATE HYDRAULIC CONDUCTIVITIES AT GRID BLOCK BOUNDARIES            MA1300
      CALL KMEAN                                                          MA1310
C                                                                         MA1320
      NT=0                                                                MA1330
      TIME(1)=TSEC/31557600.                                              MA1340
      DO 25 L=1,LW                                                        MA1350
        I=IW(L)                                                           MA1360
        J=JW(L)                                                           MA1370
        K=KW(L)                                                           MA1380
        FWH(L,1)=PHIF(I,J,K)                                              MA1390
        SWH(L,1)=PHIS(I,J,K)                                              MA1400
        ZI(L,1)=ZINT(I,J,K)                                               MA1410
        IC(L,1)=ICODE(I,J,K)                                              MA1420
        FAR(L,1)=FAREA(I,J,K)                                             MA1430
        SAR(L,1)=SAREA(I,J,K)                                             MA1440
  25  CONTINUE                                                            MA1450
C                                                                         MA1460
C  BEGIN NEW PUMPING PERIOD CALCULATIONS                                  MA1470
      DO 90 IPUMP=1,NPUMP                                                 MA1480
        CALL PUMPER(IPUMP)                                                MA1490
        TSEC=TSEC+DT                                                      MA1500
        TPUMP=DT                                                          MA1510
C                                                                         MA1520
C  BEGIN CALCULATIONS FOR NEW TIME STEP                                   MA1530
  30  NT=NT+1                                                             MA1540
      DFMAX=0.0                                                           MA1550
      DSMAX=0.0                                                           MA1560
C                                                                         MA1570
C  RESET POTENTIALS FOR PREVIOUS TIME STEP                                MA1580
      DO 40 K=1,NNL                                                       MA1590
      DO 40 I=1,NNR                                                       MA1600
      DO 40 J=1,NNC                                                       MA1610
        PHIFP(I,J,K)=PHIF(I,J,K)                                          MA1620
        PHISP(I,J,K)=PHIS(I,J,K)                                          MA1630
  40  CONTINUE                                                            MA1640
C                                                                         MA1650
C  SOLVE FRESHWATER AND SALTWATER FLOW EQUATIONS                          MA1660
        CALL SIP(*100)                                                    MA1670
C                                                                         MA1680
C  DETERMINE MAXIMUM FRESHWATER AND SALTWATER POTENTIAL CHANGE OVER       MA1690
C    TIME STEP                                                            MA1700
        DO 60 K=1,NNL                                                     MA1710
        DO 60 I=1,NNR                                                     MA1720
        DO 60 J=1,NNC                                                     MA1730
          DELF=DABS(PHIFP(I,J,K)-PHIF(I,J,K))                             MA1740
          DELS=DABS(PHISP(I,J,K)-PHIS(I,J,K))                             MA1750
          IF(DELF.GT.DFMAX)DFMAX=DELF                                     MA1760
          IF(DELS.GT.DSMAX)DSMAX=DELS                                     MA1770
  60    CONTINUE                                                          MA1780
C                                                                         MA1790
C  WRITE CURRENT RESULTS TO FILE                                          MA1800
      IF(IOP(12).EQ.0)THEN                                                MA1810
        CALL RESULTS                                                      MA1820
        REWIND(IUN(25))                                                   MA1830
        REWIND(IUN(26))                                                   MA1840
        REWIND(IUN(27))                                                   MA1850
        REWIND(IUN(28))                                                   MA1860
        REWIND(IUN(29))                                                   MA1870
        REWIND(IUN(30))                                                   MA1880
      END IF                                                              MA1890
C                                                                         MA1900
C  STOP IF MAXIMUM NUMBER OF TIME STEPS IS REACHED                        MA1910
      IF(NT.GE.NTSP)THEN                                                  MA1920
        CALL OUTPUT(1)                                                    MA1930
        IF(IOP(12).GT.0)CALL RESULTS                                      MA1940
C                                                                         MA1950
C  PRINT RESULTS AND BEGIN NEW PUMPING PERIOD IF STEADY STATE HAS         MA1960
C    BEEN REACHED                                                         MA1970
      ELSE IF(DFMAX.LT.STST.AND.DSMAX.LT.STST)THEN                        MA1980
          WRITE(IUN(24),70)                                               MA1990
          CALL OUTPUT(1)                                                  MA2000
          IF(IOP(12).GT.0)CALL RESULTS                                    MA2010
          DT=TMAX-TPUMP                                                   MA2020
          TPUMP=TMAX                                                      MA2030
          TSEC=TSEC+DT                                                    MA2040
          GO TO 90                                                        MA2050
C                                                                         MA2060
      ELSE                                                                MA2070
C                                                                         MA2080
C  PRINT RESULTS IF NUMBER OF TIME STEPS BETWEEN PRINTOUTS IS REACHED     MA2090
        IF(MOD(NT,NP).EQ.0.)THEN                                          MA2100
          CALL OUTPUT(1)                                                  MA2110
          IF(IOP(12).GT.0)CALL RESULTS                                    MA2120
        ELSE IF(IPUMP.EQ.NPUMP.AND.TPUMP.GE.TMAX)THEN                     MA2130
          CALL OUTPUT(1)                                                  MA2140
          IF(IOP(12).GT.0)CALL RESULTS                                    MA2150
          GO TO 90                                                        MA2160
        ELSE                                                              MA2170
          CALL OUTPUT(0)                                                  MA2180
        END IF                                                            MA2190
        DT=TFAC*DT                                                        MA2200
C                                                                         MA2210
C  BEGIN NEW TIME STEP                                                    MA2220
        IF((TPUMP+DT).LT.TMAX) THEN                                       MA2230
          TPUMP=TPUMP+DT                                                  MA2240
          TSEC=TSEC+DT                                                    MA2250
          GO TO 30                                                        MA2260
        ELSE IF(TPUMP.NE.TMAX) THEN                                       MA2270
          DT=TMAX-TPUMP                                                   MA2280
          TPUMP=TMAX                                                      MA2290
          TSEC=TSEC+DT                                                    MA2300
          GO TO 30                                                        MA2310
        END IF                                                            MA2390
      END IF                                                              MA2400
  90  CONTINUE                                                            MA2410
 100  CONTINUE                                                            MA2420
C                                                                         MA2430
C  WRITE OBSERVED HEADS AND INTERFACE ELEVATIONS TO FILE                  MA2440
      DO 140 L=1,LW                                                       MA2450
        I=IW(L)                                                           MA2460
        J=JW(L)                                                           MA2470
        K=KW(L)                                                           MA2480
        WRITE(IUN(31),150)I,J,K                                           MA2490
        WRITE(IUN(31),160)(TIME(N),FWH(L,N),SWH(L,N),ZI(L,N),IC(L,N),     MA2500
     &FAR(L,N),SAR(L,N),N=1,NT+1)                                         MA2510
 140  CONTINUE                                                            MA2520
C                                                                         MA2530
C  CLOSE OUTPUT FILES                                                     MA2540
      IF(IOP(1).EQ.1)THEN                                                 MA2550
        CLOSE(5,STATUS='KEEP')                                            MA2560
        CLOSE(6,STATUS='KEEP')                                            MA2570
        CLOSE(7,STATUS='KEEP')                                            MA2580
        CLOSE(8,STATUS='KEEP')                                            MA2590
        CLOSE(9,STATUS='KEEP')                                            MA2600
      END IF                                                              MA2610
C                                                                         MA2620
   70 FORMAT(/1X,25('*')/' STEADY STATE ACHIEVED AT:'/1X,25('*'))         MA2630
   80 FORMAT(10F10.3)                                                     MA2640
  150 FORMAT(/' NODE(',I3,',',I3,',',I3,'):'//'    TIME(YR)    FW HEAD',  MA2650
     1'     SW HEAD    INTERFACE     ICODE     FAREA     SAREA')          MA2660
  160 FORMAT(4F12.5,7X,A1,2F12.5)                                         MA2670
  170 FORMAT(/' ***WARNING: WATER TABLE ABOVE LAND SURFACE AT BLOCK:',    MA2671
     &I3,2X,I3,2X,I3)                                                     NA2672
      END                                                                 MA2680
C                                                                         MA2690
C ------------------------------------------------------------------------IN  10
      SUBROUTINE INPUT                                                    IN  20
C ------------------------------------------------------------------------IN  30
C                                                                         IN  40
C  A SUBROUTINE TO READ AND WRITE MODEL INPUT PARAMETERS                  IN  50
C                                                                         IN  60
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 IN  70
      PARAMETER (NR=40,NC=47,NL=5)                                        IN  80
      CHARACTER ICODE,IC,FR,M,S,TITLE*70                                  IN  90
C                                                                         IN 100
      COMMON /INTPAR/ NNR,NNC,NNL,NTSP,NP,NITP,NIT,NPUMP,IPUMP            IN 110
     1,ITMAX,NT,IWT(NR,NC),NFLAG,NCONT,NINT,                              IN 120
     2LW,IW(10),JW(10),KW(10),NUP,NLK                                     IN 130
C                                                                         IN 140
      COMMON /CHAR/ICODE(NR,NC,NL),IC(10,200),FR,M,S                      IN 150
C                                                                         IN 160
      COMMON /VALUES/ DT,TFAC,ERR,RFAC,SGF,SGS,VIF,VIS,DEL,TSEC,TMAX      IN 170
     1,TPUMP,STST,WFAC,WITER                                              IN 180
C                                                                         IN 190
      COMMON /ARRAYS/ FKX(NR,NC,NL),FKY(NR,NC,NL),                        IN 200
     1STORF(NR,NC,NL),STORS(NR,NC,NL),POR(NR,NC,NL),THCK(NR,NC,NL),       IN 210
     2ZBOT(NR,NC,NL),ZINT(NR,NC,NL),AQL(NR,NC,NL),RECH(NR,NC),            IN 220
     3PUMP(NR,NC,NL),PHIF(NR,NC,NL),PHIS(NR,NC,NL),DX(NC),DY(NR),         IN 230
     4WOPTF(10),TTFX(NR,NC,NL),TTFY(NR,NC,NL),TTSX(NR,NC,NL),             IN 240
     5TTSY(NR,NC,NL),DMAXF(200),DMAXS(200),THF(NR,NC,NL),                 IN 250
     6THS(NR,NC,NL),HEAD(NR,NC),PHIFP(NR,NC,NL),PHISP(NR,NC,NL),          IN 260
     7WTOP(NR,NC,NL),WBOT(NR,NC,NL),PUMPF(NR,NC,NL),FAREA(NR,NC,NL),      IN 270
     8SAREA(NR,NC,NL),ZTOP(NR,NC,NL),BATH(NR,NC),                         IN 280
     9FX(NR,NC,NL),FY(NR,NC,NL),SX(NR,NC,NL),SY(NR,NC,NL)                 IN 290
C                                                                         IN 300
      COMMON /IUNITS/IUN(32),IOP(12)                                      IN 310
C                                                                         IN 320
      READ(IUN(1),10)TITLE                                                IN 330
      WRITE(IUN(23),20)TITLE                                              IN 340
      WRITE(IUN(31),20)TITLE                                              IN 350
      WRITE(IUN(32),20)TITLE                                              IN 360
      READ(IUN(1),30)NNR,NNC,NNL,NP,ITMAX,NITP,NPUMP,LW,NUP,NOPT,NLK      IN 370
      WRITE(IUN(23),40)NNR,NNC,NNL,NP,ITMAX,NITP,NPUMP,NUP,NOPT,NLK       IN 380
      IF(NNR.GT.NR)WRITE(IUN(23),41)                                      IN 381
      IF(NNC.GT.NC)WRITE(IUN(23),42)                                      IN 382
      IF(NNL.GT.NL)WRITE(IUN(23),43)                                      IN 383
      READ(IUN(1),50)ERR,STST,RFAC,WFAC,WITER,SGF,SGS,VIF,VIS             IN 390
      WRITE(IUN(23),60)ERR,STST,RFAC,WFAC,WITER,SGF,SGS,VIF,VIS           IN 400
C                                                                         IN 750
C  CONTINUATION RUN INFORMATION                                           IN 760
      READ(IUN(1),31)NCONT,TSEC,NINT                                      IN 770
  31  FORMAT(I5,G20.0,I5)                                                 IN 780
      IF(NCONT.GT.0)THEN                                                  IN 790
        WRITE(IUN(23),32)TSEC                                             IN 800
  32    FORMAT(/ 'CONTINUATION RUN'/'   INITIAL TIME =',G20.10,'SEC')     IN 810
        NINT=0                                                            IN 820
      ELSE                                                                IN 830
        WRITE(IUN(23),33)                                                 IN 840
  33    FORMAT(/' NEW RUN')                                               IN 850
        IF(NINT.GT.0)WRITE(IUN(23),34)                                    IN 860
  34    FORMAT('    INTERFACE ELEVATIONS INITIALIZED TO -DEL*PHIF')       IN 870
      END IF                                                              IN 880
C                                                                         IN 410
      IF(LW.GT.0)THEN                                                     IN 420
        READ(IUN(1),29)(IW(L),JW(L),KW(L),L=1,LW)                         IN 430
  29  FORMAT(3I5)                                                         IN 440
        WRITE(IUN(23),65)LW                                               IN 450
        WRITE(IUN(23),29)(IW(L),JW(L),KW(L),L=1,LW)                       IN 460
  65  FORMAT(/' LOCATIONS OF ',I4,' OBSERVATION NODES TRACKED:')          IN 470
      END IF                                                              IN 480
C                                                                         IN 490
C  USER SPECIFIED OPTIONS                                                 IN 500
      IF(NOPT.EQ.1)READ(IUN(1),30)(IOP(I),I=2,12)                         IN 510
        WRITE(IUN(23),35)(IOP(I),I=2,12)                                  IN 520
  35  FORMAT(/' SPECIFIED INPUT/OUTPUT OPTIONS:'/                         IN 520
     &/I5,': 1=PRINT INPUT AQUIFER PARAMETERS, 0=DO NOT PRINT'/           IN 540
     &I5,': 1=PRINT INPUT PUMPING PERIOD WELL AND RECHARGE DATA, ',       IN 550
     &'0=DO NOT PRINT'/I5,': 1=PRINT CALCULATED FRESHWATER HEADS,',       IN 560
     &' 0=DO NOT PRINT'/I5,': 1=PRINT CALCULATED SALTWATER HEADS,',       IN 570
     &' 0=DO NOT PRINT'/I5,': 1=PRINT CALCULATED INTERFACE ELEVATIONS,',  IN 580
     &' 0=DO NOT PRINT'/I5,': 1=PRINT CALCULATED FAREA FACTORS,',         IN 590
     &' 0=DO NOT PRINT'/I5,': 1=PRINT CALCULATED SAREA FACTORS,',         IN 600
     &' 0=DO NOT PRINT'/I5,': 1=PRINT F-M-S MAP, 0=DO NOT PRINT'/I5,      IN 610
     &': 1=WRITE BLOCK LEAKAGE VALUES TO FILE, 0=DO NOT WRITE TO',        IN 620
     &' FILE'/I5,': 1=PRINT ITERATION INFORMATION TO SCREEN, 0=DO NOT',   IN 630
     &' PRINT'/I5,': 0=REWRITE LATEST RESULTS TO FILE AFTER EACH TIME',   IN 640
     &' STEP,'/'       1=WRITE RESULTS TO FILE EVERY NP TIME STEPS')      IN 650
C                                                                         IN 660
C  READ AND WRITE AQUIFER PARAMETERS LAYER BY LAYER                       IN 900
      DO 70 K=1,NNL                                                       IN 910
        IF(IOP(2).EQ.1)WRITE(IUN(23),80)K                                 IN 920
        IF(IOP(2).EQ.1)WRITE(IUN(23),90)                                  IN 930
        CALL READIN(FKX,NNR,NNC,K,IUN(2),IUN(23),IOP(2))                  IN 940
        IF(IOP(2).EQ.1)WRITE(IUN(23),100)                                 IN 950
        CALL READIN(FKY,NNR,NNC,K,IUN(3),IUN(23),IOP(2))                  IN 960
        IF(IOP(2).EQ.1)WRITE(IUN(23),110)                                 IN 970
        CALL READIN(STORF,NNR,NNC,K,IUN(4),IUN(23),IOP(2))                IN 980
        IF(IOP(2).EQ.1)WRITE(IUN(23),120)                                 IN 990
        CALL READIN(STORS,NNR,NNC,K,IUN(5),IUN(23),IOP(2))                IN1000
        IF(IOP(2).EQ.1)WRITE(IUN(23),130)                                 IN1010
        CALL READIN(POR,NNR,NNC,K,IUN(6),IUN(23),IOP(2))                  IN1020
        IF(IOP(2).EQ.1)WRITE(IUN(23),140)                                 IN1030
        CALL READIN(THCK,NNR,NNC,K,IUN(7),IUN(23),IOP(2))                 IN1040
        IF(IOP(2).EQ.1)WRITE(IUN(23),150)                                 IN1050
        CALL READIN(ZBOT,NNR,NNC,K,IUN(8),IUN(23),IOP(2))                 IN1060
        IF(IOP(2).EQ.1)WRITE(IUN(23),160)                                 IN1070
        CALL READIN(PHIF,NNR,NNC,K,IUN(9),IUN(23),IOP(2))                 IN1080
        IF(IOP(2).EQ.1)WRITE(IUN(23),170)                                 IN1090
        IF(NINT.EQ.0)CALL READIN(ZINT,NNR,NNC,K,IUN(10),IUN(23),IOP(2))   IN1100
C                                                                         IN1110
        DEL=SGF/(SGS-SGF)                                                 IN1120
        C2=SGF/SGS                                                        IN1130
        C3=1./(1.+DEL)                                                    IN1140
        IF(NINT.GT.0)THEN                                                 IN1141
          DO 161 I=1,NNR                                                  IN1142
          DO 161 J=1,NNC                                                  IN1143
 161        ZINT(I,J,K)=-DEL*PHIF(I,J,K)                                  IN1144
          DO 162 I=1,NNR                                                  IN1145
 162        IF(IOP(2).EQ.1)WRITE(IUN(23),174)I,(ZINT(I,J,K),J=1,NNC)      IN1146
        END IF                                                            IN1147
        DO 171 I=1,NNR                                                    IN1150
        DO 171 J=1,NNC                                                    IN1160
 171      PHIS(I,J,K)=C2*PHIF(I,J,K)+C3*ZINT(I,J,K)                       IN1170
        IF(IOP(2).EQ.1)WRITE(IUN(23),172)                                 IN1180
 172    FORMAT(/' INITIAL SALTWATER HEADS')                               IN1190
        DO 173 I=1,NNR                                                    IN1200
 173    IF(IOP(2).EQ.1)WRITE(IUN(23),174)I,(PHIS(I,J,K),J=1,NNC)          IN1210
 174    FORMAT(I5,3X,10G11.4/(8X,10G11.4))                                IN1220
        IF(NCONT.GT.0)THEN                                                IN1221
          IF(IOP(2).EQ.1)WRITE(IUN(23),181)                               IN1230
 181      FORMAT(/' FX')                                                  IN1240
          CALL READIN(FX,NNR,NNC,K,IUN(11),IUN(23),IOP(2))                IN1250
          IF(IOP(2).EQ.1)WRITE(IUN(23),182)                               IN1260
 182      FORMAT(/' FY')                                                  IN1270
          CALL READIN(FY,NNR,NNC,K,IUN(12),IUN(23),IOP(2))                IN1280
          IF(IOP(2).EQ.1)WRITE(IUN(23),183)                               IN1290
 183      FORMAT(/' SX')                                                  IN1300
          CALL READIN(SX,NNR,NNC,K,IUN(13),IUN(23),IOP(2))                IN1310
          IF(IOP(2).EQ.1)WRITE(IUN(23),184)                               IN1320
 184      FORMAT(/' SY')                                                  IN1330
          CALL READIN(SY,NNR,NNC,K,IUN(14),IUN(23),IOP(2))                IN1340
        END IF                                                            IN1341
        IF(IOP(2).EQ.1)WRITE(IUN(23),180)                                 IN1350
        CALL READIN(AQL,NNR,NNC,K,IUN(15),IUN(23),IOP(2))                 IN1360
  70  CONTINUE                                                            IN1370
C                                                                         IN1380
      IF(IOP(2).EQ.1)WRITE(IUN(23),190)                                   IN1390
      CALL READ1(HEAD,NNR,NNC,IUN(16),IUN(23),IOP(2))                     IN1400
      IF(IOP(2).EQ.1)WRITE(IUN(23),195)                                   IN1410
      CALL READ1(BATH,NNR,NNC,IUN(17),IUN(23),IOP(2))                     IN1420
 195  FORMAT(/' BATHYMETRY')                                              IN1430
      DO 210 I=1,NNR                                                      IN1440
 210    READ(IUN(18),30)(IWT(I,J),J=1,NNC)                                IN1450
      IF(IOP(2).EQ.1)THEN                                                 IN1460
        WRITE(IUN(23),220)                                                IN1470
        DO 230 I=1,NNR                                                    IN1480
 230    WRITE(IUN(23),240)I,(IWT(I,J),J=1,NNC)                            IN1490
      END IF                                                              IN1500
C                                                                         IN1510
      READ(IUN(19),250)(DX(J),J=1,NNC)                                    IN1520
      IF(IOP(2).EQ.1)WRITE(IUN(23),260)(DX(J),J=1,NNC)                    IN1530
      READ(IUN(20),250)(DY(I),I=1,NNR)                                    IN1540
      IF(IOP(2).EQ.1)WRITE(IUN(23),270)(DY(I),I=1,NNR)                    IN1550
C                                                                         IN1560
      RETURN                                                              IN1570
C                                                                         IN1580
C -------------------------------------                                   IN1590
      ENTRY PUMPER(IP)                                                    IN1600
C -------------------------------------                                   IN1610
C                                                                         IN1620
C  READ AND WRITE SPECIFICATIONS FOR NEW PUMPING PERIOD                   IN1630
C                                                                         IN1640
      DO 280 I=1,NNR                                                      IN1650
      DO 280 J=1,NNC                                                      IN1660
        IF(IWT(I,J).EQ.1.AND.BATH(I,J).LT.0.)WRITE(IUN(23),350)I,J        IN1661
      DO 280 K=1,NNL                                                      IN1670
        WTOP(I,J,K)=0.0                                                   IN1680
        WBOT(I,J,K)=0.0                                                   IN1690
        PUMP(I,J,K)=0.0                                                   IN1700
 280  CONTINUE                                                            IN1710
C                                                                         IN1720
      READ(IUN(21),290)DT,TFAC,TMAX,NTSP,NWELL                            IN1730
      WRITE(IUN(23),300)IP,DT,TFAC,TMAX,NTSP,NWELL                        IN1740
      TMAX=TMAX*86400.                                                    IN1750
      IF(DT.GT.TMAX)DT=TMAX                                               IN1760
C                                                                         IN1770
      IF(NWELL.NE.0)THEN                                                  IN1780
        IF(IOP(3).EQ.1)WRITE(IUN(23),310)                                 IN1790
          DO 320 N=1,NWELL                                                IN1800
            READ(IUN(21),330)I,J,K,PUMP(I,J,K),WTOP(I,J,K),WBOT(I,J,K)    IN1810
            IF(IOP(3).EQ.1)WRITE(IUN(23),340)I,J,K,PUMP(I,J,K),           IN1820
     &WTOP(I,J,K),WBOT(I,J,K)                                             IN1830
 320      CONTINUE                                                        IN1840
      END IF                                                              IN1850
      IF(IOP(3).EQ.1)WRITE(IUN(23),200)                                   IN1860
      CALL READ1(RECH,NNR,NNC,IUN(22),IUN(23),IOP(3))                     IN1870
C                                                                         IN1880
      RETURN                                                              IN1890
C                                                                         IN1900
  10  FORMAT(A70)                                                         IN1910
  20  FORMAT(/1X,79('-')/5X,A70/1X,79('-')/)                              IN1920
  30  FORMAT(25I5)                                                        IN1930
  40  FORMAT(' NUMBER OF ROWS=',I4/' NUMBER OF COLUMNS=',I4/' NUMBER OF   IN1940
     1 AQUIFERS=',I4/' NUMBER OF TIME STEPS BETWEEN PRINTOUT=',I4/        IN1950
     2' MAXIMUM NUMBER OF ITERATIONS=',I4/' NUMBER OF ITERATION ',        IN1960
     3'PARAMETERS=',I4/' NUMBER OF PUMPING PERIODS=',I4/                  IN1970
     4' INTERFACE FIXED AFTER ',I4,' ITERATIONS'/' INPUT/OUTPUT OPTIONS   IN1980
     &PARAMETER=',I4,' (0=USE DEFAULTS, 1=READ IN OPTIONS ARRAY)'/        IN1981
     &' LEAKAGE OPTION PARAMETER=',I4,' (0=RESTRICTED MIXING,',           IN1982
     &' 1=COMPLETE MIXING)')                                              IN1990
  41  FORMAT(/' ***WARNING-NO. OF ROWS GREATER THAN ARRAY DIMENSION')     IN1991
  42  FORMAT(/' ***WARNING-NO. OF COLUMNS GREATER THAN ARRAY DIMENSION')  IN1992
  43  FORMAT(/' ***WARNING-NO. OF LAYERS GREATER THAN ARRAY DIMENSION')   IN1993
  50  FORMAT(10G10.0)                                                     IN2000
  60  FORMAT(/' CONVERGENCE CRITERION=',G11.4/' STEADY STATE CRITERION='  IN2010
     1,G11.4/' RELAXATION FACTOR=',G11.4/' WEIGHTING FACTOR=',G11.4/      IN2020
     1' ITERATION PARAMETER FACTOR=',G11.4/                               IN2030
     2' SPECIFIC GRAVITY OF FRESHWATER='                                  IN2040
     2,G11.4/' SPECIFIC GRAVITY OF SALTWATER=',G11.4/                     IN2050
     3' VISCOSITY OF FRESHWATER=',G11.4/' VISCOSITY OF SALTWATER='        IN2060
     4,G11.4/)                                                            IN2070
  80  FORMAT(/1X,31('-')/' AQUIFER PARAMETERS FOR LAYER',I3/1X,31('-'))   IN2080
  90  FORMAT(/' HYDRAULIC CONDUCTIVITY OF FRESHWATER IN X-DIRECTION')     IN2090
 100  FORMAT(/' HYDRAULIC CONDUCTIVITY OF FRESHWATER IN Y-DIRECTION')     IN2100
 110  FORMAT(/' FRESHWATER SPECIFIC STORAGE')                             IN2110
 120  FORMAT(/' SALTWATER SPECIFIC STORAGE')                              IN2120
 130  FORMAT(/' EFFECTIVE POROSITY')                                      IN2130
 140  FORMAT(/' AQUIFER THICKNESS')                                       IN2140
 150  FORMAT(/' ELEVATION OF BASE OF AQUIFER')                            IN2150
 160  FORMAT(/' INITIAL FRESHWATER HEAD')                                 IN2160
 170  FORMAT(/' INITIAL INTERFACE ELEVATION')                             IN2170
 180  FORMAT(/' AQUITARD LEAKANCES (K''/B'')')                            IN2180
 190  FORMAT(/' HEAD IN OVERLYING AQUIFER')                               IN2190
 200  FORMAT(/' RECHARGE')                                                IN2200
 220  FORMAT(/'CODE FOR UPPER LAYER (1-UNCONFINED NODE,',                 IN2210
     2'0-CONFINED NODE)')                                                 IN2220
 240  FORMAT(I5,3X,25I5/(8X,25I5))                                        IN2230
 250  FORMAT(10G10.0)                                                     IN2240
 260  FORMAT(/' DELTA X'//(1H ,10G11.4))                                  IN2250
 270  FORMAT(/' DELTA Y'//(1H ,10G11.4))                                  IN2260
 290  FORMAT(3G10.0,4I5)                                                  IN2270
 300  FORMAT(//1X,79('-')/30X,' PUMPING PERIOD',I3/1X,79('-')//           IN2280
     120X,'INITIAL TIME STEP(SEC)=',G11.4/20X,'MULTIPLICATION FACTOR ',   IN2290
     2'FOR DELTA T=',G11.4/20X,'LENGTH OF PUMPING PERIOD(DAYS)=',G11.4/   IN2300
     320X,'MAXIMUM NUMBER OF TIME STEPS=',I3/                             IN2310
     420X,'NUMBER OF ACTIVE WELLS=',I3)                                   IN2320
 310  FORMAT(/33X,' PUMPING NODES:'/34X,14('-')//19X,'I',4X,'J',          IN2330
     14X,'K',3X,'PUMPAGE',3X,'SCREEN TOP',3X,'SCREEN BOTTOM'/             IN2340
     217X,52('-'))                                                        IN2350
 330  FORMAT(3I5,3G10.4)                                                  IN2360
 340  FORMAT(15X,3I5,G13.4,G12.4,G14.4)                                   IN2370
 350  FORMAT(/' ***WARNING-UNCONFINED BLOCK WITH BATHYMETRY LESS THAN ',  IN2371
     &'ZERO AT:',I3,',',I3)                                               IN2372
      END                                                                 IN2380
C                                                                         IN2390
C ------------------------------------------------------------------------RD  10
      SUBROUTINE READIN(DUM,IN,JN,K,IR,IW,IO)                             RD  20
C ------------------------------------------------------------------------RD  30
C                                                                         RD  40
C  A SUBROUTINE TO READ AND WRITE DATA ARRAYS                             RD  50
C                                                                         RD  60
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 RD  70
      PARAMETER (NR=40,NC=47,NL=5)                                        RD  80
C                                                                         RD  90
      DIMENSION DUM(NR,NC,NL),DUM1(NR,NC)                                 RD 100
C                                                                         RD 130
C  THREE DIMENSIONAL ARRAYS                                               RD 140
      READ(IR,100)M,DUMMY                                                 RD 150
C                                                                         RD 160
      IF(M.EQ.1)THEN                                                      RD 170
        DO 10 I=1,IN                                                      RD 180
        DO 10 J=1,JN                                                      RD 190
          DUM(I,J,K)=DUMMY                                                RD 200
  10    CONTINUE                                                          RD 210
        IF(IO.EQ.1)WRITE(IW,140)DUMMY                                     RD 220
      ELSE                                                                RD 230
        DO 20 I=1,IN                                                      RD 240
          READ(IR,110)(DUM(I,J,K),J=1,JN)                                 RD 250
  20    CONTINUE                                                          RD 260
        DO 30 I=1,IN                                                      RD 270
        DO 30 J=1,JN                                                      RD 280
          DUM(I,J,K)=DUM(I,J,K)*DUMMY                                     RD 290
  30    CONTINUE                                                          RD 300
        IF(IO.EQ.1)THEN                                                   RD 310
          DO 40 I=1,IN                                                    RD 320
            WRITE(IW,130)I,(DUM(I,J,K),J=1,JN)                            RD 330
  40      CONTINUE                                                        RD 340
        END IF                                                            RD 350
      END IF                                                              RD 360
C                                                                         RD 370
      RETURN                                                              RD 380
C                                                                         RD 390
C --------------------------------------                                  RD 400
      ENTRY READ1(DUM1,IN,JN,IR,IW,IO)                                    RD 410
C --------------------------------------                                  RD 420
C                                                                         RD 450
C TWO DIMENSIONAL ARRAYS                                                  RD 460
      READ(IR,100)M,DUMMY                                                 RD 470
C                                                                         RD 480
      IF(M.EQ.1)THEN                                                      RD 490
        DO 60 I=1,IN                                                      RD 500
        DO 60 J=1,JN                                                      RD 510
          DUM1(I,J)=DUMMY                                                 RD 520
  60    CONTINUE                                                          RD 530
        IF(IO.EQ.1)WRITE(IW,140)DUMMY                                     RD 540
      ELSE                                                                RD 550
        DO 70 I=1,IN                                                      RD 560
          READ(IR,110)(DUM1(I,J),J=1,JN)                                  RD 570
  70    CONTINUE                                                          RD 580
        DO 80 I=1,IN                                                      RD 590
        DO 80 J=1,JN                                                      RD 600
          DUM1(I,J)=DUM1(I,J)*DUMMY                                       RD 610
  80    CONTINUE                                                          RD 620
        IF(IO.EQ.1)THEN                                                   RD 630
          DO 90 I=1,IN                                                    RD 640
            WRITE(IW,130)I,(DUM1(I,J),J=1,JN)                             RD 650
  90      CONTINUE                                                        RD 660
        END IF                                                            RD 670
      END IF                                                              RD 680
C                                                                         RD 690
      RETURN                                                              RD 700
C                                                                         RD 710
 100  FORMAT(I5,G10.0)                                                    RD 720
 110  FORMAT(10G10.0)                                                     RD 730
 130  FORMAT(I5,3X,10G11.4/(8X,10G11.4))                                  RD 740
 140  FORMAT(/1H ,G11.4)                                                  RD 750
      END                                                                 RD 760
C                                                                         RD 770
C ------------------------------------------------------------------------IP  10
      SUBROUTINE IPARAM                                                   IP  20
C ------------------------------------------------------------------------IP  30
C                                                                         IP  40
C  A SUBROUTINE TO CALCULATE ITERATION PARAMETERS                         IP  50
C                                                                         IP  60
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 IP  70
      PARAMETER (NR=40,NC=47,NL=5)                                        IP  80
      CHARACTER ICODE,IC,FR,M,S,TITLE*70                                  IP  90
C                                                                         IP 100
      COMMON /INTPAR/ NNR,NNC,NNL,NTSP,NP,NITP,NIT,NPUMP,IPUMP            IP 110
     1,ITMAX,NT,IWT(NR,NC),NFLAG,NCONT,NINT,                              IP 120
     2LW,IW(10),JW(10),KW(10),NUP,NLK                                     IP 130
C                                                                         IP 140
      COMMON /CHAR/ICODE(NR,NC,NL),IC(10,200),FR,M,S                      IP 150
C                                                                         IP 160
      COMMON /VALUES/ DT,TFAC,ERR,RFAC,SGF,SGS,VIF,VIS,DEL,TSEC,TMAX      IP 170
     1,TPUMP,STST,WFAC,WITER                                              IP 180
C                                                                         IP 190
      COMMON /ARRAYS/ FKX(NR,NC,NL),FKY(NR,NC,NL),                        IP 200
     1STORF(NR,NC,NL),STORS(NR,NC,NL),POR(NR,NC,NL),THCK(NR,NC,NL),       IP 210
     2ZBOT(NR,NC,NL),ZINT(NR,NC,NL),AQL(NR,NC,NL),RECH(NR,NC),            IP 220
     3PUMP(NR,NC,NL),PHIF(NR,NC,NL),PHIS(NR,NC,NL),DX(NC),DY(NR),         IP 230
     4WOPTF(10),TTFX(NR,NC,NL),TTFY(NR,NC,NL),TTSX(NR,NC,NL),             IP 240
     5TTSY(NR,NC,NL),DMAXF(200),DMAXS(200),THF(NR,NC,NL),                 IP 250
     6THS(NR,NC,NL),HEAD(NR,NC),PHIFP(NR,NC,NL),PHISP(NR,NC,NL),          IP 260
     7WTOP(NR,NC,NL),WBOT(NR,NC,NL),PUMPF(NR,NC,NL),FAREA(NR,NC,NL),      IP 270
     8SAREA(NR,NC,NL),ZTOP(NR,NC,NL),BATH(NR,NC),                         IP 280
     9FX(NR,NC,NL),FY(NR,NC,NL),SX(NR,NC,NL),SY(NR,NC,NL)                 IP 290
C                                                                         IP 300
      COMMON /IUNITS/IUN(32),IOP(12)                                      IP 310
C                                                                         IP 320
      SNR=DBLE(NNR*NNR)                                                   IP 330
      SNC=DBLE(NNC*NNC)                                                   IP 340
      X1=DBLE(NITP-1)                                                     IP 350
      WF=0.                                                               IP 360
      WS=1.                                                               IP 370
      C1=(VIF*SGS)/(VIS*SGF)                                              IP 380
C                                                                         IP 390
      DO 10 K=1,NNL                                                       IP 400
      DO 10 I=1,NNR                                                       IP 410
      DO 10 J=1,NNC                                                       IP 420
        IF(FKX(I,J,K).EQ.0.)GO TO 10                                      IP 430
        IF(FKY(I,J,K).EQ.0.)WRITE(IUN(23),40)                             IP 431
        DX2=DX(J)**2                                                      IP 440
        DY2=DY(I)**2                                                      IP 450
          RHOF=(FKY(I,J,K)*DX2)/(FKX(I,J,K)*DY2)                          IP 460
          FMAX=DMAX1(SNC*(1.+RHOF),SNR*(1.+1./RHOF))                      IP 470
          IF(FMAX.GT.WF)WF=FMAX                                           IP 480
  10  CONTINUE                                                            IP 490
C                                                                         IP 500
      WF=3.141592654**2/(2.*WF)                                           IP 510
      WOPTF(1)=0.D0                                                       IP 520
      DO 20 I=2,NITP                                                      IP 530
        X2=DBLE(I-1)                                                      IP 540
        WOPTF(I)=1.-(WF*WITER)**(X2/X1)                                   IP 550
  20  CONTINUE                                                            IP 560
C                                                                         IP 570
      WRITE(IUN(23),30)(WOPTF(I),I=1,NITP)                                IP 580
      RETURN                                                              IP 590
C                                                                         IP 600
  30  FORMAT(/' ITERATION PARAMETERS:'/'   FRESHWATER EQUATION:'          IP 610
     &,10G9.4/(24X,10G9.4))                                               IP 620
  40  FORMAT(/' ***WARNING-FKY(',I3,',',I3,',',I3,') =0 AT AN ACTIVE ',   IP 621
     &'BLOCK')                                                            IP 622
      END                                                                 IP 630
C                                                                         IP 640
C ------------------------------------------------------------------------KM  10
      SUBROUTINE KMEAN                                                    KM  20
C ------------------------------------------------------------------------KM  30
C                                                                         KM  40
C  A SUBROUTINE TO CALCULATE MEAN CONDUCTIVITY TERMS AT BLOCK BOUNDARIES  KM  50
C                                                                         KM  60
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 KM  70
      PARAMETER (NR=40,NC=47,NL=5)                                        KM  80
      CHARACTER ICODE,IC,FR,M,S,TITLE*70                                  KM  90
C                                                                         KM 100
      COMMON /INTPAR/ NNR,NNC,NNL,NTSP,NP,NITP,NIT,NPUMP,IPUMP            KM 110
     1,ITMAX,NT,IWT(NR,NC),NFLAG,NCONT,NINT,                              KM 120
     2LW,IW(10),JW(10),KW(10),NUP,NLK                                     KM 130
C                                                                         KM 140
      COMMON /CHAR/ICODE(NR,NC,NL),IC(10,200),FR,M,S                      KM 150
C                                                                         KM 160
      COMMON /VALUES/ DT,TFAC,ERR,RFAC,SGF,SGS,VIF,VIS,DEL,TSEC,TMAX      KM 170
     1,TPUMP,STST,WFAC,WITER                                              KM 180
C                                                                         KM 190
      COMMON /ARRAYS/ FKX(NR,NC,NL),FKY(NR,NC,NL),                        KM 200
     1STORF(NR,NC,NL),STORS(NR,NC,NL),POR(NR,NC,NL),THCK(NR,NC,NL),       KM 210
     2ZBOT(NR,NC,NL),ZINT(NR,NC,NL),AQL(NR,NC,NL),RECH(NR,NC),            KM 220
     3PUMP(NR,NC,NL),PHIF(NR,NC,NL),PHIS(NR,NC,NL),DX(NC),DY(NR),         KM 230
     4WOPTF(10),TTFX(NR,NC,NL),TTFY(NR,NC,NL),TTSX(NR,NC,NL),             KM 240
     5TTSY(NR,NC,NL),DMAXF(200),DMAXS(200),THF(NR,NC,NL),                 KM 250
     6THS(NR,NC,NL),HEAD(NR,NC),PHIFP(NR,NC,NL),PHISP(NR,NC,NL),          KM 260
     7WTOP(NR,NC,NL),WBOT(NR,NC,NL),PUMPF(NR,NC,NL),FAREA(NR,NC,NL),      KM 270
     8SAREA(NR,NC,NL),ZTOP(NR,NC,NL),BATH(NR,NC),                         KM 280
     9FX(NR,NC,NL),FY(NR,NC,NL),SX(NR,NC,NL),SY(NR,NC,NL)                 KM 290
C                                                                         KM 300
      COMMON /COEF/DPHIF(NR,NC,NL),DPHIS(NR,NC,NL),VF(NR,NC,NL),          KM 310
     1VS(NR,NC,NL),E(NR,NC,NL,4),F(NR,NC,NL,4),G(NR,NC,NL,4),A(4),        KM 320
     2B(4),C(4),D(4),AA(4),BB(4),CC(4),DD(4),EE(4),FF(4),GG(4),HH(4),     KM 330
     3SS(4),TT(4),UU(4),WW(4),ZZ(4),BBB(4),ZZZ(4),ARRAY(4),EK1(4),        KM 340
     4EI1(4),FJ1(4),GJ1(4),FK1(4),GI1(4),EIJK(4),FIJK(4),GIJK(4),         KM 350
     5TFX(NR,NC,NL),TFY(NR,NC,NL),TIME(200),FWH(10,200),SWH(10,200),      KM 360
     6ZI(10,200),FAR(10,200),SAR(10,200)                                  KM 370
C                                                                         KM 380
C  CALCULATE HARMONIC MEANS                                               KM 390
      DO 30 K=1,NNL                                                       KM 400
C                                                                         KM 410
C  X DIRECTION                                                            KM 420
        DO 10 I=1,NNR                                                     KM 430
        DO 10 J=1,NNC-1                                                   KM 440
          J2=J+1                                                          KM 450
          TFX(I,J,K)=0.0                                                  KM 460
          IF(FKX(I,J,K).EQ.0.) GO TO 10                                   KM 470
          T1=2*FKX(I,J2,K)*FKX(I,J,K)                                     KM 480
          B1=FKX(I,J2,K)*DX(J)+FKX(I,J,K)*DX(J2)                          KM 490
          TFX(I,J,K)=T1/B1                                                KM 500
  10    CONTINUE                                                          KM 510
C                                                                         KM 520
C  Y DIRECTION                                                            KM 530
        DO 20 I=1,NNR-1                                                   KM 540
        DO 20 J=1,NNC                                                     KM 550
          I2=I+1                                                          KM 560
          TFY(I,J,K)=0.0                                                  KM 570
          IF(FKX(I,J,K).EQ.0.) GO TO 20                                   KM 580
          T1=2*FKY(I2,J,K)*FKY(I,J,K)                                     KM 590
          B1=FKY(I2,J,K)*DY(I)+FKY(I,J,K)*DY(I2)                          KM 600
          TFY(I,J,K)=T1/B1                                                KM 610
  20    CONTINUE                                                          KM 620
  30  CONTINUE                                                            KM 630
C                                                                         KM 640
      RETURN                                                              KM 650
      END                                                                 KM 660
C                                                                         KM 670
C ------------------------------------------------------------------------SI  10
      SUBROUTINE SIP(*)                                                   SI  20
C ------------------------------------------------------------------------SI  30
C                                                                         SI  40
C  A SUBROUTINE TO SOLVE THE FRESHWATER AND SALTWATER FLOW EQUATIONS      SI  50
C    USING THE STRONGLY IMPLICIT PROCEDURE                                SI  60
C                                                                         SI  70
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 SI  80
      PARAMETER (NR=40,NC=47,NL=5)                                        SI  90
      CHARACTER ICODE,IC,FR,M,S,TITLE*70                                  SI 100
C                                                                         SI 110
      COMMON /INTPAR/ NNR,NNC,NNL,NTSP,NP,NITP,NIT,NPUMP,IPUMP            SI 120
     1,ITMAX,NT,IWT(NR,NC),NFLAG,NCONT,NINT,                              SI 130
     2LW,IW(10),JW(10),KW(10),NUP,NLK                                     SI 140
C                                                                         SI 150
      COMMON /CHAR/ICODE(NR,NC,NL),IC(10,200),FR,M,S                      SI 160
C                                                                         SI 170
      COMMON /VALUES/ DT,TFAC,ERR,RFAC,SGF,SGS,VIF,VIS,DEL,TSEC,TMAX      SI 180
     1,TPUMP,STST,WFAC,WITER                                              SI 190
C                                                                         SI 200
      COMMON /ARRAYS/ FKX(NR,NC,NL),FKY(NR,NC,NL),                        SI 210
     1STORF(NR,NC,NL),STORS(NR,NC,NL),POR(NR,NC,NL),THCK(NR,NC,NL),       SI 220
     2ZBOT(NR,NC,NL),ZINT(NR,NC,NL),AQL(NR,NC,NL),RECH(NR,NC),            SI 230
     3PUMP(NR,NC,NL),PHIF(NR,NC,NL),PHIS(NR,NC,NL),DX(NC),DY(NR),         SI 240
     4WOPTF(10),TTFX(NR,NC,NL),TTFY(NR,NC,NL),TTSX(NR,NC,NL),             SI 250
     5TTSY(NR,NC,NL),DMAXF(200),DMAXS(200),THF(NR,NC,NL),                 SI 260
     6THS(NR,NC,NL),HEAD(NR,NC),PHIFP(NR,NC,NL),PHISP(NR,NC,NL),          SI 270
     7WTOP(NR,NC,NL),WBOT(NR,NC,NL),PUMPF(NR,NC,NL),FAREA(NR,NC,NL),      SI 280
     8SAREA(NR,NC,NL),ZTOP(NR,NC,NL),BATH(NR,NC),                         SI 290
     9FX(NR,NC,NL),FY(NR,NC,NL),SX(NR,NC,NL),SY(NR,NC,NL)                 SI 300
C                                                                         SI 310
      COMMON /XNEW/FXN(NR,NC,NL),FYN(NR,NC,NL),SXN(NR,NC,NL),             SI 320
     1SYN(NR,NC,NL)                                                       SI 330
C                                                                         SI 340
      COMMON /COEF/DPHIF(NR,NC,NL),DPHIS(NR,NC,NL),VF(NR,NC,NL),          SI 350
     1VS(NR,NC,NL),E(NR,NC,NL,4),F(NR,NC,NL,4),G(NR,NC,NL,4),A(4),        SI 360
     2B(4),C(4),D(4),AA(4),BB(4),CC(4),DD(4),EE(4),FF(4),GG(4),HH(4),     SI 370
     3SS(4),TT(4),UU(4),WW(4),ZZ(4),BBB(4),ZZZ(4),ARRAY(4),EK1(4),        SI 380
     4EI1(4),FJ1(4),GJ1(4),FK1(4),GI1(4),EIJK(4),FIJK(4),GIJK(4),         SI 390
     5TFX(NR,NC,NL),TFY(NR,NC,NL),TIME(200),FWH(10,200),SWH(10,200),      SI 400
     6ZI(10,200),FAR(10,200),SAR(10,200)                                  SI 410
C                                                                         SI 420
      COMMON /IUNITS/IUN(32),IOP(12)                                      SI 430
C                                                                         SI 440
C  INITIALIZE VARIABLES                                                   SI 450
  5   NIT=0                                                               SI 460
      ITP=0                                                               SI 470
      DO 10 K=1,NNL                                                       SI 480
      DO 10 I=1,NNR                                                       SI 490
      DO 10 J=1,NNC                                                       SI 500
        VF(I,J,K)=0.0                                                     SI 510
        VS(I,J,K)=0.0                                                     SI 520
        DPHIF(I,J,K)=0.0                                                  SI 530
        DPHIS(I,J,K)=0.0                                                  SI 540
      DO 10 L=1,4                                                         SI 550
        E(I,J,K,L)=0.                                                     SI 560
        F(I,J,K,L)=0.                                                     SI 570
        G(I,J,K,L)=0.                                                     SI 580
 10   CONTINUE                                                            SI 590
C                                                                         SI 600
C  BEGIN NEW ITERATION                                                    SI 610
 100  NIT=NIT+1                                                           SI 620
      DFMAX=0.0                                                           SI 630
      DSMAX=0.0                                                           SI 640
C                                                                         SI 650
C  STOP IF MAXIMUM NUMBER OF ITERATIONS IS EXCEEDED                       SI 660
      IF(NIT.GT.ITMAX)THEN                                                SI 670
          WRITE(IUN(24),20)                                               SI 680
          CALL OUTPUT(1)                                                  SI 690
          RETURN 1                                                        SI 700
      END IF                                                              SI 710
C                                                                         SI 720
C  SELECT ITERATION PARAMETER                                             SI 730
      IF(MOD(NIT,NITP))30,30,40                                           SI 740
  30  ITP=0                                                               SI 750
  40  ITP=ITP+1                                                           SI 760
      W=WOPTF(ITP)                                                        SI 770
C                                                                         SI 780
C  UPDATE TRANSMISSIVITY COEFFICIENTS AND INTERFACE TRACKING              SI 790
      IF(NIT.LE.NUP)CALL TCOEF                                            SI 800
C                                                                         SI 810
C  SELECT NORMAL OR REVERSE ALGORITHM                                     SI 820
      IF(MOD(NIT,2))300,300,200                                           SI 830
C                                                                         SI 840
C  NORMAL ALGORITHM                                                       SI 850
 200  IFLAG=0                                                             SI 860
C                                                                         SI 870
C  FACTOR COEFFICIENT MATRIX (A+B)                                        SI 880
      DO 50 K=1,NNL                                                       SI 890
      DO 50 I=2,NNR-1                                                     SI 900
      DO 50 J=2,NNC-1                                                     SI 910
        IF(FKX(I,J,K).EQ.0.)GO TO 50                                      SI 920
        AL=0.0                                                            SI 930
        IF(K.EQ.NNL.AND.IWT(I,J).EQ.1)AL=1.0                              SI 940
        CALL FACTOR(I,J,K,IFLAG,AL,W)                                     SI 950
  50  CONTINUE                                                            SI 960
C                                                                         SI 970
C  BACK SUBSTITUTION FOR CHANGES IN FRESHWATER AND SALTWATER HEADS        SI 980
      DO 60 KK=1,NNL                                                      SI 990
        K=NNL-KK+1                                                        SI1000
      DO 60 II=1,NNR-2                                                    SI1010
        I=NNR-II                                                          SI1020
      DO 60 JJ=1,NNC-2                                                    SI1030
        J=NNC-JJ                                                          SI1040
        I2=I+1                                                            SI1050
        J2=J+1                                                            SI1060
        K2=K+1                                                            SI1070
        DFK2=0.                                                           SI1080
        DSK2=0.                                                           SI1090
        IF(K2.LE.NNL)THEN                                                 SI1100
          DFK2=DPHIF(I,J,K2)                                              SI1110
          DSK2=DPHIS(I,J,K2)                                              SI1120
        END IF                                                            SI1130
        DPHIF(I,J,K)=VF(I,J,K)-(E(I,J,K,1)*DPHIF(I,J2,K)+E(I,J,K,2)*      SI1140
     1DPHIS(I,J2,K)+F(I,J,K,1)*DPHIF(I2,J,K)+F(I,J,K,2)*DPHIS(I2,J,K)     SI1150
     2+G(I,J,K,1)*DFK2+G(I,J,K,2)*DSK2)                                   SI1160
        DPHIS(I,J,K)=VS(I,J,K)-(E(I,J,K,3)*DPHIF(I,J2,K)+E(I,J,K,4)*      SI1170
     1DPHIS(I,J2,K)+F(I,J,K,3)*DPHIF(I2,J,K)+F(I,J,K,4)*DPHIS(I2,J,K)     SI1180
     2+G(I,J,K,3)*DFK2+G(I,J,K,4)*DSK2)                                   SI1190
  60  CONTINUE                                                            SI1200
      GO TO 400                                                           SI1210
C                                                                         SI1220
C REVERSE ALGORITHM                                                       SI1230
 300  IFLAG=1                                                             SI1240
C  FACTOR COEFFICIENT MATRIX A                                            SI1250
      DO 70 KK=1,NNL                                                      SI1260
        K=NNL-KK+1                                                        SI1270
      DO 70 II=1,NNR-2                                                    SI1280
        I=NNR-II                                                          SI1290
      DO 70 J=2,NNC-1                                                     SI1300
        IF(FKX(I,J,K).EQ.0.)GO TO 70                                      SI1310
        AL=0.0                                                            SI1320
        IF(K.EQ.NNL.AND.IWT(I,J).EQ.1)AL=1.0                              SI1330
        CALL FACTOR(I,J,K,IFLAG,AL,W)                                     SI1340
  70  CONTINUE                                                            SI1350
C                                                                         SI1360
C  BACK SUBSTITUTE FOR FRESHWATER AND SALTWATER HEAD CHANGES              SI1370
      DO 80 K=1,NNL                                                       SI1380
      DO 80 I=2,NNR-1                                                     SI1390
      DO 80 JJ=1,NNC-2                                                    SI1400
        J=NNC-JJ                                                          SI1410
        I1=I-1                                                            SI1420
        J2=J+1                                                            SI1430
        K1=K-1                                                            SI1440
        DFK1=0.                                                           SI1450
        DSK1=0.                                                           SI1460
        IF(K1.GE.1)THEN                                                   SI1470
          DFK1=DPHIF(I,J,K1)                                              SI1480
          DSK1=DPHIS(I,J,K1)                                              SI1490
        END IF                                                            SI1500
        DPHIF(I,J,K)=VF(I,J,K)-(E(I,J,K,1)*DPHIF(I,J2,K)+E(I,J,K,2)*      SI1510
     1DPHIS(I,J2,K)+F(I,J,K,1)*DPHIF(I1,J,K)+F(I,J,K,2)*DPHIS(I1,J,K)     SI1520
     2+G(I,J,K,1)*DFK1+G(I,J,K,2)*DSK1)                                   SI1530
        DPHIS(I,J,K)=VS(I,J,K)-(E(I,J,K,3)*DPHIF(I,J2,K)+E(I,J,K,4)*      SI1540
     1DPHIS(I,J2,K)+F(I,J,K,3)*DPHIF(I1,J,K)+F(I,J,K,4)*DPHIS(I1,J,K)     SI1550
     2+G(I,J,K,3)*DFK1+G(I,J,K,4)*DSK1)                                   SI1560
  80  CONTINUE                                                            SI1570
C                                                                         SI1580
 400  CONTINUE                                                            SI1590
C                                                                         SI1600
C  DETERMINE LARGEST FRESHWATER AND SALTWATER HEAD CHANGE OVER            SI1610
C    ITERATION AND UPDATE VALUES                                          SI1620
      DO 90 K=1,NNL                                                       SI1630
      DO 90 I=2,NNR-1                                                     SI1640
      DO 90 J=2,NNC-1                                                     SI1650
        IF(FKX(I,J,K).EQ.0.)GO TO 90                                      SI1660
        FMULT=1-INT(1.-FAREA(I,J,K))                                      SI1670
        SMULT=1-INT(1.-SAREA(I,J,K))                                      SI1680
        APHIF=DABS(FMULT*DPHIF(I,J,K))                                    SI1690
        APHIS=DABS(SMULT*DPHIS(I,J,K))                                    SI1700
        IF(APHIF.GT.DFMAX)THEN                                            SI1710
          DFMAX=APHIF                                                     SI1720
          IF=I                                                            SI1730
          JF=J                                                            SI1740
          KF=K                                                            SI1750
        END IF                                                            SI1760
        IF(APHIS.GT.DSMAX)THEN                                            SI1770
          DSMAX=APHIS                                                     SI1780
          IS=I                                                            SI1790
          JS=J                                                            SI1800
          KS=K                                                            SI1810
        END IF                                                            SI1820
        PHIF(I,J,K)=PHIF(I,J,K)+DPHIF(I,J,K)                              SI1830
        PHIS(I,J,K)=PHIS(I,J,K)+DPHIS(I,J,K)                              SI1840
  90  CONTINUE                                                            SI1850
      DMAXF(NIT)=DFMAX                                                    SI1860
      DMAXS(NIT)=DSMAX                                                    SI1870
C                                                                         SI1871
C  WRITE ITERATION INFORMATION TO SCREEN                                  SI1872
      IF(IOP(11).GT.0)THEN                                                SI1873
      WRITE(1,'((A),I4,(A),I4,(A),G12.4,3I3,A3,(A),G12.4,3I3,A3))')       SI1874
     &  'TS:',NT,' IT:',NIT,' DFMAX:',DPHIF(IF,JF,KF),IF,JF,KF,           SI1875
     &  ICODE(IF,JF,KF),' DSMAX:',DPHIS(IS,JS,KS),IS,JS,KS,               SI1876
     &  ICODE(IS,JS,KS)                                                   SI1877
      END IF                                                              SI1878
C                                                                         SI1880
C  BEGIN NEW ITERATION IF CONVERGENCE HAS NOT BEEN ACHIEVED               SI1890
      IF(DFMAX.GT.ERR.OR.DSMAX.GT.ERR) GO TO 100                          SI1900
C                                                                         SI1910
C  STORE VALUES AT OBSERVATION NODES                                      SI1920
      N=NT+1                                                              SI1930
      TIME(N)=TSEC/31557600.                                              SI1940
      DO 110 L=1,LW                                                       SI1950
        I=IW(L)                                                           SI1960
        J=JW(L)                                                           SI1970
        K=KW(L)                                                           SI1980
        FWH(L,N)=PHIF(I,J,K)                                              SI1990
        SWH(L,N)=PHIS(I,J,K)                                              SI2000
        ZI(L,N)=ZINT(I,J,K)                                               SI2010
        IC(L,N)=ICODE(I,J,K)                                              SI2020
        FAR(L,N)=FAREA(I,J,K)                                             SI2030
        SAR(L,N)=SAREA(I,J,K)                                             SI2040
 110  CONTINUE                                                            SI2050
C                                                                         SI2060
      RETURN                                                              SI2070
C                                                                         SI2080
  20  FORMAT(/1X,37('*')/' MAXIMUM NUMBER OF ITERATIONS EXCEEDED'/        SI2090
     11X,37('*'))                                                         SI2100
      END                                                                 SI2110
C                                                                         SI2120
C ------------------------------------------------------------------------TC  10
      SUBROUTINE TCOEF                                                    TC  20
C ------------------------------------------------------------------------TC  30
C                                                                         TC  40
C  A SUBROUTINE TO CALCULATE FRESHWATER AND SALTWATER TRANSMISSIVITY      TC  50
C    COEFFICIENTS                                                         TC  60
C                                                                         TC  70
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 TC  80
      PARAMETER (NR=40,NC=47,NL=5)                                        TC  90
      CHARACTER ICODE,IC,FR,M,S,TITLE*70                                  TC 100
C                                                                         TC 110
      COMMON /INTPAR/ NNR,NNC,NNL,NTSP,NP,NITP,NIT,NPUMP,IPUMP            TC 120
     1,ITMAX,NT,IWT(NR,NC),NFLAG,NCONT,NINT,                              TC 130
     2LW,IW(10),JW(10),KW(10),NUP,NLK                                     TC 140
C                                                                         TC 150
      COMMON /CHAR/ICODE(NR,NC,NL),IC(10,200),FR,M,S                      TC 160
C                                                                         TC 170
      COMMON /VALUES/ DT,TFAC,ERR,RFAC,SGF,SGS,VIF,VIS,DEL,TSEC,TMAX      TC 180
     1,TPUMP,STST,WFAC,WITER                                              TC 190
C                                                                         TC 200
      COMMON /ARRAYS/ FKX(NR,NC,NL),FKY(NR,NC,NL),                        TC 210
     1STORF(NR,NC,NL),STORS(NR,NC,NL),POR(NR,NC,NL),THCK(NR,NC,NL),       TC 220
     2ZBOT(NR,NC,NL),ZINT(NR,NC,NL),AQL(NR,NC,NL),RECH(NR,NC),            TC 230
     3PUMP(NR,NC,NL),PHIF(NR,NC,NL),PHIS(NR,NC,NL),DX(NC),DY(NR),         TC 240
     4WOPTF(10),TTFX(NR,NC,NL),TTFY(NR,NC,NL),TTSX(NR,NC,NL),             TC 250
     5TTSY(NR,NC,NL),DMAXF(200),DMAXS(200),THF(NR,NC,NL),                 TC 260
     6THS(NR,NC,NL),HEAD(NR,NC),PHIFP(NR,NC,NL),PHISP(NR,NC,NL),          TC 270
     7WTOP(NR,NC,NL),WBOT(NR,NC,NL),PUMPF(NR,NC,NL),FAREA(NR,NC,NL),      TC 280
     8SAREA(NR,NC,NL),ZTOP(NR,NC,NL),BATH(NR,NC),                         TC 290
     9FX(NR,NC,NL),FY(NR,NC,NL),SX(NR,NC,NL),SY(NR,NC,NL)                 TC 300
C                                                                         TC 310
      COMMON /XNEW/FXN(NR,NC,NL),FYN(NR,NC,NL),SXN(NR,NC,NL),             TC 320
     1SYN(NR,NC,NL)                                                       TC 330
C                                                                         TC 340
      COMMON /COEF/DPHIF(NR,NC,NL),DPHIS(NR,NC,NL),VF(NR,NC,NL),          TC 350
     1VS(NR,NC,NL),E(NR,NC,NL,4),F(NR,NC,NL,4),G(NR,NC,NL,4),A(4),        TC 360
     2B(4),C(4),D(4),AA(4),BB(4),CC(4),DD(4),EE(4),FF(4),GG(4),HH(4),     TC 370
     3SS(4),TT(4),UU(4),WW(4),ZZ(4),BBB(4),ZZZ(4),ARRAY(4),EK1(4),        TC 380
     4EI1(4),FJ1(4),GJ1(4),FK1(4),GI1(4),EIJK(4),FIJK(4),GIJK(4),         TC 390
     5TFX(NR,NC,NL),TFY(NR,NC,NL),TIME(200),FWH(10,200),SWH(10,200),      TC 400
     6ZI(10,200),FAR(10,200),SAR(10,200)                                  TC 410
C                                                                         TC 411
      COMMON /IUNITS/IUN(32),IOP(12)                                      TC 412
C                                                                         TC 420
C CALCULATE FRESHWATER AND SALTWATER THICKNESSES                          TC 430
      C1=(VIF*SGS)/(VIS*SGF)                                              TC 440
      C2=SGF/SGS                                                          TC 450
      C3=1./(1.+DEL)                                                      TC 460
      DO 10 K=1,NNL                                                       TC 470
      DO 10 I=1,NNR                                                       TC 480
      DO 10 J=1,NNC                                                       TC 490
        IF(FKX(I,J,K).EQ.0.) GO TO 10                                     TC 500
C                                                                         TC 510
C  CALCULATE NEW INTERFACE ELEVATION                                      TC 520
        ZINT(I,J,K)=(1.+DEL)*PHIS(I,J,K)-DEL*PHIF(I,J,K)                  TC 530
        IF(IWT(I,J).EQ.1.AND.K.EQ.NNL)THEN                                TC 540
          ZTOP(I,J,K)=PHIF(I,J,K)                                         TC 541
          IF(ZTOP(I,J,K).GT.(ZBOT(I,J,K)+THCK(I,J,K)))WRITE(IUN(23),170)  TC 542
     &      I,J,K                                                         TC 543
        END IF                                                            TC 544
        TOP=ZTOP(I,J,K)                                                   TC 550
        THICK=TOP-ZBOT(I,J,K)                                             TC 560
        FAREA(I,J,K)=0.                                                   TC 570
        SAREA(I,J,K)=0.                                                   TC 580
        FXN(I,J,K)=0.                                                     TC 590
        FYN(I,J,K)=0.                                                     TC 600
        SXN(I,J,K)=0.                                                     TC 610
        SYN(I,J,K)=0.                                                     TC 620
C                                                                         TC 630
C  DETERMINE TYPE OF NODE                                                 TC 640
          IF((ZINT(I,J,K)-ZBOT(I,J,K)).LE.(.01*THICK))THEN                TC 650
          ZINT(I,J,K)=ZBOT(I,J,K)                                         TC 660
          ICODE(I,J,K)=FR                                                 TC 670
          FAREA(I,J,K)=1.                                                 TC 680
          FXN(I,J,K)=1.                                                   TC 690
          FYN(I,J,K)=1.                                                   TC 700
         ELSE IF((ZTOP(I,J,K)-ZINT(I,J,K)).LE.(.01*THICK))THEN            TC 710
          ZINT(I,J,K)=TOP                                                 TC 720
          ICODE(I,J,K)=S                                                  TC 730
          SAREA(I,J,K)=1.                                                 TC 740
          SXN(I,J,K)=1.                                                   TC 750
          SYN(I,J,K)=1.                                                   TC 760
        ELSE                                                              TC 770
          ICODE(I,J,K)=M                                                  TC 780
          FAREA(I,J,K)=1.                                                 TC 790
          SAREA(I,J,K)=1.                                                 TC 800
          FXN(I,J,K)=1.                                                   TC 810
          FYN(I,J,K)=1.                                                   TC 820
          SXN(I,J,K)=1.                                                   TC 830
          SYN(I,J,K)=1.                                                   TC 840
        END IF                                                            TC 850
C                                                                         TC 860
C  CALCULATE FRESHWATER AND SALTWATER THICKNESSES                         TC 870
        THS(I,J,K)=ZINT(I,J,K)-ZBOT(I,J,K)                                TC 880
        IF(THS(I,J,K).LT.0.0)THS(I,J,K)=0.0                               TC 890
        THF(I,J,K)=ZTOP(I,J,K)-ZINT(I,J,K)                                TC 900
        IF(THF(I,J,K).LT.0.0)THF(I,J,K)=0.0                               TC 910
C                                                                         TC 920
  10  CONTINUE                                                            TC 930
C                                                                         TC 940
C  CALCULATE TRANSMISSIVITY COEFFICIENTS                                  TC 950
      DO 40 K=1,NNL                                                       TC 960
C                                                                         TC 970
C     X DIRECTION                                                         TC 980
        DO 20 I=1,NNR                                                     TC 990
        DO 20 J=1,NNC-1                                                   TC1000
          IF(FKX(I,J,K).EQ.0.)GO TO 20                                    TC1010
          J2=J+1                                                          TC1020
          FACT=DX(J)/(DX(J2)+DX(J))                                       TC1030
          THIC=THF(I,J,K)+FACT*(THF(I,J2,K)-THF(I,J,K))                   TC1040
          TTFX(I,J,K)=TFX(I,J,K)*THIC                                     TC1050
          THIC=THS(I,J,K)+FACT*(THS(I,J2,K)-THS(I,J,K))                   TC1060
          TTSX(I,J,K)=TFX(I,J,K)*THIC*C1                                  TC1070
  20    CONTINUE                                                          TC1080
C                                                                         TC1090
C     Y DIRECTION                                                         TC1100
        DO 30 I=1,NNR-1                                                   TC1110
        DO 30 J=1,NNC                                                     TC1120
          IF(FKX(I,J,K).EQ.0.)GO TO 30                                    TC1130
          I2=I+1                                                          TC1140
          FACT=DY(I)/(DY(I2)+DY(I))                                       TC1150
          THIC=THF(I,J,K)+FACT*(THF(I2,J,K)-THF(I,J,K))                   TC1160
          TTFY(I,J,K)=TFY(I,J,K)*THIC                                     TC1170
          THIC=THS(I,J,K)+FACT*(THS(I2,J,K)-THS(I,J,K))                   TC1180
          TTSY(I,J,K)=TFY(I,J,K)*THIC*C1                                  TC1190
  30    CONTINUE                                                          TC1200
C                                                                         TC1210
C  INTERFACE TIP TRACKING                                                 TC1220
C    X DIRECTION PROJECTION                                               TC1230
        DO 50 I=2,NNR-1                                                   TC1240
        DO 50 J=2,NNC-1                                                   TC1250
          IF(FKX(I,J,K).EQ.0.)GO TO 50                                    TC1260
          IF(ICODE(I,J,K).EQ.S)GO TO 50                                   TC1270
            JP=J+1                                                        TC1290
            JM=J-1                                                        TC1300
            DX2=DX(J)/2                                                   TC1310
C                                                                         TC1320
C  HEAD DERIVATIVES IN X DIRECTION                                        TC1330
            DXM=0.5*(DX(J)+DX(JM))                                        TC1340
            DXP=0.5*(DX(JP)+DX(J))                                        TC1350
            DSM=(PHIS(I,J,K)-PHIS(I,JM,K))/DXM                            TC1360
            DSP=(PHIS(I,JP,K)-PHIS(I,J,K))/DXP                            TC1370
            DFM=(PHIF(I,J,K)-PHIF(I,JM,K))/DXM                            TC1380
            DFP=(PHIF(I,JP,K)-PHIF(I,J,K))/DXP                            TC1390
C                                                                         TC1400
C  PROJECTION IN POSITIVE X DIRECTION                                     TC1410
          IF(ICODE(I,JP,K).EQ.S)THEN                                      TC1420
            IF(ICODE(I,JM,K).EQ.'.'.OR.ICODE(I,JM,K).EQ.S)THEN            TC1430
              DSM=DSP                                                     TC1440
              DFM=DFP                                                     TC1450
            END IF                                                        TC1460
C                                                                         TC1470
C  INTERFACE DERIVATIVE AND SLOPE OF TOP OF AQUIFER                       TC1480
            XLOLD=0.                                                      TC1490
            IF(FX(I,J,K).GE.1.0)THEN                                      TC1500
              XLOLD=(FX(I,J,K)-0.5)*DX(J)+FX(I,JP,K)*DX(JP)               TC1510
            ELSE IF(FX(I,J,K).GT.0.5)THEN                                 TC1520
              XLOLD=(FX(I,J,K)-0.5)*DX(J)                                 TC1530
            END IF                                                        TC1540
            FP=2.*FX(I,JP,K)                                              TC1550
            IF(FP.GT.1.)FP=1.                                             TC1560
            DZINT=(1.+DEL)*((1.-FP)*DSM+FP*DSP)-DEL*                      TC1570
     1((1.-FP)*DFM+FP*DFP)                                                TC1580
            DZTOP=(ZTOP(I,JP,K)-ZTOP(I,J,K))/DXP                          TC1590
C                                                                         TC1600
C  NEW PROJECTED DISTANCE AND ADJUSTED TRANSMISSIVITY COEFFICIENTS        TC1610
          DEN=DZINT-DZTOP                                                 TC1620
          IF(DEN.NE.0.0)THEN                                              TC1630
            XLNEW=THF(I,J,K)/(DZINT-DZTOP)                                TC1640
            IF(ICODE(I,J,K).EQ.FR)XLNEW=(ZTOP(I,J,K)-                     TC1650
     1((1.+DEL)*PHIS(I,J,K)-DEL*PHIF(I,J,K)))/(DZINT-DZTOP)               TC1660
          ELSE                                                            TC1670
            XLNEW=XLOLD                                                   TC1680
          END IF                                                          TC1690
          AXL=WFAC*DABS(XLNEW)+(1.-WFAC)*XLOLD                            TC1700
          IF(AXL.LE.DX2)THEN                                              TC1710
             FXN(I,J,K)=(AXL+DX2)/DX(J)                                   TC1720
             TTSX(I,J,K)=TTSX(I,J,K)+TTFX(I,J,K)*C1                       TC1730
             TTFX(I,J,K)=0.                                               TC1740
             IF(K.EQ.NNL.AND.IWT(I,J).EQ.1)FXN(I,J,K)=1.                  TC1741
          ELSE                                                            TC1750
             FL=(AXL-DX2)/DX(JP)                                          TC1760
             IF(FL.GT.1.0)FL=1.0                                          TC1770
               FXN(I,JP,K)=FL                                             TC1780
                 TTFXN=THF(I,J,K)*(AXL-DX2)/AXL*TFX(I,J,K)                TC1790
                 TTSX(I,J,K)=TTSX(I,J,K)+(TTFX(I,J,K)-TTFXN)*C1           TC1800
                 TTFX(I,J,K)=TTFXN                                        TC1810
           END IF                                                         TC1820
          END IF                                                          TC1830
C                                                                         TC1840
C  PROJECTION IN THE NEGATIVE X DIRECTION                                 TC1850
          IF(ICODE(I,JM,K).EQ.S)THEN                                      TC1860
            IF(ICODE(I,JP,K).EQ.'.'.OR.ICODE(I,JP,K).EQ.S)THEN            TC1870
              DSP=DSM                                                     TC1880
              DFP=DFM                                                     TC1890
            END IF                                                        TC1900
C                                                                         TC1910
C  INTERFACE DERIVATIVE AND SLOPE OF TOP OF AQUIFER                       TC1920
            XLOLD=0.                                                      TC1930
            IF(FX(I,J,K).GE.1.0)THEN                                      TC1940
              XLOLD=(FX(I,J,K)-0.5)*DX(J)+FX(I,JM,K)*DX(JM)               TC1950
            ELSE IF(FX(I,J,K).GT.0.5)THEN                                 TC1960
              XLOLD=(FX(I,J,K)-0.5)*DX(J)                                 TC1970
            END IF                                                        TC1980
            FM=2.*FX(I,JM,K)                                              TC1990
            IF(FM.GT.1.)FM=1.                                             TC2000
            DZINT=(1.+DEL)*(FM*DSM+(1.-FM)*DSP)-DEL*                      TC2010
     1(FM*DFM+(1.-FM)*DFP)                                                TC2020
            DZTOP=(ZTOP(I,J,K)-ZTOP(I,JM,K))/DXM                          TC2030
C                                                                         TC2040
C  NEW PROJECTED DISTANCE AND ADJUSTED TRANSMISSIVITY COEFFICIENTS        TC2050
          DEN=DZINT-DZTOP                                                 TC2060
          IF(DEN.NE.0.0)THEN                                              TC2070
            XLNEW=THF(I,J,K)/(DZINT-DZTOP)                                TC2080
            IF(ICODE(I,J,K).EQ.FR)XLNEW=(ZTOP(I,J,K)-                     TC2090
     1((1.+DEL)*PHIS(I,J,K)-DEL*PHIF(I,J,K)))/(DZINT-DZTOP)               TC2100
          ELSE                                                            TC2110
            XLNEW=XLOLD                                                   TC2120
          END IF                                                          TC2130
          AXL=WFAC*DABS(XLNEW)+(1.-WFAC)*XLOLD                            TC2140
          IF(AXL.LE.DX2)THEN                                              TC2150
            IF(FXN(I,J,K).LT.1.)THEN                                      TC2160
             FXN(I,J,K)=FXN(I,J,K)+(AXL+DX2)/DX(J)                        TC2170
            ELSE                                                          TC2180
             FXN(I,J,K)=(AXL+DX2)/DX(J)                                   TC2190
            END IF                                                        TC2200
             IF(FXN(I,J,K).GT.1.)FXN(I,J,K)=1.0                           TC2210
             TTSX(I,JM,K)=TTSX(I,JM,K)+TTFX(I,JM,K)*C1                    TC2220
             TTFX(I,JM,K)=0.                                              TC2230
             IF(K.EQ.NNL.AND.IWT(I,J).EQ.1)FXN(I,J,K)=1.                  TC2231
          ELSE                                                            TC2240
             FL=(AXL-DX2)/DX(JM)                                          TC2250
             IF(FL.GT.1.0)FL=1.0                                          TC2260
              IF(FXN(I,J,K).LT.1.)THEN                                    TC2270
               FXN(I,JM,K)=FXN(I,JM,K)+FL                                 TC2280
              ELSE                                                        TC2290
               FXN(I,JM,K)=FL                                             TC2300
              END IF                                                      TC2310
             IF(FXN(I,JM,K).GT.1.)FXN(I,JM,K)=1.0                         TC2320
               TTFXN=THF(I,J,K)*(AXL-DX2)/AXL*TFX(I,JM,K)                 TC2330
               TTSX(I,JM,K)=TTSX(I,JM,K)+(TTFX(I,JM,K)-TTFXN)*C1          TC2340
               TTFX(I,JM,K)=TTFXN                                         TC2350
           END IF                                                         TC2360
          END IF                                                          TC2370
C                                                                         TC2380
C  Y DIRECTION PROJECTION                                                 TC2390
            IP=I+1                                                        TC2400
            IM=I-1                                                        TC2410
            DY2=DY(I)/2.                                                  TC2420
C                                                                         TC2430
C  HEAD DERIVATIVES IN THE Y DIRECTION                                    TC2440
            DYM=0.5*(DY(I)+DY(IM))                                        TC2450
            DYP=0.5*(DY(IP)+DY(I))                                        TC2460
            DSM=(PHIS(I,J,K)-PHIS(IM,J,K))/DYM                            TC2470
            DSP=(PHIS(IP,J,K)-PHIS(I,J,K))/DYP                            TC2480
            DFM=(PHIF(I,J,K)-PHIF(IM,J,K))/DYM                            TC2490
            DFP=(PHIF(IP,J,K)-PHIF(I,J,K))/DYP                            TC2500
C                                                                         TC2510
C  PROJECTION IN THE POSITIVE Y DIRECTION                                 TC2520
          IF(ICODE(IP,J,K).EQ.S)THEN                                      TC2530
            IF(ICODE(IM,J,K).EQ.'.'.OR.ICODE(IM,J,K).EQ.S)THEN            TC2540
              DSM=DSP                                                     TC2550
              DFM=DFP                                                     TC2560
            END IF                                                        TC2570
C                                                                         TC2580
C  INTERFACE DERIVATIVE AND SLOPE OF TOP OF AQUIFER                       TC2590
            YLOLD=0.                                                      TC2600
            IF(FY(I,J,K).GE.1.0)THEN                                      TC2610
              YLOLD=(FY(I,J,K)-0.5)*DY(I)+FY(IP,J,K)*DY(IP)               TC2620
            ELSE IF(FY(I,J,K).GT.0.5)THEN                                 TC2630
              YLOLD=(FY(I,J,K)-0.5)*DY(I)                                 TC2640
            END IF                                                        TC2650
            FP=2.*FY(IP,J,K)                                              TC2660
            IF(FP.GT.1.)FP=1.                                             TC2670
            DZINT=(1.+DEL)*((1.-FP)*DSM+FP*DSP)-DEL*                      TC2680
     1((1.-FP)*DFM+FP*DFP)                                                TC2690
            DZTOP=(ZTOP(IP,J,K)-ZTOP(I,J,K))/DYP                          TC2700
C                                                                         TC2710
C  NEW PROJECTED DISTANCE AND ADJUSTED TRANSMISSIVITY COEFFICIENTS        TC2720
          DEN=DZINT-DZTOP                                                 TC2730
          IF(DEN.NE.0.0)THEN                                              TC2740
            YLNEW=THF(I,J,K)/(DZINT-DZTOP)                                TC2750
            IF(ICODE(I,J,K).EQ.FR)YLNEW=(ZTOP(I,J,K)-                     TC2760
     1((1.+DEL)*PHIS(I,J,K)-DEL*PHIF(I,J,K)))/(DZINT-DZTOP)               TC2770
          ELSE                                                            TC2780
            YLNEW=YLOLD                                                   TC2790
          END IF                                                          TC2800
          AYL=WFAC*DABS(YLNEW)+(1.-WFAC)*YLOLD                            TC2810
          IF(AYL.LE.DY2)THEN                                              TC2820
            FL=(AYL+DY2)/DY(I)                                            TC2830
            FYN(I,J,K)=FL                                                 TC2840
            TTSY(I,J,K)=TTSY(I,J,K)+TTFY(I,J,K)*C1                        TC2850
            TTFY(I,J,K)=0.0                                               TC2860
             IF(K.EQ.NNL.AND.IWT(I,J).EQ.1)FYN(I,J,K)=1.                  TC2861
          ELSE                                                            TC2870
            FL=(AYL-DY2)/DY(IP)                                           TC2880
            IF(FL.GT.1.0)FL=1.0                                           TC2890
            FYN(IP,J,K)=FL                                                TC2900
            TTFYN=THF(I,J,K)*(AYL-DY2)/AYL*TFY(I,J,K)                     TC2910
            TTSY(I,J,K)=TTSY(I,J,K)+(TTFY(I,J,K)-TTFYN)*C1                TC2920
            TTFY(I,J,K)=TTFYN                                             TC2930
          END IF                                                          TC2940
          END IF                                                          TC2950
C                                                                         TC2960
C  PROJECTION IN THE NEGATIVE Y DIRECTION                                 TC2970
          IF(ICODE(IM,J,K).EQ.S)THEN                                      TC2980
            IF(ICODE(IP,J,K).EQ.'.'.OR.ICODE(IP,J,K).EQ.S)THEN            TC2990
              DSP=DSM                                                     TC3000
              DFP=DFM                                                     TC3010
            END IF                                                        TC3020
C                                                                         TC3030
C  INTERFACE DERIVATIVE AND SLOPE OF TOP OF AQUIFER                       TC3040
            YLOLD=0.                                                      TC3050
            IF(FY(I,J,K).GE.1.0)THEN                                      TC3060
              YLOLD=(FY(I,J,K)-0.5)*DY(I)+FY(IM,J,K)*DY(IM)               TC3070
            ELSE IF(FY(I,J,K).GT.0.5)THEN                                 TC3080
              YLOLD=(FY(I,J,K)-0.5)*DY(I)                                 TC3090
            END IF                                                        TC3100
            FM=2.*FY(IM,J,K)                                              TC3110
            IF(FM.GT.1.)FM=1.                                             TC3120
            DZINT=(1.+DEL)*(FM*DSM+(1.-FM)*DSP)-DEL*                      TC3130
     1(FM*DFM+(1.-FM)*DFP)                                                TC3140
            DZTOP=(ZTOP(I,J,K)-ZTOP(IM,J,K))/DYM                          TC3150
C                                                                         TC3160
C  NEW PROJECTED DISTANCE AND ADJUSTED TRANSMISSIVITY COEFFICIENTS        TC3170
          DEN=DZINT-DZTOP                                                 TC3180
          IF(DEN.NE.0.0)THEN                                              TC3190
            YLNEW=THF(I,J,K)/(DZINT-DZTOP)                                TC3200
            IF(ICODE(I,J,K).EQ.FR)YLNEW=(ZTOP(I,J,K)-                     TC3210
     1((1.+DEL)*PHIS(I,J,K)-DEL*PHIF(I,J,K)))/(DZINT-DZTOP)               TC3220
          ELSE                                                            TC3230
            YLNEW=YLOLD                                                   TC3240
          END IF                                                          TC3250
          AYL=WFAC*DABS(YLNEW)+(1.-WFAC)*YLOLD                            TC3260
          IF(AYL.LE.DY2)THEN                                              TC3270
            FL=(AYL+DY2)/DY(I)                                            TC3280
           IF(FYN(I,J,K).LT.1.)THEN                                       TC3290
            FYN(I,J,K)=FYN(I,J,K)+FL                                      TC3300
           ELSE                                                           TC3310
            FYN(I,J,K)=FL                                                 TC3320
           END IF                                                         TC3330
            IF(FYN(I,J,K).GT.1.)FYN(I,J,K)=1.0                            TC3340
            TTSY(IM,J,K)=TTSY(IM,J,K)+TTFY(IM,J,K)*C1                     TC3350
            TTFY(IM,J,K)=0.0                                              TC3360
             IF(K.EQ.NNL.AND.IWT(I,J).EQ.1)FYN(I,J,K)=1.                  TC3361
          ELSE                                                            TC3370
            FL=(AYL-DY2)/DY(IM)                                           TC3380
            IF(FL.GT.1.0)FL=1.0                                           TC3390
           IF(FYN(IM,J,K).LT.1.)THEN                                      TC3400
            FYN(IM,J,K)=FYN(IM,J,K)+FL                                    TC3410
           ELSE                                                           TC3420
            FYN(IM,J,K)=FL                                                TC3430
           END IF                                                         TC3440
            IF(FYN(IM,J,K).GT.1.)FYN(IM,J,K)=1.0                          TC3450
            TTFYN=THF(I,J,K)*(AYL-DY2)/AYL*TFY(IM,J,K)                    TC3460
            TTSY(IM,J,K)=TTSY(IM,J,K)+(TTFY(IM,J,K)-TTFYN)*C1             TC3470
            TTFY(IM,J,K)=TTFYN                                            TC3480
          END IF                                                          TC3490
          END IF                                                          TC3500
  50    CONTINUE                                                          TC3510
C                                                                         TC3520
C  INTERFACE TOE TRACKING                                                 TC3530
C    X DIRECTION  PROJECTION                                              TC3540
        DO 80 I=2,NNR-1                                                   TC3550
        DO 80 J=2,NNC-1                                                   TC3560
          IF(FKX(I,J,K).EQ.0.)GO TO 80                                    TC3570
          IF(ICODE(I,J,K).EQ.FR)GO TO 80                                  TC3580
            JP=J+1                                                        TC3590
            JM=J-1                                                        TC3600
            DX2=DX(J)/2.                                                  TC3610
C                                                                         TC3620
C  HEAD DERIVATIVES IN X DIRECTION                                        TC3630
            DXM=0.5*(DX(J)+DX(JM))                                        TC3640
            DXP=0.5*(DX(JP)+DX(J))                                        TC3650
            DSM=(PHIS(I,J,K)-PHIS(I,JM,K))/DXM                            TC3660
            DSP=(PHIS(I,JP,K)-PHIS(I,J,K))/DXP                            TC3670
            DFM=(PHIF(I,J,K)-PHIF(I,JM,K))/DXM                            TC3680
            DFP=(PHIF(I,JP,K)-PHIF(I,J,K))/DXP                            TC3690
C                                                                         TC3700
C  PROJECTION IN POSITIVE X DIRECTION                                     TC3710
          IF(ICODE(I,JP,K).EQ.FR)THEN                                     TC3720
            IF(ICODE(I,JM,K).EQ.'.'.OR.ICODE(I,JM,K).EQ.FR)THEN           TC3730
              DSM=DSP                                                     TC3740
              DFM=DFP                                                     TC3750
            END IF                                                        TC3760
C                                                                         TC3770
C  INTERFACE DERIVATIVE AND SLOPE OF BOTTOM OF AQUIFER                    TC3780
            XLOLD=0.                                                      TC3790
            IF(SX(I,J,K).GE.1.0)THEN                                      TC3800
              XLOLD=(SX(I,J,K)-0.5)*DX(J)+SX(I,JP,K)*DX(JP)               TC3810
            ELSE IF(SX(I,J,K).GT.0.5)THEN                                 TC3820
              XLOLD=(SX(I,J,K)-0.5)*DX(J)                                 TC3830
            END IF                                                        TC3840
            SP=XLOLD/DXP                                                  TC3850
            IF(SP.GT.1.)SP=1.                                             TC3860
            DZINT=(1.+DEL)*((1.-SP)*DSM+SP*DSP)-DEL*                      TC3870
     1((1.-SP)*DFM+SP*DFP)                                                TC3880
            DZBOT=(ZBOT(I,JP,K)-ZBOT(I,J,K))/DXP                          TC3890
C                                                                         TC3900
C  NEW PROJECTED DISTANCE AND ADJUSTED TRANSMISSIVITY COEFFICIENTS        TC3910
          DEN=DZBOT-DZINT                                                 TC3920
          IF(DEN.NE.0.0)THEN                                              TC3930
            XLNEW=THS(I,J,K)/(DZBOT-DZINT)                                TC3940
            IF(ICODE(I,J,K).EQ.S)XLNEW=(((1.+DEL)*PHIS(I,J,K)-            TC3950
     1DEL*PHIF(I,J,K))-ZBOT(I,J,K))/(DZBOT-DZINT)                         TC3960
          ELSE                                                            TC3970
            XLNEW=XLOLD                                                   TC3980
          END IF                                                          TC3990
          AXL=WFAC*DABS(XLNEW)+(1.-WFAC)*XLOLD                            TC4000
          IF(AXL.LE.DX2)THEN                                              TC4010
             SXN(I,J,K)=(AXL+DX2)/DX(J)                                   TC4020
             IF (ICODE(I,J,K).NE.S) THEN                                  TC4030
               TTFX(I,J,K)=TTFX(I,J,K)+TTSX(I,J,K)/C1                     TC4040
               TTSX(I,J,K)=0.                                             TC4050
             ENDIF                                                        TC4060
          ELSE                                                            TC4070
             SL=(AXL-DX2)/DX(JP)                                          TC4080
             IF(SL.GT.1.0)SL=1.0                                          TC4090
               SXN(I,JP,K)=SL                                             TC4100
               IF (ICODE(I,J,K).NE.S) THEN                                TC4110
                 TTSXN=THS(I,J,K)*(AXL-DX2)/AXL*TFX(I,J,K)*C1             TC4120
                 TTFX(I,J,K)=TTFX(I,J,K)+(TTSX(I,J,K)-TTSXN)/C1           TC4130
                 TTSX(I,J,K)=TTSXN                                        TC4140
               ENDIF                                                      TC4150
           END IF                                                         TC4160
          END IF                                                          TC4170
C                                                                         TC4180
C  PROJECTION IN NEGATIVE X DIRECTION                                     TC4190
          IF(ICODE(I,JM,K).EQ.FR)THEN                                     TC4200
            IF(ICODE(I,JP,K).EQ.'.'.OR.ICODE(I,JP,K).EQ.FR)THEN           TC4210
              DSP=DSM                                                     TC4220
              DFP=DFM                                                     TC4230
            END IF                                                        TC4240
C                                                                         TC4250
C  INTERFACE DERIVATIVE AND SLOPE OF BOTTOM OF AQUIFER                    TC4260
            XLOLD=0.                                                      TC4270
            IF(SX(I,J,K).GE.1.0)THEN                                      TC4280
              XLOLD=(SX(I,J,K)-0.5)*DX(J)+SX(I,JM,K)*DX(JM)               TC4290
            ELSE IF(SX(I,J,K).GT.0.5)THEN                                 TC4300
              XLOLD=(SX(I,J,K)-0.5)*DX(J)                                 TC4310
            END IF                                                        TC4320
           SM=XLOLD/DXM                                                   TC4330
            IF(SM.GT.1.)SM=1.                                             TC4350
            DZINT=(1.+DEL)*(SM*DSM+(1.-SM)*DSP)-DEL*                      TC4360
     1(SM*DFM+(1.-SM)*DFP)                                                TC4370
            DZBOT=(ZBOT(I,J,K)-ZBOT(I,JM,K))/DXM                          TC4380
C                                                                         TC4390
C  NEW PROJECTED DISTANCE AND ADJUSTED TRANSMISSIVITY COEFFICIENTS        TC4400
          DEN=DZBOT-DZINT                                                 TC4410
          IF(DEN.NE.0.0)THEN                                              TC4420
            XLNEW=THS(I,J,K)/(DZBOT-DZINT)                                TC4430
            IF(ICODE(I,J,K).EQ.S)XLNEW=(((1.+DEL)*PHIS(I,J,K)-            TC4440
     1DEL*PHIF(I,J,K))-ZBOT(I,J,K))/(DZBOT-DZINT)                         TC4450
          ELSE                                                            TC4460
            XLNEW=XLOLD                                                   TC4470
          END IF                                                          TC4480
          AXL=WFAC*DABS(XLNEW)+(1.-WFAC)*XLOLD                            TC4490
          IF(AXL.LE.DX2)THEN                                              TC4500
            IF(SXN(I,J,K).LT.1.)THEN                                      TC4510
             SXN(I,J,K)=SXN(I,J,K)+(AXL+DX2)/DX(J)                        TC4520
            ELSE                                                          TC4530
             SXN(I,J,K)=(AXL+DX2)/DX(J)                                   TC4540
            END IF                                                        TC4550
            IF(SXN(I,J,K).GT.1.)SXN(I,J,K)=1.                             TC4560
             IF(ICODE(I,J,K).NE.S) THEN                                   TC4570
               TTFX(I,JM,K)=TTFX(I,JM,K)+TTSX(I,JM,K)/C1                  TC4580
               TTSX(I,JM,K)=0.                                            TC4590
             ENDIF                                                        TC4600
          ELSE                                                            TC4610
             SL=(AXL-DX2)/DX(JM)                                          TC4620
             IF(SL.GT.1.0)SL=1.0                                          TC4630
              IF(SXN(I,JM,K).LT.1.)THEN                                   TC4640
               SXN(I,JM,K)=SXN(I,JM,K)+SL                                 TC4650
              ELSE                                                        TC4660
               SXN(I,JM,K)=SL                                             TC4670
              END IF                                                      TC4680
              IF(SXN(I,JM,K).GT.1.)SXN(I,JM,K)=1.0                        TC4690
               IF(ICODE(I,J,K).EQ.S) THEN                                 TC4700
                 FACT=DX(JM)/(DX(J)+DX(JM))                               TC4710
                 THIC=THCK(I,JM,K)+FACT*(THCK(I,J,K)-THCK(I,JM,K))        TC4720
               ELSE                                                       TC4730
                 TTSXN=THS(I,J,K)*(AXL-DX2)/AXL*TFX(I,JM,K)*C1            TC4740
                 TTFX(I,JM,K)=TTFX(I,JM,K)+(TTSX(I,JM,K)-TTSXN)/C1        TC4750
                 TTSX(I,JM,K)=TTSXN                                       TC4760
               END IF                                                     TC4770
           END IF                                                         TC4780
          END IF                                                          TC4790
C                                                                         TC4800
C  Y DIRECTION PROJECTION                                                 TC4810
            IP=I+1                                                        TC4820
            IM=I-1                                                        TC4830
            DY2=DY(I)/2.                                                  TC4840
C                                                                         TC4850
C  HEAD DERIVATIVES IN Y DIRECTION                                        TC4860
            DYM=0.5*(DY(I)+DY(IM))                                        TC4870
            DYP=0.5*(DY(IP)+DY(I))                                        TC4880
            DSM=(PHIS(I,J,K)-PHIS(IM,J,K))/DYM                            TC4890
            DSP=(PHIS(IP,J,K)-PHIS(I,J,K))/DYP                            TC4900
            DFM=(PHIF(I,J,K)-PHIF(IM,J,K))/DYM                            TC4910
            DFP=(PHIF(IP,J,K)-PHIF(I,J,K))/DYP                            TC4920
C                                                                         TC4930
C  PROJECTION IN POSITIVE Y DIRECTION                                     TC4940
          IF(ICODE(IP,J,K).EQ.FR)THEN                                     TC4950
            IF(ICODE(IM,J,K).EQ.'.'.OR.ICODE(IM,J,K).EQ.FR)THEN           TC4960
              DSM=DSP                                                     TC4970
              DFM=DFP                                                     TC4980
            END IF                                                        TC4990
C                                                                         TC5000
C  INTERFACE DERIVATIVE AND SLOPE OF BASE OF AQUIFER                      TC5010
            YLOLD=0.                                                      TC5020
            IF(SY(I,J,K).GE.1.0)THEN                                      TC5030
              YLOLD=(SY(I,J,K)-0.5)*DY(I)+SY(IP,J,K)*DY(IP)               TC5040
            ELSE IF(SX(I,J,K).GT.0.5)THEN                                 TC5050
              YLOLD=(SY(I,J,K)-0.5)*DY(I)                                 TC5060
            END IF                                                        TC5070
           SP=YLOLD/DYP                                                   TC5080
            IF(SP.GT.1.)SP=1.                                             TC5090
            DZINT=(1.+DEL)*((1.-SP)*DSM+SP*DSP)-DEL*                      TC5100
     1((1.-SP)*DFM+SP*DFP)                                                TC5110
            DZBOT=(ZBOT(IP,J,K)-ZBOT(I,J,K))/DYP                          TC5120
C                                                                         TC5130
C  NEW PROJECTED DISTANCE AND ADJUSTED TRANSMISSIVITY COEFFICIENTS        TC5140
          DEN=DZBOT-DZINT                                                 TC5150
          IF(DEN.NE.0.0)THEN                                              TC5160
            YLNEW=THS(I,J,K)/(DZBOT-DZINT)                                TC5170
            IF(ICODE(I,J,K).EQ.S)YLNEW=(((1.+DEL)*PHIS(I,J,K)-            TC5180
     1DEL*PHIF(I,J,K))-ZBOT(I,J,K))/(DZBOT-DZINT)                         TC5190
          ELSE                                                            TC5200
            YLNEW=YLOLD                                                   TC5210
          END IF                                                          TC5220
          AYL=WFAC*DABS(YLNEW)+(1.-WFAC)*YLOLD                            TC5230
          IF(AYL.LE.DY2)THEN                                              TC5240
            SL=(AYL+DY2)/DY(I)                                            TC5250
            SYN(I,J,K)=SL                                                 TC5260
            IF (ICODE(I,J,K).NE.S) THEN                                   TC5270
              TTFY(I,J,K)=TTFY(I,J,K)+TTSY(I,J,K)/C1                      TC5280
              TTSY(I,J,K)=0.0                                             TC5290
            ENDIF                                                         TC5300
          ELSE                                                            TC5310
            SL=(AYL-DY2)/DY(IP)                                           TC5320
            IF(SL.GT.1.0)SL=1.0                                           TC5330
            SYN(IP,J,K)=SL                                                TC5340
            IF (ICODE(I,J,K).NE.S) THEN                                   TC5350
              TTSYN=THS(I,J,K)*(AYL-DY2)/AYL*TFY(I,J,K)*C1                TC5360
              TTFY(I,J,K)=TTFY(I,J,K)+(TTSY(I,J,K)-TTSYN)/C1              TC5370
              TTSY(I,J,K)=TTSYN                                           TC5380
            ENDIF                                                         TC5390
          END IF                                                          TC5400
          END IF                                                          TC5410
C                                                                         TC5420
C  PROJECTION IN NEGATIVE Y DIRECTION                                     TC5430
          IF(ICODE(IM,J,K).EQ.FR)THEN                                     TC5440
            IF(ICODE(IP,J,K).EQ.'.'.OR.ICODE(IP,J,K).EQ.FR)THEN           TC5450
              DSP=DSM                                                     TC5460
              DFP=DFM                                                     TC5470
            END IF                                                        TC5480
C                                                                         TC5490
C  INTERFACE DERIVATIVE AND SLOPE OF BASE OF AQUIFER                      TC5500
            YLOLD=0.                                                      TC5510
            IF(SY(I,J,K).GE.1.0)THEN                                      TC5520
              YLOLD=(SY(I,J,K)-0.5)*DY(I)+SY(IM,J,K)*DY(IM)               TC5530
            ELSE IF(SY(I,J,K).GT.0.5)THEN                                 TC5540
              YLOLD=(SY(I,J,K)-0.5)*DY(I)                                 TC5550
            END IF                                                        TC5560
            SM=YLOLD/DYM                                                  TC5570
            IF(SM.GT.1.)SM=1.                                             TC5580
            DZINT=(1.+DEL)*(SM*DSM+(1.-SM)*DSP)-DEL*                      TC5590
     1(SM*DFM+(1.-SM)*DFP)                                                TC5600
            DZBOT=(ZBOT(I,J,K)-ZBOT(IM,J,K))/DYM                          TC5610
C                                                                         TC5620
C  NEW PROJECTED DISTANCE AND ADJUSTED TRANSMISSIVITY COEFFICIENTS        TC5630
          DEN=DZBOT-DZINT                                                 TC5640
          IF(DEN.NE.0.0)THEN                                              TC5650
            YLNEW=THS(I,J,K)/(DZBOT-DZINT)                                TC5660
            IF(ICODE(I,J,K).EQ.S)YLNEW=(((1.+DEL)*PHIS(I,J,K)-            TC5670
     1DEL*PHIF(I,J,K))-ZBOT(I,J,K))/(DZBOT-DZINT)                         TC5680
          ELSE                                                            TC5690
            YLNEW=YLOLD                                                   TC5700
          END IF                                                          TC5710
          AYL=WFAC*DABS(YLNEW)+(1.-WFAC)*YLOLD                            TC5720
          IF(AYL.LE.DY2)THEN                                              TC5730
            SL=(AYL+DY2)/DY(I)                                            TC5740
           IF(SYN(I,J,K).LT.1.)THEN                                       TC5750
            SYN(I,J,K)=SYN(I,J,K)+SL                                      TC5780
           ELSE                                                           TC5790
            SYN(I,J,K)=SL                                                 TC5800
           END IF                                                         TC5810
           IF(SYN(I,J,K).GT.1.)SYN(I,J,K)=1.                              TC5820
            IF (ICODE(I,J,K).NE.S) THEN                                   TC5830
              TTFY(IM,J,K)=TTFY(IM,J,K)+TTSY(IM,J,K)/C1                   TC5840
              TTSY(IM,J,K)=0.0                                            TC5850
            ENDIF                                                         TC5860
          ELSE                                                            TC5870
            SL=(AYL-DY2)/DY(IM)                                           TC5880
            IF(SL.GT.1.0)SL=1.0                                           TC5890
           IF(SYN(IM,J,K).LT.1.)THEN                                      TC5900
            SYN(IM,J,K)=SYN(IM,J,K)+SL                                    TC5910
           ELSE                                                           TC5920
            SYN(IM,J,K)=SL                                                TC5930
           END IF                                                         TC5940
           IF(SYN(IM,J,K).GT.1.)SYN(IM,J,K)=1.0                           TC5950
            IF (ICODE(I,J,K).NE.S) THEN                                   TC5960
              TTSYN=THS(I,J,K)*(AYL-DY2)/AYL*TFY(IM,J,K)*C1               TC5970
              TTFY(IM,J,K)=TTFY(IM,J,K)+(TTSY(IM,J,K)-TTSYN)/C1           TC5980
              TTSY(IM,J,K)=TTSYN                                          TC5990
            ENDIF                                                         TC6000
          END IF                                                          TC6010
          END IF                                                          TC6020
  80    CONTINUE                                                          TC6030
C                                                                         TC6040
C  CALCULATE NEW FRESHWATER AND SALTWATER AREA FACTORS                    TC6050
        DO 100 I=2,NNR-1                                                  TC6060
        DO 100 J=2,NNC-1                                                  TC6070
          IF(FKX(I,J,K).EQ.0.)GO TO 100                                   TC6080
          IF(FXN(I,J,K).EQ.0.0.OR.FXN(I,J,K).EQ.1.0)THEN                  TC6090
            FAREA(I,J,K)=FYN(I,J,K)                                       TC6100
          ELSE IF(FYN(I,J,K).EQ.0.0.OR.FYN(I,J,K).EQ.1.0)THEN             TC6110
            FAREA(I,J,K)=FXN(I,J,K)                                       TC6120
          ELSE IF(FXN(I,J,K).LE.0.5)THEN                                  TC6130
            FAREA(I,J,K)=(1.-DSQRT(FXN(I,J,K)*FYN(I,J,K)))*               TC6140
     1(FXN(I,J,K)+FYN(I,J,K))                                             TC6150
          ELSE IF(FYN(I,J,K).LE.0.5)THEN                                  TC6160
            FAREA(I,J,K)=(1.-DSQRT(FXN(I,J,K)*FYN(I,J,K)))*               TC6170
     1(FXN(I,J,K)+FYN(I,J,K))                                             TC6180
          ELSE                                                            TC6190
            FL=FXN(I,J,K)*FYN(I,J,K)                                      TC6200
            FAREA(I,J,K)=FL*(1.+(1./((FL+.75)**7)))                       TC6210
            IF(FAREA(I,J,K).GT.1.0)FAREA(I,J,K)=1.0                       TC6220
          END IF                                                          TC6230
          IF(SXN(I,J,K).EQ.0.0.OR.SXN(I,J,K).EQ.1.0)THEN                  TC6240
            SAREA(I,J,K)=SYN(I,J,K)                                       TC6250
          ELSE IF(SYN(I,J,K).EQ.0.0.OR.SYN(I,J,K).EQ.1.0)THEN             TC6260
            SAREA(I,J,K)=SXN(I,J,K)                                       TC6270
          ELSE IF(SXN(I,J,K).LE.0.5)THEN                                  TC6280
            SAREA(I,J,K)=(1.-DSQRT(SXN(I,J,K)*SYN(I,J,K)))*               TC6290
     1(SXN(I,J,K)+SYN(I,J,K))                                             TC6300
          ELSE IF(SYN(I,J,K).LE.0.5)THEN                                  TC6310
            SAREA(I,J,K)=(1.-DSQRT(SXN(I,J,K)*SYN(I,J,K)))*               TC6320
     1(SXN(I,J,K)+SYN(I,J,K))                                             TC6330
          ELSE                                                            TC6340
            SL=SXN(I,J,K)*SYN(I,J,K)                                      TC6350
            SAREA(I,J,K)=SL*(1.+(1./((SL+.75)**7)))                       TC6360
            IF(SAREA(I,J,K).GT.1.0)SAREA(I,J,K)=1.0                       TC6370
          END IF                                                          TC6380
          FX(I,J,K)=FXN(I,J,K)                                            TC6390
          FY(I,J,K)=FYN(I,J,K)                                            TC6400
          SX(I,J,K)=SXN(I,J,K)                                            TC6410
          SY(I,J,K)=SYN(I,J,K)                                            TC6420
 100    CONTINUE                                                          TC6430
C                                                                         TC6440
  40  CONTINUE                                                            TC6450
C                                                                         TC6460
 170  FORMAT(/' ***WARNING: WATER TABLE ABOVE LAND SURFACE AT BLOCK:',    TC6461
     &I3,2X,I3,2X,I3)                                                     TC6462
C                                                                         TC6463
      RETURN                                                              TC6470
      END                                                                 TC6480
C                                                                         TC6490
C ------------------------------------------------------------------------FA  10
      SUBROUTINE FACTOR(I,J,K,IFLAG,AL,W)                                 FA  20
C ------------------------------------------------------------------------FA  30
C                                                                         FA  40
C  A SUBROUTINE TO CALCULATE COEFFICIENTS OF MATRIX (A) AND FACTOR        FA  50
C    MODIFIED COEFFICIENT MATRIX (A+B)                                    FA  60
C                                                                         FA  70
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 FA  80
      PARAMETER (NR=40,NC=47,NL=5)                                        FA  90
      CHARACTER ICODE,IC,FR,M,S,TITLE*70                                  FA 100
C                                                                         FA 110
      COMMON /INTPAR/ NNR,NNC,NNL,NTSP,NP,NITP,NIT,NPUMP,IPUMP            FA 120
     1,ITMAX,NT,IWT(NR,NC),NFLAG,NCONT,NINT,                              FA 130
     2LW,IW(10),JW(10),KW(10),NUP,NLK                                     FA 140
C                                                                         FA 150
      COMMON /CHAR/ICODE(NR,NC,NL),IC(10,200),FR,M,S                      FA 160
C                                                                         FA 170
      COMMON /VALUES/ DT,TFAC,ERR,RFAC,SGF,SGS,VIF,VIS,DEL,TSEC,TMAX      FA 180
     1,TPUMP,STST,WFAC,WITER                                              FA 190
C                                                                         FA 200
      COMMON /ARRAYS/ FKX(NR,NC,NL),FKY(NR,NC,NL),                        FA 210
     1STORF(NR,NC,NL),STORS(NR,NC,NL),POR(NR,NC,NL),THCK(NR,NC,NL),       FA 220
     2ZBOT(NR,NC,NL),ZINT(NR,NC,NL),AQL(NR,NC,NL),RECH(NR,NC),            FA 230
     3PUMP(NR,NC,NL),PHIF(NR,NC,NL),PHIS(NR,NC,NL),DX(NC),DY(NR),         FA 240
     4WOPTF(10),TTFX(NR,NC,NL),TTFY(NR,NC,NL),TTSX(NR,NC,NL),             FA 250
     5TTSY(NR,NC,NL),DMAXF(200),DMAXS(200),THF(NR,NC,NL),                 FA 260
     6THS(NR,NC,NL),HEAD(NR,NC),PHIFP(NR,NC,NL),PHISP(NR,NC,NL),          FA 270
     7WTOP(NR,NC,NL),WBOT(NR,NC,NL),PUMPF(NR,NC,NL),FAREA(NR,NC,NL),      FA 280
     8SAREA(NR,NC,NL),ZTOP(NR,NC,NL),BATH(NR,NC),                         FA 290
     9FX(NR,NC,NL),FY(NR,NC,NL),SX(NR,NC,NL),SY(NR,NC,NL)                 FA 300
C                                                                         FA 310
      COMMON /COEF/DPHIF(NR,NC,NL),DPHIS(NR,NC,NL),VF(NR,NC,NL),          FA 320
     1VS(NR,NC,NL),E(NR,NC,NL,4),F(NR,NC,NL,4),G(NR,NC,NL,4),A(4),        FA 330
     2B(4),C(4),D(4),AA(4),BB(4),CC(4),DD(4),EE(4),FF(4),GG(4),HH(4),     FA 340
     3SS(4),TT(4),UU(4),WW(4),ZZ(4),BBB(4),ZZZ(4),ARRAY(4),EK1(4),        FA 350
     4EI1(4),FJ1(4),GJ1(4),FK1(4),GI1(4),EIJK(4),FIJK(4),GIJK(4),         FA 360
     5TFX(NR,NC,NL),TFY(NR,NC,NL),TIME(200),FWH(10,200),SWH(10,200),      FA 370
     6ZI(10,200),FAR(10,200),SAR(10,200)                                  FA 380
C                                                                         FA 390
      COMMON /IUNITS/IUN(32),IOP(12)                                      FA 400
C                                                                         FA 410
      DIMENSION G1(4)                                                     FA 420
C                                                                         FA 430
      I1=I-1                                                              FA 440
      I2=I+1                                                              FA 450
      J1=J-1                                                              FA 460
      J2=J+1                                                              FA 470
      K1=K-1                                                              FA 480
      K2=K+1                                                              FA 490
      AREA=DX(J)*DY(I)                                                    FA 500
C                                                                         FA 510
      DO 5 L=1,4                                                          FA 520
        BB(L)=0.                                                          FA 530
        DD(L)=0.                                                          FA 540
        EE(L)=0.                                                          FA 550
        FF(L)=0.                                                          FA 560
        HH(L)=0.                                                          FA 570
        SS(L)=0.                                                          FA 580
        ZZ(L)=0.                                                          FA 590
        EK1(L)=0.                                                         FA 600
        FK1(L)=0.                                                         FA 610
        A(L)=0.0                                                          FA 611
   5  CONTINUE                                                            FA 620
C                                                                         FA 630
C  ASSIGN VALUES OF TRANSMISSIVITY COEFFICIENTS                           FA 640
        BB(1)=TTFY(I1,J,K)*DX(J)                                          FA 650
        DD(1)=TTFX(I,J1,K)*DY(I)                                          FA 660
        FF(1)=TTFX(I,J,K)*DY(I)                                           FA 670
        HH(1)=TTFY(I,J,K)*DX(J)                                           FA 680
        BB(4)=TTSY(I1,J,K)*DX(J)                                          FA 690
        DD(4)=TTSX(I,J1,K)*DY(I)                                          FA 700
        FF(4)=TTSX(I,J,K)*DY(I)                                           FA 710
        HH(4)=TTSY(I,J,K)*DX(J)                                           FA 720
C                                                                         FA 730
C  CALCULATE LEAKAGE COEFFICIENTS                                         FA 740
      AQLTOP=(1.-AL)*AQL(I,J,K)*AREA                                      FA 750
      AQLBOT=0.0                                                          FA 760
      SGR=SGS/SGF                                                         FA 770
      IF(K.NE.1)AQLBOT=AQL(I,J,K1)*AREA                                   FA 780
      QF2=0.                                                              FA 790
      QS2=0.                                                              FA 800
      QF1=0.                                                              FA 810
      QS1=0.                                                              FA 820
      QFL=0.                                                              FA 830
      QSL=0.                                                              FA 840
      ZZ1=0.                                                              FA 850
      E5=0.                                                               FA 860
      WT1=1.0                                                             FA 870
      WT2=1.0                                                             FA 880
      WT3=0.0                                                             FA 890
      RT3=1.0                                                             FA 900
      WT4=1.0                                                             FA 910
      WT5=1.0                                                             FA 911
        IF(NLK.EQ.0)THEN                                                  FA 920
          WT2=0.0                                                         FA 960
          WT3=FAREA(I,J,K)                                                FA 970
          WT4=0.0                                                         FA1010
          IF(K.NE.NNL)THEN                                                FA 921
            DIRP=(SGR*PHIS(I,J,K+1)-PHIF(I,J,K))-(ZBOT(I,J,K2)+           FA 930
     &ZTOP(I,J,K))/(2.*DEL)                                               FA 940
            IF(DIRP.GT.0.0)WT1=0.0                                        FA 950
            IF(ICODE(I,J,K2).EQ.'.')THEN                                  FA 955
              WT1=1.0                                                     FA 960
              WT2=1.0                                                     FA 970
              IF(PHIF(I,J,K2).GT.PHIF(I,J,K).AND.BATH(I,J).LT.0.)WT5=0.0  FA 980
            END IF                                                        FA 990
          END IF                                                          FA1000
          IF(K.NE.1)THEN                                                  FA1010
            DIRM=(PHIF(I,J,K1)-SGR*PHIS(I,J,K))+(ZBOT(I,J,K)+             FA1020
     &ZTOP(I,J,K1))/(2.*DEL)                                              FA1030
            IF(DIRM.LT.0.0)RT3=0.0                                        FA1040
          END IF                                                          FA1050
        END IF                                                            FA1060
      IF(K.EQ.NNL)THEN                                                    FA1070
        PHTOPF=(1.-AL)*HEAD(I,J)                                          FA1080
        PHTOPS=(1.-AL)*(HEAD(I,J)/SGR+BATH(I,J)/(DEL+1.))                 FA1090
        QF1=FAREA(I,J,K)*AQLTOP*PHTOPF                                    FA1100
        QS1=(1.-FAREA(I,J,K))*AQLTOP*PHTOPS                               FA1110
        QF2=FAREA(I,J,K)*AQLTOP                                           FA1120
        QS2=(1.-FAREA(I,J,K))*AQLTOP                                      FA1130
        IF(NLK.EQ.0)THEN                                                  FA1131
          IF(HEAD(I,J).GT.PHIF(I,J,K).AND.BATH(I,J).LT.0.)THEN            FA1132
            QF1=0.0                                                       FA1133
            QF2=0.0                                                       FA1134
          END IF                                                          FA1135
        END IF                                                            FA1136
C                                                                         FA1140
       ELSE                                                               FA1150
         IF(FAREA(I,J,K).GE.(1.-SAREA(I,J,K2)))THEN                       FA1160
           SS(1)=(1.-SAREA(I,J,K2))*AQLTOP                                FA1170
           SS(2)=(FAREA(I,J,K)-(1.-SAREA(I,J,K2)))*AQLTOP*SGR*WT1         FA1180
           SS(4)=(1.-FAREA(I,J,K))*AQLTOP                                 FA1190
           QFL=-SS(2)/SGR*(ZBOT(I,J,K2)+ZTOP(I,J,K))/(2.*DEL)             FA1200
         ELSE                                                             FA1210
           SS(1)=FAREA(I,J,K)*AQLTOP*WT5                                  FA1220
           SS(3)=((1.-FAREA(I,J,K))-SAREA(I,J,K2))*AQLTOP*                FA1230
     &WT2                                                                 FA1240
           SS(4)=SAREA(I,J,K2)*AQLTOP                                     FA1250
           QSL=SS(3)*(ZBOT(I,J,K2)+ZTOP(I,J,K))/(2.*DEL)                  FA1260
         END IF                                                           FA1270
       END IF                                                             FA1280
       IF(K.NE.1)THEN                                                     FA1290
         IF(FAREA(I,J,K1).GT.(1.-SAREA(I,J,K)))THEN                       FA1300
           ZZ1=RT3*WT3*(FAREA(I,J,K1)-(1.-SAREA(I,J,K)))*AQLBOT           FA1310
           E5=ZZ1*SGR                                                     FA1320
           ZZ(1)=(1.-SAREA(I,J,K))*AQLBOT+ZZ1                             FA1330
           ZZ(3)=(FAREA(I,J,K1)-(1.-SAREA(I,J,K)))*AQLBOT*(1.-            FA1340
     &WT3)*RT3                                                            FA1350
           ZZ(4)=(1.-FAREA(I,J,K1))*AQLBOT                                FA1360
           QFL=QFL+ZZ1*(ZBOT(I,J,K)+ZTOP(I,J,K1))/(2.*DEL)                FA1370
           QSL=QSL+ZZ(3)*(ZBOT(I,J,K)+ZTOP(I,J,K1))/(2.*DEL)              FA1380
         ELSE                                                             FA1390
           ZZ(1)=FAREA(I,J,K1)*AQLBOT                                     FA1400
           ZZ(2)=WT4*((1.-FAREA(I,J,K1))-SAREA(I,J,K))*AQLBOT*SGR         FA1410
           ZZ(4)=SAREA(I,J,K)*AQLBOT                                      FA1420
           QFL=QFL-ZZ(2)/SGR*(ZBOT(I,J,K)+ZTOP(I,J,K1))/(2.*DEL)          FA1430
        END IF                                                            FA1440
       END IF                                                             FA1450
C                                                                         FA1460
       E2=SS(1)+SS(2)/SGR+(ZZ(1)-ZZ1)+ZZ(2)/SGR+QF2                       FA1470
       E4=SS(3)*SGR+SS(4)+ZZ(3)*SGR+ZZ(4)+QS2                             FA1480
C                                                                         FA1490
C  CALCULATE REMAINING COEFFICIENTS                                       FA1500
       APROP=FAREA(I,J,K)+SAREA(I,J,K)-1.                                 FA1510
       ARDT=AREA/DT                                                       FA1520
       IF(APROP.GT.0.)THEN                                                FA1530
         E1=(POR(I,J,K)*DEL*APROP+(POR(I,J,K)*AL+STORF(I,J,K)*            FA1540
     1THF(I,J,K))*FAREA(I,J,K))*ARDT                                      FA1550
         EE(2)=POR(I,J,K)*(1.+DEL)*APROP*ARDT                             FA1560
         EE(3)=POR(I,J,K)*DEL*APROP*ARDT                                  FA1570
         E3=(POR(I,J,K)*(1.+DEL)*APROP+STORS(I,J,K)*THS(I,J,K)*           FA1580
     1SAREA(I,J,K))*ARDT                                                  FA1590
       ELSE                                                               FA1600
         E1=SAREA(I,J,K)+(STORF(I,J,K)*THF(I,J,K)+AL*POR(I,J,K))*         FA1610
     1FAREA(I,J,K)*ARDT                                                   FA1620
         EE(2)=(1.-FAREA(I,J,K))*SGR                                      FA1630
         EE(3)=(1.-SAREA(I,J,K))/SGR                                      FA1640
         E3=FAREA(I,J,K)+STORS(I,J,K)*THS(I,J,K)*SAREA(I,J,K)*ARDT        FA1650
       END IF                                                             FA1660
C                                                                         FA1670
       EE(1)=-(BB(1)+DD(1)+FF(1)+HH(1)+E1+E2)                             FA1680
       EE(2)=EE(2)-E5                                                     FA1690
       EE(4)=-(BB(4)+DD(4)+FF(4)+HH(4)+E3+E4)                             FA1700
C                                                                         FA1710
C  CALCULATE FRESHWATER AND SALTWATER PUMPAGES                            FA1720
      PUMPF(I,J,K)=0.0                                                    FA1720
      PUMPS=0.0                                                           FA1730
      IF(PUMP(I,J,K).EQ.0.) GO TO 10                                      FA1740
        TOP=WTOP(I,J,K)                                                   FA1750
        BOT=WBOT(I,J,K)                                                   FA1760
        IF(WTOP(I,J,K).GT.ZTOP(I,J,K))TOP=ZTOP(I,J,K)                     FA1770
        IF(TOP.LE.WBOT(I,J,K))THEN                                        FA1780
          PUMP(I,J,K)=0.                                                  FA1790
          WRITE(IUN(24),20)I,J,K                                          FA1800
          GO TO 10                                                        FA1810
        END IF                                                            FA1820
C                                                                         FA1830
        IF(TOP.GT.ZINT(I,J,K))THEN                                        FA1840
          IF(BOT.LT.ZINT(I,J,K))THEN                                      FA1850
            THICF=TOP-ZINT(I,J,K)                                         FA1860
            THICS=ZINT(I,J,K)-BOT                                         FA1870
          ELSE                                                            FA1880
            THICF=TOP-BOT                                                 FA1890
            THICS=0.0                                                     FA1900
          END IF                                                          FA1910
        ELSE                                                              FA1920
          THICF=0.0                                                       FA1930
          THICS=TOP-BOT                                                   FA1940
        END IF                                                            FA1950
C                                                                         FA1960
        THICK=THICF+THICS                                                 FA1970
        PUMPF(I,J,K)=THICF*PUMP(I,J,K)/THICK                              FA1980
        PUMPS=PUMP(I,J,K)-PUMPF(I,J,K)                                    FA1990
C                                                                         FA2000
C  ZERO COEFFICIENTS AT CONSTANT HEAD NODES                               FA2010
  10    IF(STORF(I,J,K).LT.0.)THEN                                        FA2020
          BB(1)=0.                                                        FA2030
          DD(1)=0.                                                        FA2040
          FF(1)=0.                                                        FA2050
          HH(1)=0.                                                        FA2060
          SS(1)=0.                                                        FA2070
          SS(2)=0.                                                        FA2080
          ZZ(1)=0.                                                        FA2090
          ZZ(2)=0.                                                        FA2100
          QF1=0.                                                          FA2110
          QF2=0.                                                          FA2120
          EE(1)=1.0E+06                                                   FA2130
          EE(2)=0.                                                        FA2140
          QFL=0.                                                          FA2150
          E5=0.                                                           FA2160
        END IF                                                            FA2170
        IF(STORS(I,J,K).LT.0.)THEN                                        FA2180
          BB(4)=0.                                                        FA2190
          DD(4)=0.                                                        FA2200
          FF(4)=0.                                                        FA2210
          HH(4)=0.                                                        FA2220
          SS(3)=0.                                                        FA2230
          SS(4)=0.                                                        FA2240
          ZZ(3)=0.                                                        FA2250
          ZZ(4)=0.                                                        FA2260
          QS1=0.                                                          FA2270
          QS2=0.                                                          FA2280
          EE(3)=0.                                                        FA2290
          EE(4)=1.0E+06                                                   FA2300
          QSL=0.                                                          FA2310
        END IF                                                            FA2320
C                                                                         FA2330
C  CALCULATE FRESHWATER AND SALTWATER RESIDUALS                           FA2340
        QF=(EE(2)+E5)*PHISP(I,J,K)-E1*PHIFP(I,J,K)                        FA2350
     1-AL*RECH(I,J)*AREA+PUMPF(I,J,K)-QF1-QFL                             FA2360
        Z1=0.                                                             FA2370
        S1=0.                                                             FA2380
        IF(K.NE.1)Z1=ZZ(1)*PHIF(I,J,K1)+ZZ(2)*PHIS(I,J,K1)                FA2390
        IF(K.NE.NNL)S1=SS(1)*PHIF(I,J,K2)+SS(2)*PHIS(I,J,K2)              FA2400
        RESF=QF-(BB(1)*PHIF(I1,J,K)+DD(1)*PHIF(I,J1,K)+EE(1)*PHIF(I,J,K)  FA2410
     1+EE(2)*PHIS(I,J,K)+FF(1)*PHIF(I,J2,K)+HH(1)*PHIF(I2,J,K)+Z1+S1)     FA2420
        IF(FAREA(I,J,K).LE.0.)RESF=0.                                     FA2430
        QS=EE(3)*PHIFP(I,J,K)-E3*PHISP(I,J,K)                             FA2440
     1+PUMPS-QS1-QSL                                                      FA2450
        Z2=0.                                                             FA2460
        S2=0.                                                             FA2470
        IF(K.NE.1)Z2=ZZ(3)*PHIF(I,J,K1)+ZZ(4)*PHIS(I,J,K1)                FA2480
        IF(K.NE.NNL)S2=SS(3)*PHIF(I,J,K2)+SS(4)*PHIS(I,J,K2)              FA2490
        RESS=QS-(BB(4)*PHIS(I1,J,K)+DD(4)*PHIS(I,J1,K)+EE(3)*PHIF(I,J,K)  FA2500
     1+EE(4)*PHIS(I,J,K)+FF(4)*PHIS(I,J2,K)+HH(4)*PHIS(I2,J,K)+Z2+S2)     FA2510
        IF(SAREA(I,J,K).LE.0.)RESS=0.                                     FA2520
        IF(STORF(I,J,K).LT.0.)THEN                                        FA2530
          RESF=0.                                                         FA2540
        END IF                                                            FA2550
        IF(STORS(I,J,K).LT.0.)THEN                                        FA2560
          RESS=0.                                                         FA2570
        END IF                                                            FA2580
C                                                                         FA2590
C  PICK COEFFICIENTS FOR NORMAL OR REVERSE FACTORIZATION                  FA2600
      IF(IFLAG.EQ.0)THEN                                                  FA2610
        II=I1                                                             FA2620
        KK=K1                                                             FA2630
      ELSE                                                                FA2640
        DO 7 L=1,4                                                        FA2650
          BBB(L)=HH(L)                                                    FA2660
          HH(L)=BB(L)                                                     FA2670
          BB(L)=BBB(L)                                                    FA2680
          ZZZ(L)=SS(L)                                                    FA2690
          SS(L)=ZZ(L)                                                     FA2700
          ZZ(L)=ZZZ(L)                                                    FA2710
   7    CONTINUE                                                          FA2720
        II=I2                                                             FA2730
        KK=K2                                                             FA2740
      ENDIF                                                               FA2750
C                                                                         FA2760
C  CALCULATION OF MODIFIED COEFFICIENT MATRIX (A+B) AND FACTORIZATION     FA2770
C                                                                         FA2780
      IF(KK.GE.1.AND.KK.LE.NNL)THEN                                       FA2790
        CALL ASSIGN(E,EK1,I,J,KK)                                         FA2800
        CALL ASSIGN(F,FK1,I,J,KK)                                         FA2810
        CALL MAT1(EK1,FK1,ARRAY,W)                                        FA2820
        CALL INV(ARRAY,ARRAY)                                             FA2830
        CALL MULT(ZZ,ARRAY,A)                                             FA2840
      END IF                                                              FA2850
C                                                                         FA2860
      CALL ASSIGN(E,EI1,II,J,K)                                           FA2870
      CALL ASSIGN(G,GI1,II,J,K)                                           FA2880
      CALL MAT1(EI1,GI1,ARRAY,W)                                          FA2890
      CALL INV(ARRAY,ARRAY)                                               FA2900
      CALL MULT(BB,ARRAY,B)                                               FA2910
C                                                                         FA2920
      CALL ASSIGN(F,FJ1,I,J1,K)                                           FA2930
      CALL ASSIGN(G,GJ1,I,J1,K)                                           FA2940
      CALL MAT1(FJ1,GJ1,ARRAY,W)                                          FA2950
      CALL INV(ARRAY,ARRAY)                                               FA2960
      CALL MULT(DD,ARRAY,C)                                               FA2970
C                                                                         FA2980
      CALL MULT(A,EK1,AA)                                                 FA2990
      CALL MULT(B,EI1,CC)                                                 FA3000
      CALL MULT(C,FJ1,GG)                                                 FA3010
      CALL MULT(C,GJ1,WW)                                                 FA3020
      CALL MULT(A,FK1,TT)                                                 FA3030
      CALL MULT(B,GI1,UU)                                                 FA3040
C                                                                         FA3050
      DO 100 L=1,4                                                        FA3060
        D(L)=EE(L)+W*(CC(L)+GG(L)+AA(L)+WW(L)+TT(L)+UU(L))                FA3070
 100  CONTINUE                                                            FA3080
C                                                                         FA3090
      IF(KK.LT.1.OR.KK.GT.NNL)THEN                                        FA3100
        DO 150 L=1,4                                                      FA3110
          G1(L)=0.                                                        FA3120
          A(L)=0.                                                         FA3130
 150    CONTINUE                                                          FA3140
        VF1=0.                                                            FA3150
        VS1=0.                                                            FA3160
      ELSE                                                                FA3170
        DO 160 L=1,4                                                      FA3180
          G1(L)=G(I,J,KK,L)                                               FA3190
 160    CONTINUE                                                          FA3200
        VF1=VF(I,J,KK)                                                    FA3210
        VS1=VS(I,J,KK)                                                    FA3220
      END IF                                                              FA3230
C                                                                         FA3240
      D(1)=D(1)-(A(1)*G1(1)+A(2)*G1(3)+B(1)*F(II,J,K,1)                   FA3250
     1+B(2)*F(II,J,K,3)+C(1)*E(I,J1,K,1)+C(2)*E(I,J1,K,3))                FA3260
      D(2)=D(2)-(A(1)*G1(2)+A(2)*G1(4)+B(1)*F(II,J,K,2)                   FA3270
     1+B(2)*F(II,J,K,4)+C(1)*E(I,J1,K,2)+C(2)*E(I,J1,K,4))                FA3280
      D(3)=D(3)-(A(3)*G1(1)+A(4)*G1(3)+B(3)*F(II,J,K,1)                   FA3290
     1+B(4)*F(II,J,K,3)+C(3)*E(I,J1,K,1)+C(4)*E(I,J1,K,3))                FA3300
      D(4)=D(4)-(A(3)*G1(2)+A(4)*G1(4)+B(3)*F(II,J,K,2)                   FA3310
     1+B(4)*F(II,J,K,4)+C(3)*E(I,J1,K,2)+C(4)*E(I,J1,K,4))                FA3320
C                                                                         FA3330
      CALL MAT2(FF,CC,AA,ARRAY,W)                                         FA3340
      CALL INV(D,D)                                                       FA3350
      CALL MULT(D,ARRAY,EIJK)                                             FA3360
C                                                                         FA3370
      CALL MAT2(HH,GG,TT,ARRAY,W)                                         FA3380
      CALL MULT(D,ARRAY,FIJK)                                             FA3390
C                                                                         FA3400
      CALL MAT2(SS,WW,UU,ARRAY,W)                                         FA3410
      CALL MULT(D,ARRAY,GIJK)                                             FA3420
C                                                                         FA3430
      DO 200 L=1,4                                                        FA3440
        E(I,J,K,L)=EIJK(L)                                                FA3450
        F(I,J,K,L)=FIJK(L)                                                FA3460
        G(I,J,K,L)=GIJK(L)                                                FA3470
 200  CONTINUE                                                            FA3480
C                                                                         FA3490
C  FORWARD SUBSTITUTION FOR VECTOR (V)                                    FA3500
      XF=RFAC*RESF-(A(1)*VF1+A(2)*VS1+B(1)*VF(II,J,K)                     FA3510
     1+B(2)*VS(II,J,K)+C(1)*VF(I,J1,K)+C(2)*VS(I,J1,K))                   FA3520
      XS=RFAC*RESS-(A(3)*VF1+A(4)*VS1+B(3)*VF(II,J,K)                     FA3530
     1+B(4)*VS(II,J,K)+C(3)*VF(I,J1,K)+C(4)*VS(I,J1,K))                   FA3540
C                                                                         FA3550
      VF(I,J,K)=D(1)*XF+D(2)*XS                                           FA3560
      VS(I,J,K)=D(3)*XF+D(4)*XS                                           FA3570
C                                                                         FA3580
      RETURN                                                              FA3590
C                                                                         FA3600
  20  FORMAT(' WELL(',I2,',',I2,',',I2,') GOES DRY')                      FA3610
      END                                                                 FA3620
C                                                                         FA3630
C------------------------------------------------------------------------ MU  10
      SUBROUTINE MULT(A,B,C)                                              MU  20
C------------------------------------------------------------------------ MU  30
C                                                                         MU  40
C  A SUBROUTINE TO MULTIPLY MATRIX A TIMES MATRIX B                       MU  50
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 MU  60
      DIMENSION A(4),B(4),C(4)                                            MU  70
C                                                                         MU  80
      C(1)=A(1)*B(1)+A(2)*B(3)                                            MU  90
      C(2)=A(1)*B(2)+A(2)*B(4)                                            MU 100
      C(3)=A(3)*B(1)+A(4)*B(3)                                            MU 110
      C(4)=A(3)*B(2)+A(4)*B(4)                                            MU 120
C                                                                         MU 130
      RETURN                                                              MU 140
      END                                                                 MU 150
C                                                                         MU 160
C-------------------------------------------------------------------------IV  10
      SUBROUTINE INV(A,AINV)                                              IV  20
C-------------------------------------------------------------------------IV  30
C                                                                         IV  40
C  A SUBROUTINE TO TAKE THE INVERSE OF MATRIX A                           IV  50
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 IV  60
      DIMENSION A(4),AINV(4)                                              IV  70
C                                                                         IV  80
      DET=A(1)*A(4)-A(2)*A(3)                                             IV  90
      A1=A(1)                                                             IV 100
      AINV(1)=A(4)/DET                                                    IV 110
      AINV(2)=-A(2)/DET                                                   IV 120
      AINV(3)=-A(3)/DET                                                   IV 130
      AINV(4)=A1/DET                                                      IV 140
C                                                                         IV 150
      RETURN                                                              IV 160
      END                                                                 IV 170
C                                                                         IV 180
C-------------------------------------------------------------------------MT  10
      SUBROUTINE MAT1(A,B,C,W)                                            MT  20
C-------------------------------------------------------------------------MT  30
C                                                                         MT  40
C  A SUBROUTINE TO DO MATRIX CALCULATIONS USED IN FACTORIZATION           MT  50
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 MT  60
      DIMENSION A(4),B(4),C(4)                                            MT  70
C                                                                         MT  80
      DO 10 I=1,4,3                                                       MT  90
        C(I)=1.+W*(A(I)+B(I))                                             MT 100
  10  CONTINUE                                                            MT 110
      DO 20 I=2,3                                                         MT 120
        C(I)=W*(A(I)+B(I))                                                MT 130
  20  CONTINUE                                                            MT 140
C                                                                         MT 150
      RETURN                                                              MT 160
      END                                                                 MT 170
C                                                                         MT 180
C-------------------------------------------------------------------------MR  10
      SUBROUTINE MAT2(A,B,C,D,W)                                          MR  20
C-------------------------------------------------------------------------MR  30
C                                                                         MR  40
C  A SUBROUTINE TO DO MATRIX CALCULATIONS USED IN FACTORIZATION           MR  50
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 MR  60
      DIMENSION A(4),B(4),C(4),D(4)                                       MR  70
C                                                                         MR  80
      DO 10 I=1,4                                                         MR  90
        D(I)=A(I)-W*(B(I)+C(I))                                           MR 100
  10  CONTINUE                                                            MR 110
C                                                                         MR 120
      RETURN                                                              MR 130
      END                                                                 MR 140
C                                                                         MR 150
C-------------------------------------------------------------------------AS  10
      SUBROUTINE ASSIGN(A,B,I,J,K)                                        AS  20
C-------------------------------------------------------------------------AS  30
C                                                                         AS  40
C  A SUBROUTINE TO ASSIGN MATRIX ELEMENTS                                 AS  50
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 AS  60
      PARAMETER (NR=40,NC=47,NL=5)                                        AS  70
      DIMENSION A(NR,NC,NL,4),B(4)                                        AS  80
C                                                                         AS  90
      DO 10 L=1,4                                                         AS 100
        B(L)=A(I,J,K,L)                                                   AS 110
  10  CONTINUE                                                            AS 120
C                                                                         AS 130
      RETURN                                                              AS 140
      END                                                                 AS 150
C                                                                         AS 160
C------------------------------------------------------------------------ RE  10
      SUBROUTINE RESULTS                                                  RE  20
C------------------------------------------------------------------------ RE  30
C                                                                         RE  40
C  A SUBROUTINE TO WRITE TIME STEP RESULTS TO FILE                        RE  50
C                                                                         RE  60
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 RE  70
      PARAMETER (NR=40,NC=47,NL=5)                                        RE  80
      CHARACTER ICODE,IC,FR,M,S,TITLE*70                                  RE  90
C                                                                         RE 100
      COMMON /INTPAR/ NNR,NNC,NNL,NTSP,NP,NITP,NIT,NPUMP,IPUMP            RE 110
     1,ITMAX,NT,IWT(NR,NC),NFLAG,NCONT,NINT,                              RE 120
     2LW,IW(10),JW(10),KW(10),NUP,NLK                                     RE 130
C                                                                         RE 140
      COMMON /CHAR/ICODE(NR,NC,NL),IC(10,200),FR,M,S                      RE 150
C                                                                         RE 160
      COMMON /VALUES/ DT,TFAC,ERR,RFAC,SGF,SGS,VIF,VIS,DEL,TSEC,TMAX      RE 170
     1,TPUMP,STST,WFAC,WITER                                              RE 180
C                                                                         RE 190
      COMMON /ARRAYS/ FKX(NR,NC,NL),FKY(NR,NC,NL),                        RE 200
     1STORF(NR,NC,NL),STORS(NR,NC,NL),POR(NR,NC,NL),THCK(NR,NC,NL),       RE 210
     2ZBOT(NR,NC,NL),ZINT(NR,NC,NL),AQL(NR,NC,NL),RECH(NR,NC),            RE 220
     3PUMP(NR,NC,NL),PHIF(NR,NC,NL),PHIS(NR,NC,NL),DX(NC),DY(NR),         RE 230
     4WOPTF(10),TTFX(NR,NC,NL),TTFY(NR,NC,NL),TTSX(NR,NC,NL),             RE 240
     5TTSY(NR,NC,NL),DMAXF(200),DMAXS(200),THF(NR,NC,NL),                 RE 250
     6THS(NR,NC,NL),HEAD(NR,NC),PHIFP(NR,NC,NL),PHISP(NR,NC,NL),          RE 260
     7WTOP(NR,NC,NL),WBOT(NR,NC,NL),PUMPF(NR,NC,NL),FAREA(NR,NC,NL),      RE 270
     8SAREA(NR,NC,NL),ZTOP(NR,NC,NL),BATH(NR,NC),                         RE 280
     9FX(NR,NC,NL),FY(NR,NC,NL),SX(NR,NC,NL),SY(NR,NC,NL)                 RE 290
C                                                                         RE 300
      COMMON /IUNITS/IUN(32),IOP(12)                                      RE 310
C                                                                         RE 320
      DO 130 K=1,NNL                                                      RE 330
      WRITE(IUN(25),81)K,TSEC                                             RE 340
  81  FORMAT('    0       1.0   PHIF LAYER',I3,', T (SEC)=',G20.10)       RE 350
      DO 110 I=1,NNR                                                      RE 360
 110  WRITE(IUN(25),80)(PHIF(I,J,K),J=1,NNC)                              RE 370
      DO 115 I=1,NNR                                                      RE 380
      DO 115 J=1,NNC                                                      RE 390
        ZINT(I,J,K)=(1.+DEL)*PHIS(I,J,K)-DEL*PHIF(I,J,K)                  RE 400
 115  CONTINUE                                                            RE 410
      WRITE(IUN(26),82)K,TSEC                                             RE 420
  82  FORMAT('    0       1.0   ZINT LAYER',I3,', T (SEC)=',G20.10)       RE 430
      DO 120 I=1,NNR                                                      RE 440
 120  WRITE(IUN(26),80)(ZINT(I,J,K),J=1,NNC)                              RE 450
      WRITE(IUN(27),83)K,TSEC                                             RE 460
  83  FORMAT('    0       1.0   FX LAYER',I3,', T (SEC)=',G20.10)         RE 470
      DO 121 I=1,NNR                                                      RE 480
 121  WRITE(IUN(27),80)(FX(I,J,K),J=1,NNC)                                RE 490
      WRITE(IUN(28),84)K,TSEC                                             RE 500
  84  FORMAT('    0       1.0   FY LAYER',I3,', T (SEC)=',G20.10)         RE 510
      DO 122 I=1,NNR                                                      RE 520
 122  WRITE(IUN(28),80)(FY(I,J,K),J=1,NNC)                                RE 530
      WRITE(IUN(29),85)K,TSEC                                             RE 540
  85  FORMAT('    0       1.0   SX LAYER',I3,', T (SEC)=',G20.10)         RE 550
      DO 123 I=1,NNR                                                      RE 560
 123  WRITE(IUN(29),80)(SX(I,J,K),J=1,NNC)                                RE 570
      WRITE(IUN(30),86)K,TSEC                                             RE 580
  86  FORMAT('    0       1.0   SY LAYER',I3,', T (SEC)=',G20.10)         RE 590
      DO 124 I=1,NNR                                                      RE 600
 124  WRITE(IUN(30),80)(SY(I,J,K),J=1,NNC)                                RE 610
 130  CONTINUE                                                            RE 620
C                                                                         RE 630
   80 FORMAT(10F10.3)                                                     RE 640
C                                                                         RE 650
      RETURN                                                              RE 660
      END                                                                 RE 670
C                                                                         RE 680
C ------------------------------------------------------------------------OP  10
      SUBROUTINE OUTPUT(KWRITE)                                           OP  20
C ------------------------------------------------------------------------OP  30
C                                                                         OP  40
C  A SUBROUTINE TO PRINT MODEL SOLUTION                                   OP  50
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 OP  60
      PARAMETER (NR=40,NC=47,NL=5)                                        OP  70
      CHARACTER ICODE,IC,FR,M,S,TITLE*70                                  OP  80
C                                                                         OP  90
      COMMON /INTPAR/ NNR,NNC,NNL,NTSP,NP,NITP,NIT,NPUMP,IPUMP            OP 100
     1,ITMAX,NT,IWT(NR,NC),NFLAG,NCONT,NINT,                              OP 110
     2LW,IW(10),JW(10),KW(10),NUP,NLK                                     OP 120
C                                                                         OP 130
      COMMON /CHAR/ICODE(NR,NC,NL),IC(10,200),FR,M,S                      OP 140
C                                                                         OP 150
      COMMON /VALUES/ DT,TFAC,ERR,RFAC,SGF,SGS,VIF,VIS,DEL,TSEC,TMAX      OP 160
     1,TPUMP,STST,WFAC,WITER                                              OP 170
C                                                                         OP 180
      COMMON /ARRAYS/ FKX(NR,NC,NL),FKY(NR,NC,NL),                        OP 190
     1STORF(NR,NC,NL),STORS(NR,NC,NL),POR(NR,NC,NL),THCK(NR,NC,NL),       OP 200
     2ZBOT(NR,NC,NL),ZINT(NR,NC,NL),AQL(NR,NC,NL),RECH(NR,NC),            OP 210
     3PUMP(NR,NC,NL),PHIF(NR,NC,NL),PHIS(NR,NC,NL),DX(NC),DY(NR),         OP 220
     4WOPTF(10),TTFX(NR,NC,NL),TTFY(NR,NC,NL),TTSX(NR,NC,NL),             OP 230
     5TTSY(NR,NC,NL),DMAXF(200),DMAXS(200),THF(NR,NC,NL),                 OP 240
     6THS(NR,NC,NL),HEAD(NR,NC),PHIFP(NR,NC,NL),PHISP(NR,NC,NL),          OP 250
     7WTOP(NR,NC,NL),WBOT(NR,NC,NL),PUMPF(NR,NC,NL),FAREA(NR,NC,NL),      OP 260
     8SAREA(NR,NC,NL),ZTOP(NR,NC,NL),BATH(NR,NC),                         OP 270
     9FX(NR,NC,NL),FY(NR,NC,NL),SX(NR,NC,NL),SY(NR,NC,NL)                 OP 280
C                                                                         OP 290
      COMMON /BAL/FIN(7),FOUT(5),SSIN(7),SOUT(5),CFIN(7,NL),CFOUT(5,NL),  OP 300
     1CSIN(7,NL),CSOUT(5,NL),FFLOW(200),SFLOW(200),FLKT(NR,NC),           OP 310
     2SLKT(NR,NC),FLKB(NR,NC),SLKB(NR,NC),FSTOR,CFSTR(10),SSTOR,          OP 320
     3CSSTR(10),TFIN,TCFIN,TSSIN,TCSIN,TFOUT,TCFOUT,TSOUT,TCSOUT,         OP 330
     4FSLKT(NR,NC),SFLKT(NR,NC),FDIF,CFDIF,SDIF,CSDIF,FRERF,FRERS,        OP 340
     5FSLKB(NR,NC),SFLKB(NR,NC),CFRERF,CFRERS,SRERF,SRERS,CSRERF,CSRERS,  OP 350
     6IFL(200),JFL(200),ISL(200),JSL(200)                                 OP 360
C                                                                         OP 361
      SAVE /BAL/                                                          OP 362
C                                                                         OP 370
      COMMON /WHEAD/WPHIF(NR,NC),WPHIS(NR,NC),WZINT(NR,NC)                OP 380
C                                                                         OP 390
      COMMON /IUNITS/IUN(32),IOP(12)                                      OP 400
C                                                                         OP 410
C  PRINT TIME STEP INFORMATION                                            OP 420
      DTHRS=DT/3600.                                                      OP 430
      DTDAYS=DTHRS/24.                                                    OP 440
      DTYRS=DTDAYS/365.25                                                 OP 450
      THRS=TSEC/3600.                                                     OP 460
      TDAYS=THRS/24.                                                      OP 470
      TYRS=TDAYS/365.25                                                   OP 480
      PHRS=TPUMP/3600.                                                    OP 490
      PDAYS=PHRS/24.                                                      OP 500
      PYRS=PDAYS/365.25                                                   OP 510
      WRITE(IUN(24),10)NT,DT,DTHRS,DTDAYS,DTYRS,TPUMP,PHRS,PDAYS,PYRS,    OP 520
     1TSEC,THRS,TDAYS,TYRS                                                OP 530
      WRITE(IUN(24),20)(DMAXF(I),I=1,NIT)                                 OP 540
      WRITE(IUN(24),30)(DMAXS(I),I=1,NIT)                                 OP 550
C                                                                         OP 560
C  IF NUMBER OF TIME STEPS BETWEEN PRINTOUTS HAS NOT BEEN REACHED,        OP 570
C    CALCULATE MASS BALANCE AND BEGIN NEW TIME STEP                       OP 580
      IF(KWRITE.EQ.0)THEN                                                 OP 590
        DO 199 K=1,NNL                                                    OP 600
  199   CALL MASBAL(NFH,NSH,K)                                            OP 610
        RETURN                                                            OP 620
      ELSE                                                                OP 630
      DO 200 K=1,NNL                                                      OP 640
        WRITE(IUN(24),40)K                                                OP 650
        WRITE(IUN(32),45)NT,K                                             OP 660
C                                                                         OP 670
C  CALCULATE AND PRINT AQUIFER MASS BALANCE                               OP 680
        CALL MASBAL(NFH,NSH,K)                                            OP 690
        WRITE(IUN(24),50)                                                 OP 700
        WRITE(IUN(24),60)FSTOR,CFSTR(K),SSTOR,CSSTR(K),FIN(1),CFIN(1,K)   OP 710
        WRITE(IUN(24),70)(FIN(I),CFIN(I,K),SSIN(I),CSIN(I,K),I=2,7),      OP 720
     1TFIN,TCFIN,TSSIN,TCSIN                                              OP 730
        WRITE(IUN(24),80)(FOUT(I),CFOUT(I,K),SOUT(I),CSOUT(I,K),I=2,5),   OP 740
     1TFOUT,TCFOUT,TSOUT,TCSOUT,FDIF,CFDIF,SDIF,CSDIF,FRERF,              OP 750
     1CFRERF,SRERF,CSRERF,FRERS,CFRERS,SRERS,CSRERS                       OP 760
C                                                                         OP 770
        DO 99 I=1,NNR                                                     OP 780
        DO 99 J=1,NNC                                                     OP 790
          WPHIF(I,J)=PHIF(I,J,K)                                          OP 800
          WPHIS(I,J)=PHIS(I,J,K)                                          OP 810
          WZINT(I,J)=11111.                                               OP 820
          IF(FAREA(I,J,K).LE.0.)THEN                                      OP 830
            WPHIF(I,J)=11111.                                             OP 840
          END IF                                                          OP 850
          IF(SAREA(I,J,K).LE.0.)THEN                                      OP 860
            WPHIS(I,J)=11111.                                             OP 870
          END IF                                                          OP 880
        IF(ICODE(I,J,K).EQ.M)THEN                                         OP 890
          WZINT(I,J)=ZINT(I,J,K)                                          OP 900
        END IF                                                            OP 910
   99   CONTINUE                                                          OP 920
C                                                                         OP 930
C  PRINT AQUIFER POTENTIALS AND INTERFACE ELEVATIONS                      OP 940
        IF(IOP(4).EQ.1)THEN                                               OP 950
          WRITE(IUN(24),90)                                               OP 960
          DO 100 I=1,NNR                                                  OP 970
 100        WRITE(IUN(24),110)I,(WPHIF(I,J),J=1,NNC)                      OP 980
        END IF                                                            OP 990
        IF(IOP(5).EQ.1)THEN                                               OP1000
          WRITE(IUN(24),120)                                              OP1010
          DO 130 I=1,NNR                                                  OP1020
 130        WRITE(IUN(24),110)I,(WPHIS(I,J),J=1,NNC)                      OP1030
        END IF                                                            OP1040
        IF(IOP(6).EQ.1)THEN                                               OP1050
          WRITE(IUN(24),140)                                              OP1060
          DO 150 I=1,NNR                                                  OP1070
 150        WRITE(IUN(24),110)I,(WZINT(I,J),J=1,NNC)                      OP1080
        END IF                                                            OP1090
        IF(IOP(7).EQ.1)THEN                                               OP1100
          WRITE(IUN(24),220)                                              OP1110
          DO 190 I=1,NNR                                                  OP1120
 190        WRITE(IUN(24),240)I,(FAREA(I,J,K),J=1,NNC)                    OP1130
        END IF                                                            OP1140
        IF(IOP(8).EQ.1)THEN                                               OP1150
          WRITE(IUN(24),230)                                              OP1160
          DO 195 I=1,NNR                                                  OP1170
 195        WRITE(IUN(24),240)I,(SAREA(I,J,K),J=1,NNC)                    OP1180
        END IF                                                            OP1190
        IF(IOP(9).EQ.1)THEN                                               OP1200
          WRITE(IUN(24),160)                                              OP1210
          DO 170 I=1,NNR                                                  OP1220
 170        WRITE(IUN(24),180)I,(ICODE(I,J,K),J=1,NNC)                    OP1230
        END IF                                                            OP1240
        IF(IOP(10).EQ.1)THEN                                              OP1250
          WRITE(IUN(32),250)                                              OP1260
          DO 196 I=1,NNR                                                  OP1270
  196     WRITE(IUN(32),111)I,(FLKT(I,J),J=1,NNC)                         OP1280
          WRITE(IUN(32),251)                                              OP1281
          DO 296 I=1,NNR                                                  OP1282
  296     WRITE(IUN(32),111)I,(FSLKT(I,J),J=1,NNC)                        OP1283
          WRITE(IUN(32),260)                                              OP1290
          DO 197 I=1,NNR                                                  OP1300
  197     WRITE(IUN(32),111)I,(FLKB(I,J),J=1,NNC)                         OP1310
          WRITE(IUN(32),261)                                              OP1311
          DO 297 I=1,NNR                                                  OP1312
  297     WRITE(IUN(32),111)I,(FSLKB(I,J),J=1,NNC)                        OP1313
          WRITE(IUN(32),270)                                              OP1320
          DO 198 I=1,NNR                                                  OP1330
  198     WRITE(IUN(32),111)I,(SLKT(I,J),J=1,NNC)                         OP1340
          WRITE(IUN(32),271)                                              OP1341
          DO 298 I=1,NNR                                                  OP1342
  298     WRITE(IUN(32),111)I,(SFLKT(I,J),J=1,NNC)                        OP1343
          WRITE(IUN(32),280)                                              OP1350
          DO 201 I=1,NNR                                                  OP1360
  201     WRITE(IUN(32),111)I,(SLKB(I,J),J=1,NNC)                         OP1370
          WRITE(IUN(32),281)                                              OP1371
          DO 301 I=1,NNR                                                  OP1372
  301     WRITE(IUN(32),111)I,(SFLKB(I,J),J=1,NNC)                        OP1373
          IF(NFH.GT.0)THEN                                                OP1380
            WRITE(IUN(32),290)                                            OP1390
            WRITE(IUN(32),300)(IFL(I),JFL(I),FFLOW(I),I=1,NFH)            OP1400
          END IF                                                          OP1410
          IF(NSH.GT.0)THEN                                                OP1420
            WRITE(IUN(32),310)                                            OP1430
            WRITE(IUN(32),300)(ISL(I),JSL(I),SFLOW(I),I=1,NSH)            OP1440
          END IF                                                          OP1450
        END IF                                                            OP1460
 200  CONTINUE                                                            OP1470
      END IF                                                              OP1480
C                                                                         OP1490
      RETURN                                                              OP1500
C                                                                         OP1510
  10  FORMAT(/1X,13('-')/' TIME STEP',I4/1X,13('-')/26X,'SECONDS',        OP1520
     16X,'HOURS',6X,'DAYS',7X,'YEARS'/'   TIME STEP SIZE:',7X,4G11.4/     OP1530
     2'   PUMPING PERIOD TIME:',2X,4G11.4/'   TOTAL SIMULATION TIME:',    OP1540
     44G11.4/)                                                            OP1550
  20  FORMAT(' MAXIMUM FRESHWATER HEAD CHANGE:',8G11.4                    OP1560
     1/(33X,8G11.4))                                                      OP1570
  30  FORMAT(' MAXIMUM SALTWATER HEAD CHANGE: ',8G11.4                    OP1580
     1/(33X,8G11.4))                                                      OP1590
  40  FORMAT(/' AQUIFER',I4/1X,11('-'))                                   OP1600
  45  FORMAT(//1X,13('-')/' TIME STEP',I4/1X,13('-')//                    OP1610
     1' AQUIFER',I4/1X,11('-'))                                           OP1620
  50  FORMAT(5X,' MASS BALANCE:')                                         OP1630
  60  FORMAT(38X,'  FRESHWATER ',21X,' SALTWATER'                         OP1640
     1/35X,'T.S. RATE',3X,'CUM. VOLUME',9X,'T.S. RATE',3X,                OP1650
     2'CUM. VOLUME'/9X,' CHANGE IN STORAGE',7X,2G11.4,10X,2G11.4/         OP1660
     410X,' SOURCES'/10X,'   RECHARGE',14X,2G11.4)                        OP1670
  70  FORMAT(10X,'   CONSTANT HEAD NODES',3X,2G11.4,10X,2G11.4/           OP1680
     110X,'   INJECTION',13X,2G11.4,10X,2G11.4/                           OP1690
     210X,'   FW LEAKAGE ACROSS TOP',1X,2G11.4,10X,2G11.4/                OP1700
     210X,'   FW LEAKAGE ACROSS BOT',1X,2G11.4,10X,2G11.4/                OP1710
     210X,'   SW LEAKAGE ACROSS TOP',1X,2G11.4,10X,2G11.4/                OP1711
     210X,'   SW LEAKAGE ACROSS BOT',1X,2G11.4,10X,2G11.4/                OP1712
     310X,'   TOTAL',17X,2G11.4,10X,2G11.4)                               OP1720
  80  FORMAT(10X,' SINKS'/10X,'   CONSTANT HEAD NODES',3X,2G11.4,10X,     OP1730
     12G11.4/10X,'   PUMPAGE',15X,2G11.4,10X,2G11.4/10X,'   LEAKAGE',     OP1740
     1' ACROSS TOP',4X,2G11.4,10X,2G11.4/10X,'   LEAKAGE ACROSS BOT',     OP1750
     24X,2G11.4,10X,2G11.4/10X,'   TOTAL',17X,2G11.4,10X,2G11.4/          OP1760
     310X,' SOURCES-SINKS',11X,2G11.4,10X,2G11.4/                         OP1770
     48X,' RELATIVE ERROR (%)'/                                           OP1780
     512X,' INFLUX',16X,2G11.4,10X,2G11.4/12X,' STORAGE',15X,2G11.4,10X,  OP1790
     62G11.4)                                                             OP1800
  90  FORMAT(/' FRESHWATER HEADS')                                        OP1810
 110  FORMAT(I5,3X,10G11.5/(8X,10G11.5))                                  OP1820
 111  FORMAT(I5,3X,10G11.4/(8X,10G11.4))                                  OP1830
 120  FORMAT(/' SALTWATER HEADS')                                         OP1840
 140  FORMAT(/' INTERFACE ELEVATION')                                     OP1850
 160  FORMAT(/' MAP OF EXTENT OF INTRUSION',                              OP1860
     1' (F-FRESHWATER, M-FRESH AND SALTWATER, S-SALTWATER)'/)             OP1870
 180  FORMAT(I5,3X,50A2/(8X,50A2))                                        OP1880
 210  FORMAT(/' CUMULATIVE MASS BALANCE:')                                OP1890
 220  FORMAT(//' FAREA'/)                                                 OP1900
 230  FORMAT(//' SAREA'/)                                                 OP1910
 240  FORMAT(I5,3X,30F4.1/(8X,30F4.1))                                    OP1920
 250  FORMAT(/' FRESHWATER LEAKAGE ACROSS TOP FROM OVERLYING ',           OP1930
     &'FRESHWATER')                                                       OP1931
 251  FORMAT(/' FRESHWATER LEAKAGE ACROSS TOP FROM OVERLYING SALTWATER')  OP1932
 260  FORMAT(/' FRESHWATER LEAKAGE ACROSS BOT FROM UNDERLYING ',          OP1940
     &'FRESHWATER')                                                       OP1941
 261  FORMAT(/' FRESHWATER LEAKAGE ACROSS BOT FROM UNDERLYING ',          OP1942
     &'SALTWATER')                                                        OP1943
 270  FORMAT(/' SALTWATER LEAKAGE ACROSS TOP FROM OVERLYING SALTWATER')   OP1950
 271  FORMAT(/' SALTWATER LEAKAGE ACROSS TOP FROM OVERLYING FRESHWATER')  OP1951
 280  FORMAT(/' SALTWATER LEAKAGE ACROSS BOT FROM UNDERLYING ',           OP1960
     &'SALTWATER')                                                        OP1961
 281  FORMAT(/' SALTWATER LEAKAGE ACROSS BOT FROM UNDERLYING ',           OP1962
     &'FRESHWATER')                                                       OP1963
 290  FORMAT(/' FLOW RATES TO FRESHWATER CONSTANT HEAD NODES:'/           OP1970
     19X,'I',4X,'J',7X,'RATE',3(14X,'I',4X,'J',7X,'RATE'))                OP1980
 300  FORMAT(4(5X,2I5,3X,G11.4))                                          OP1990
 310  FORMAT(/' FLOW RATES TO SALTWATER CONSTANT HEAD NODES:'/            OP2000
     19X,'I',4X,'J',5X,'RATE',3(14X,'I',4X,'J',5X,'RATE'))                OP2010
      END                                                                 OP2020
C                                                                         OP2030
C ------------------------------------------------------------------------MB  10
      SUBROUTINE MASBAL(NFH,NSH,K)                                        MB  20
C ------------------------------------------------------------------------MB  30
C                                                                         MB  40
C  A SUBROUTINE TO CALCULATE AQUIFER AND CUMULATIVE MASS BALANCES         MB  50
C                                                                         MB  60
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 MB  70
      PARAMETER (NR=40,NC=47,NL=5)                                        MB  80
      CHARACTER ICODE,IC,FR,M,S,TITLE*70                                  MB  90
C                                                                         MB 100
      COMMON /INTPAR/ NNR,NNC,NNL,NTSP,NP,NITP,NIT,NPUMP,IPUMP            MB 110
     1,ITMAX,NT,IWT(NR,NC),NFLAG,NCONT,NINT,                              MB 120
     2LW,IW(10),JW(10),KW(10),NUP,NLK                                     MB 130
C                                                                         MB 140
      COMMON /CHAR/ICODE(NR,NC,NL),IC(10,200),FR,M,S                      MB 150
C                                                                         MB 160
      COMMON /VALUES/ DT,TFAC,ERR,RFAC,SGF,SGS,VIF,VIS,DEL,TSEC,TMAX      MB 170
     1,TPUMP,STST,WFAC,WITER                                              MB 180
C                                                                         MB 190
      COMMON /ARRAYS/ FKX(NR,NC,NL),FKY(NR,NC,NL),                        MB 200
     1STORF(NR,NC,NL),STORS(NR,NC,NL),POR(NR,NC,NL),THCK(NR,NC,NL),       MB 210
     2ZBOT(NR,NC,NL),ZINT(NR,NC,NL),AQL(NR,NC,NL),RECH(NR,NC),            MB 220
     3PUMP(NR,NC,NL),PHIF(NR,NC,NL),PHIS(NR,NC,NL),DX(NC),DY(NR),         MB 230
     4WOPTF(10),TTFX(NR,NC,NL),TTFY(NR,NC,NL),TTSX(NR,NC,NL),             MB 240
     5TTSY(NR,NC,NL),DMAXF(200),DMAXS(200),THF(NR,NC,NL),                 MB 250
     6THS(NR,NC,NL),HEAD(NR,NC),PHIFP(NR,NC,NL),PHISP(NR,NC,NL),          MB 260
     7WTOP(NR,NC,NL),WBOT(NR,NC,NL),PUMPF(NR,NC,NL),FAREA(NR,NC,NL),      MB 270
     8SAREA(NR,NC,NL),ZTOP(NR,NC,NL),BATH(NR,NC),                         MB 280
     9FX(NR,NC,NL),FY(NR,NC,NL),SX(NR,NC,NL),SY(NR,NC,NL)                 MB 290
C                                                                         MB 300
      COMMON /BAL/FIN(7),FOUT(5),SSIN(7),SOUT(5),CFIN(7,NL),CFOUT(5,NL),  MB 310
     1CSIN(7,NL),CSOUT(5,NL),FFLOW(200),SFLOW(200),FLKT(NR,NC),           MB 320
     2SLKT(NR,NC),FLKB(NR,NC),SLKB(NR,NC),FSTOR,CFSTR(10),SSTOR,          MB 330
     3CSSTR(10),TFIN,TCFIN,TSSIN,TCSIN,TFOUT,TCFOUT,TSOUT,TCSOUT,         MB 340
     4FSLKT(NR,NC),SFLKT(NR,NC),FDIF,CFDIF,SDIF,CSDIF,FRERF,FRERS,        MB 350
     5FSLKB(NR,NC),SFLKB(NR,NC),CFRERF,CFRERS,SRERF,SRERS,CSRERF,CSRERS,  MB 360
     6IFL(200),JFL(200),ISL(200),JSL(200)                                 MB 370
C                                                                         MB 371
      COMMON /IUNITS/IUN(32),IOP(12)                                      MB 372
C                                                                         MB 373
      SAVE /BAL/                                                          MB 374
C                                                                         MB 380
C  INITIALIZE VARIABLES                                                   MB 390
      IF(NFLAG.EQ.1)THEN                                                  MB 400
        DO 11 L=1,NNL                                                     MB 410
        CFSTR(L)=0.0                                                      MB 420
        CSSTR(L)=0.0                                                      MB 430
        DO 10 I=1,5                                                       MB 440
          CFIN(I,L)=0.                                                    MB 450
          CFOUT(I,L)=0.                                                   MB 460
          CSIN(I,L)=0.                                                    MB 470
          CSOUT(I,L)=0.                                                   MB 480
   10   CONTINUE                                                          MB 490
        CFIN(6,L)=0.0                                                     MB 491
        CFIN(7,L)=0.0                                                     MB 492
        CSIN(6,L)=0.0                                                     MB 493
        CSIN(7,L)=0.0                                                     MB 494
   11   CONTINUE                                                          MB 495
      END IF                                                              MB 500
      DO 20 I=1,5                                                         MB 510
        FIN(I)=0.                                                         MB 520
        FOUT(I)=0.                                                        MB 530
        SSIN(I)=0.                                                        MB 540
        SOUT(I)=0.                                                        MB 550
 20   CONTINUE                                                            MB 560
      FIN(6)=0.                                                           MB 561
      FIN(7)=0.                                                           MB 562
      SSIN(6)=0.                                                          MB 563
      SSIN(7)=0.                                                          MB 564
      NFLAG=0                                                             MB 570
      FSTOR=0.0                                                           MB 580
      SSTOR=0.0                                                           MB 590
      FRERF=0.0                                                           MB 600
      SRERF=0.0                                                           MB 610
      FRERS=0.0                                                           MB 620
      SRERS=0.0                                                           MB 630
      CFRERF=0.0                                                          MB 640
      CSRERF=0.0                                                          MB 650
      CFRERS=0.0                                                          MB 660
      CSRERS=0.0                                                          MB 670
      NFH=0                                                               MB 680
      NSH=0                                                               MB 690
C                                                                         MB 700
      DO 199 I=1,NNR                                                      MB 710
      DO 199 J=1,NNC                                                      MB 720
        FLKT(I,J)=0.                                                      MB 730
        FSLKT(I,J)=0.                                                     MB 731
        FLKB(I,J)=0.                                                      MB 740
        FSLKB(I,J)=0.                                                     MB 741
        SLKT(I,J)=0.                                                      MB 750
        SFLKT(I,J)=0.                                                     MB 751
        SLKB(I,J)=0.                                                      MB 760
        SFLKB(I,J)=0.                                                     MB 761
 199  CONTINUE                                                            MB 770
C  BEGIN CALCULATIONS                                                     MB 780
      DO 200 I=2,NNR-1                                                    MB 790
      DO 200 J=2,NNC-1                                                    MB 800
      IF (FKX(I,J,K).EQ.0.) GO TO 200                                     MB 810
        AREA=DX(J)*DY(I)                                                  MB 820
        AL=0.0                                                            MB 830
        IF(K.EQ.NNL.AND.IWT(I,J).EQ.1)AL=1.0                              MB 840
C                                                                         MB 850
C  COMPUTE FRESHWATER AND SALTWATER RATE CHANGES FOR TIME STEP            MB 860
C  CONSTANT FRESHWATER HEAD NODES                                         MB 870
        IF(STORF(I,J,K).GE.0.) GO TO 50                                   MB 880
          NFH=NFH+1                                                       MB 890
          IFL(NFH)=I                                                      MB 900
          JFL(NFH)=J                                                      MB 910
          FFLOW(NFH)=0.0                                                  MB 920
          IF(STORF(I,J-1,K).GE.0..AND.FAREA(I,J-1,K).GT.0.)FFLOW(NFH)=    MB 930
     1FFLOW(NFH)+(PHIF(I,J,K)-PHIF(I,J-1,K))*TTFX(I,J-1,K)*DY(I)          MB 940
          IF(STORF(I,J+1,K).GE.0..AND.FAREA(I,J+1,K).GT.0.)FFLOW(NFH)=    MB 950
     1FFLOW(NFH)+(PHIF(I,J,K)-PHIF(I,J+1,K))*TTFX(I,J,K)*DY(I)            MB 960
          IF(STORF(I-1,J,K).GE.0..AND.FAREA(I-1,J,K).GT.0.)FFLOW(NFH)=    MB 970
     1FFLOW(NFH)+(PHIF(I,J,K)-PHIF(I-1,J,K))*TTFY(I-1,J,K)*DX(J)          MB 980
          IF(STORF(I+1,J,K).GE.0..AND.FAREA(I+1,J,K).GT.0.)FFLOW(NFH)=    MB 990
     1FFLOW(NFH)+(PHIF(I,J,K)-PHIF(I+1,J,K))*TTFY(I,J,K)*DX(J)            MB1000
        IF(FFLOW(NFH).GE.0.)THEN                                          MB1010
          FIN(2)=FIN(2)+FFLOW(NFH)                                        MB1020
        ELSE                                                              MB1030
          FOUT(2)=FOUT(2)-FFLOW(NFH)                                      MB1040
        END IF                                                            MB1050
C                                                                         MB1060
C  CONSTANT SALTWATER HEAD NODES                                          MB1070
  50    IF(STORS(I,J,K).GE.0.) GO TO 80                                   MB1080
          NSH=NSH+1                                                       MB1090
          ISL(NSH)=I                                                      MB1100
          JSL(NSH)=J                                                      MB1110
          SFLOW(NSH)=0.0                                                  MB1120
          IF(STORS(I,J-1,K).GE.0..AND.SAREA(I,J-1,K).GT.0.)SFLOW(NSH)=    MB1130
     1SFLOW(NSH)+(PHIS(I,J,K)-PHIS(I,J-1,K))*TTSX(I,J-1,K)*DY(I)          MB1140
          IF(STORS(I,J+1,K).GE.0..AND.SAREA(I,J+1,K).GT.0.)SFLOW(NSH)=    MB1150
     1SFLOW(NSH)+(PHIS(I,J,K)-PHIS(I,J+1,K))*TTSX(I,J,K)*DY(I)            MB1160
          IF(STORS(I-1,J,K).GE.0..AND.SAREA(I-1,J,K).GT.0.)SFLOW(NSH)=    MB1170
     1SFLOW(NSH)+(PHIS(I,J,K)-PHIS(I-1,J,K))*TTSY(I-1,J,K)*DX(J)          MB1180
          IF(STORS(I+1,J,K).GE.0..AND.SAREA(I+1,J,K).GT.0.)SFLOW(NSH)=    MB1190
     1SFLOW(NSH)+(PHIS(I,J,K)-PHIS(I+1,J,K))*TTSY(I,J,K)*DX(J)            MB1200
        IF(SFLOW(NSH).GE.0.)THEN                                          MB1210
          SSIN(2)=SSIN(2)+SFLOW(NSH)                                      MB1220
        ELSE                                                              MB1230
          SOUT(2)=SOUT(2)-SFLOW(NSH)                                      MB1240
        END IF                                                            MB1250
C                                                                         MB1260
C  RECHARGE                                                               MB1270
  80    FIN(1)=FIN(1)+AL*RECH(I,J)*AREA                                   MB1280
C                                                                         MB1290
C  PUMPAGE                                                                MB1300
        IF(PUMP(I,J,K).EQ.0.)GO TO 140                                    MB1310
          PUMPS=PUMP(I,J,K)-PUMPF(I,J,K)                                  MB1320
          IF(PUMPF(I,J,K).LE.0.)THEN                                      MB1330
            FIN(3)=FIN(3)-PUMPF(I,J,K)                                    MB1340
          ELSE                                                            MB1350
            FOUT(3)=FOUT(3)+PUMPF(I,J,K)                                  MB1360
          END IF                                                          MB1370
          IF(PUMPS.LE.0.)THEN                                             MB1380
            SSIN(3)=SSIN(3)-PUMPS                                         MB1390
          ELSE                                                            MB1400
            SOUT(3)=SOUT(3)+PUMPS                                         MB1410
          END IF                                                          MB1420
C                                                                         MB1430
C  STORAGE                                                                MB1440
  140   APROP=FAREA(I,J,K)+SAREA(I,J,K)-1.                                MB1450
        IF(APROP.GT.0.)THEN                                               MB1460
          FST=AREA*((POR(I,J,K)*DEL*APROP+(POR(I,J,K)*AL                  MB1470
     1+STORF(I,J,K)*THF(I,J,K))*FAREA(I,J,K))*(PHIF(I,J,K)                MB1480
     2-PHIFP(I,J,K))-POR(I,J,K)*(1.+DEL)*APROP*(PHIS(I,J,K)               MB1490
     3-PHISP(I,J,K)))                                                     MB1500
          SST=AREA*((POR(I,J,K)*(1.+DEL)*APROP+STORS(I,J,K)               MB1510
     1*THS(I,J,K)*SAREA(I,J,K))*(PHIS(I,J,K)-PHISP(I,J,K))                MB1520
     2-POR(I,J,K)*DEL*APROP*(PHIF(I,J,K)-PHIFP(I,J,K)))                   MB1530
        ELSE                                                              MB1540
          FST=AREA*(STORF(I,J,K)*THF(I,J,K)+AL*POR(I,J,K))                MB1550
     1*FAREA(I,J,K)*(PHIF(I,J,K)-PHIFP(I,J,K))                            MB1560
          SST=AREA*STORS(I,J,K)*THS(I,J,K)*SAREA(I,J,K)                   MB1570
     1*(PHIS(I,J,K)-PHISP(I,J,K))                                         MB1580
        END IF                                                            MB1590
C                                                                         MB1600
        IF(STORF(I,J,K).LT.0.)FST=0.                                      MB1610
        IF(STORS(I,J,K).LT.0.)SST=0.                                      MB1620
        FSTOR=FSTOR+FST/DT                                                MB1630
        SSTOR=SSTOR+SST/DT                                                MB1640
        CFSTR(K)=CFSTR(K)+FST                                             MB1650
        CSSTR(K)=CSSTR(K)+SST                                             MB1660
C                                                                         MB1670
C  LEAKAGE                                                                MB1680
        K1=K-1                                                            MB1690
        K2=K+1                                                            MB1700
C                                                                         MB1710
C  ACROSS TOP                                                             MB1720
        AQLTOP=(1.-AL)*AQL(I,J,K)*AREA                                    MB1730
        AQLBOT=0.0                                                        MB1740
        SGR=SGS/SGF                                                       MB1750
        IF(K.NE.1)AQLBOT=AQL(I,J,K1)*AREA                                 MB1760
      WT1=1.0                                                             MB1770
      WT2=1.0                                                             MB1780
      WT3=0.0                                                             MB1790
      RT3=1.0                                                             MB1800
      WT4=1.0                                                             MB1810
      WT5=1.0                                                             MB1811
        IF(NLK.EQ.0)THEN                                                  MB1820
          WT2=0.0                                                         MB1860
          WT3=FAREA(I,J,K)                                                MB1870
          WT4=0.0                                                         MB1910
          IF(K.NE.NNL)THEN                                                MB1821
            DIRP=(SGR*PHIS(I,J,K+1)-PHIF(I,J,K))-(ZBOT(I,J,K2)+           MB1830
     &ZTOP(I,J,K))/(2.*DEL)                                               MB1840
            IF(DIRP.GT.0.0)WT1=0.0                                        MB1850
            IF(ICODE(I,J,K2).EQ.'.')THEN                                  MB1860
              WT1=1.0                                                     MB1870
              WT2=1.0                                                     MB1880
              IF(PHIF(I,J,K2).GT.PHIF(I,J,K).AND.BATH(I,J).LT.0.)WT5=0.0  MB1890
            END IF                                                        MB1900
          END IF                                                          MB1910
          IF(K.NE.1)THEN                                                  MB1920
            DIRM=(PHIF(I,J,K1)-SGR*PHIS(I,J,K))+(ZBOT(I,J,K)+             MB1930
     &ZTOP(I,J,K1))/(2.*DEL)                                              MB1940
            IF(DIRM.LT.0.0)RT3=0.0                                        MB1950
          END IF                                                          MB1960
        END IF                                                            MB1965
        IF(K.EQ.NNL)THEN                                                  MB1970
          FLKT(I,J)=(1.-AL)*FAREA(I,J,K)*AQLTOP*(HEAD(I,J)-PHIF(I,J,K))   MB1980
          IF(BATH(I,J).LT.0.0)THEN                                        MB1981
            IF(FLKT(I,J).GT.0.)THEN                                       MB1982
              IF(NLK.EQ.0)THEN                                            MB1983
                FLKT(I,J)=0.D0                                            MB1984
              ELSE                                                        MB1985
                FSLKT(I,J)=FLKT(I,J)                                      MB1986
                FLKT(I,J)=0.D0                                            MB1987
              END IF                                                      MB1988
            END IF                                                        MB1989
          END IF                                                          MB1990
          SLKT(I,J)=(1.-AL)*(1.-FAREA(I,J,K))*AQLTOP*(HEAD(I,J)/SGR+      MB1991
     1BATH(I,J)/(DEL+1.)-PHIS(I,J,K))                                     MB2000
        ELSE                                                              MB2010
          IF(FAREA(I,J,K).GE.(1.-SAREA(I,J,K2)))THEN                      MB2020
            FLKT(I,J)=(1.-SAREA(I,J,K2))*AQLTOP*(PHIF(I,J,K2)-            MB2030
     1PHIF(I,J,K))                                                        MB2031
            FSLKT(I,J)=WT1*(FAREA(I,J,K)-(1.-SAREA(I,J,K2)))*AQLTOP*(     MB2032
     2(SGR*PHIS(I,J,K+1)-PHIF(I,J,K))-(ZBOT(I,J,K2)+ZTOP(I,J,K))/         MB2033
     3(2.*DEL))                                                           MB2040
            IF(FSLKT(I,J).LT.0.0)THEN                                     MB2041
              FLKT(I,J)=FLKT(I,J)+FSLKT(I,J)                              MB2042
              FSLKT(I,J)=0.0                                              MB2043
            END IF                                                        MB2040
            SLKT(I,J)=(1.-FAREA(I,J,K))*AQLTOP*(PHIS(I,J,K2)-             MB2050
     1PHIS(I,J,K))                                                        MB2060
          ELSE                                                            MB2070
            FLKT(I,J)=FAREA(I,J,K)*AQLTOP*(PHIF(I,J,K2)-PHIF(I,J,K))*WT5  MB2080
            SLKT(I,J)=SAREA(I,J,K2)*AQLTOP*(PHIS(I,J,K2)-PHIS(I,J,K))     MB2090
            SFLKT(I,J)=WT2*((1.-FAREA(I,J,K))-                            MB2100
     &SAREA(I,J,K2))*AQLTOP*                                              MB2120
     1(PHIF(I,J,K2)-SGR*PHIS(I,J,K)+(ZBOT(I,J,K2)+ZTOP(I,J,K))/           MB2130
     2(2.*DEL))                                                           MB2140
            IF(ICODE(I,J,K2).EQ.'.'.AND.BATH(I,J).LT.0.0)THEN             MB2150
              IF(FLKT(I,J).GT.0.0)THEN                                    MB2151
                FSLKT(I,J)=FLKT(I,J)                                      MB2152
                FLKT(I,J)=0.0                                             MB2153
              END IF                                                      MB2154
              SLKT(I,J)=SLKT(I,J)+SFLKT(I,J)                              MB2155
              SFLKT(I,J)=0.0                                              MB2156
            END IF                                                        MB2157
          END IF                                                          MB2160
        END IF                                                            MB2170
C                                                                         MB2180
C  ACROSS BOTTOM                                                          MB2190
        IF(K.NE.1)THEN                                                    MB2200
          IF(FAREA(I,J,K1).GT.(1.-SAREA(I,J,K)))THEN                      MB2210
            FLKB(I,J)=(1.-SAREA(I,J,K))*AQLBOT*(PHIF(I,J,K1)-             MB2220
     1PHIF(I,J,K))+RT3*WT3*(FAREA(I,J,K1)-(1.-SAREA(I,J,K)))*             MB2230
     1AQLBOT*(PHIF(I,J,K1)-SGR*PHIS(I,J,K)+(ZBOT(I,J,K)+ZTOP(I,J,K1))/    MB2240
     2(2.*DEL))                                                           MB2250
            SLKB(I,J)=(1.-FAREA(I,J,K1))*AQLBOT*(PHIS(I,J,K1)-            MB2251
     &PHIS(I,J,K))                                                        MB2252
            SFLKB(I,J)=RT3*(1.-WT3)*(FAREA(I,J,K1)-                       MB2260
     &(1.-SAREA(I,J,K)))*                                                 MB2270
     1AQLBOT*(PHIF(I,J,K1)-SGR*PHIS(I,J,K)+(ZBOT(I,J,K)+ZTOP(I,J,K1))/    MB2280
     2(2.*DEL))                                                           MB2290
          ELSE                                                            MB2300
            FLKB(I,J)=FAREA(I,J,K1)*AQLBOT*(PHIF(I,J,K1)-                 MB2310
     1PHIF(I,J,K))                                                        MB2311
            FSLKB(I,J)=WT4*((1.-FAREA(I,J,K1))-SAREA(I,J,K))*AQLBOT*      MB2320
     2(SGR*PHIS(I,J,K1)-PHIF(I,J,K)-(ZBOT(I,J,K)+ZTOP(I,J,K1))/(2.*DEL))  MB2330
            SLKB(I,J)=SAREA(I,J,K)*AQLBOT*(PHIS(I,J,K1)-PHIS(I,J,K))      MB2340
          END IF                                                          MB2350
        END IF                                                            MB2360
C                                                                         MB2370
        IF(STORF(I,J,K).LT.0.)THEN                                        MB2380
           FFLOW(NFH)=FFLOW(NFH)-FLKT(I,J)-FLKB(I,J)-FSLKT(I,J)           MB2390
     &-FSLKB(I,J)                                                         MB2410
           IF(FLKT(I,J).LE.0.)THEN                                        MB2420
             FIN(2)=FIN(2)-FLKT(I,J)                                      MB2430
           ELSE                                                           MB2440
             FOUT(2)=FOUT(2)+FLKT(I,J)                                    MB2450
           END IF                                                         MB2460
           IF(FLKB(I,J).LE.0.)THEN                                        MB2470
             FIN(2)=FIN(2)-FLKB(I,J)                                      MB2480
           ELSE                                                           MB2490
             FOUT(2)=FOUT(2)+FLKB(I,J)                                    MB2500
           END IF                                                         MB2510
           IF(FSLKT(I,J).LE.0.)THEN                                       MB2420
             FIN(2)=FIN(2)-FSLKT(I,J)                                     MB2430
           ELSE                                                           MB2440
             FOUT(2)=FOUT(2)+FSLKT(I,J)                                   MB2450
           END IF                                                         MB2460
           IF(FSLKB(I,J).LE.0.)THEN                                       MB2470
             FIN(2)=FIN(2)-FSLKB(I,J)                                     MB2480
           ELSE                                                           MB2490
             FOUT(2)=FOUT(2)+FSLKB(I,J)                                   MB2500
           END IF                                                         MB2510
        END IF                                                            MB2520
        IF(STORS(I,J,K).LT.0.)THEN                                        MB2530
           SFLOW(NSH)=SFLOW(NSH)-SLKT(I,J)-SLKB(I,J)-SFLKT(I,J)           MB2540
     &-SFLKB(I,J)                                                         MB2560
           IF(SLKT(I,J).LE.0.)THEN                                        MB2570
             SSIN(2)=SSIN(2)-SLKT(I,J)                                    MB2580
           ELSE                                                           MB2590
             SOUT(2)=SOUT(2)+SLKT(I,J)                                    MB2600
           END IF                                                         MB2610
           IF(SLKB(I,J).LE.0.)THEN                                        MB2620
             SSIN(2)=SSIN(2)-SLKB(I,J)                                    MB2630
           ELSE                                                           MB2640
             SOUT(2)=SOUT(2)+SLKB(I,J)                                    MB2650
           END IF                                                         MB2660
           IF(SFLKT(I,J).LE.0.)THEN                                       MB2570
             SSIN(2)=SSIN(2)-SFLKT(I,J)                                   MB2580
           ELSE                                                           MB2590
             SOUT(2)=SOUT(2)+SFLKT(I,J)                                   MB2600
           END IF                                                         MB2610
           IF(SFLKB(I,J).LE.0.)THEN                                       MB2620
             SSIN(2)=SSIN(2)-SFLKB(I,J)                                   MB2630
           ELSE                                                           MB2640
             SOUT(2)=SOUT(2)+SFLKB(I,J)                                   MB2650
           END IF                                                         MB2660
        END IF                                                            MB2670
C                                                                         MB2680
C  SUM VALUES                                                             MB2690
        IF(FLKT(I,J).GE.0.)THEN                                           MB2700
          FIN(4)=FIN(4)+FLKT(I,J)                                         MB2710
        ELSE                                                              MB2720
          FOUT(4)=FOUT(4)-FLKT(I,J)                                       MB2730
        END IF                                                            MB2740
        IF(FSLKT(I,J).GE.0.)THEN                                          MB2741
          FIN(6)=FIN(6)+FSLKT(I,J)                                        MB2742
        ELSE                                                              MB2743
          FOUT(4)=FOUT(4)-FSLKT(I,J)                                      MB2744
        END IF                                                            MB2745
        IF(FLKB(I,J).GE.0.)THEN                                           MB2750
          FIN(5)=FIN(5)+FLKB(I,J)                                         MB2760
        ELSE                                                              MB2770
          FOUT(5)=FOUT(5)-FLKB(I,J)                                       MB2780
        END IF                                                            MB2790
        IF(FSLKB(I,J).GE.0.)THEN                                          MB2791
          FIN(7)=FIN(7)+FSLKB(I,J)                                        MB2792
        ELSE                                                              MB2793
          FOUT(5)=FOUT(5)-FSLKB(I,J)                                      MB2794
        END IF                                                            MB2795
        IF(SLKT(I,J).GE.0.)THEN                                           MB2800
          SSIN(6)=SSIN(6)+SLKT(I,J)                                       MB2810
        ELSE                                                              MB2820
          SOUT(4)=SOUT(4)-SLKT(I,J)                                       MB2830
        END IF                                                            MB2840
        IF(SFLKT(I,J).GE.0.)THEN                                          MB2841
          SSIN(4)=SSIN(4)+SFLKT(I,J)                                      MB2842
        ELSE                                                              MB2843
          SOUT(4)=SOUT(4)-SFLKT(I,J)                                      MB2845
        END IF                                                            MB2846
        IF(SLKB(I,J).GE.0.)THEN                                           MB2850
          SSIN(7)=SSIN(7)+SLKB(I,J)                                       MB2860
        ELSE                                                              MB2870
          SOUT(5)=SOUT(5)-SLKB(I,J)                                       MB2880
        END IF                                                            MB2890
        IF(SFLKB(I,J).GE.0.)THEN                                          MB2891
          SSIN(5)=SSIN(5)+SFLKB(I,J)                                      MB2892
        ELSE                                                              MB2893
          SOUT(5)=SOUT(5)-SFLKB(I,J)                                      MB2894
        END IF                                                            MB2895
  200 CONTINUE                                                            MB2900
C                                                                         MB2910
C  COMPUTE CUMULATIVE FRESHWATER AND SALTWATER VOLUME CHANGES             MB2920
      DO 210 I=1,5                                                        MB2930
        CFIN(I,K)=CFIN(I,K)+FIN(I)*DT                                     MB2940
        CFOUT(I,K)=CFOUT(I,K)+FOUT(I)*DT                                  MB2950
        CSIN(I,K)=CSIN(I,K)+SSIN(I)*DT                                    MB2960
        CSOUT(I,K)=CSOUT(I,K)+SOUT(I)*DT                                  MB2970
 210  CONTINUE                                                            MB2980
      CFIN(6,K)=CFIN(6,K)+FIN(6)*DT                                       MB2981
      CFIN(7,K)=CFIN(7,K)+FIN(7)*DT                                       MB2982
      CSIN(6,K)=CSIN(6,K)+SSIN(6)*DT                                      MB2983
      CSIN(7,K)=CSIN(7,K)+SSIN(7)*DT                                      MB2984
C                                                                         MB2990
C  COMPUTE TOTALS FOR EACH AQUIFER                                        MB3000
      TFIN=FIN(1)+FIN(2)+FIN(3)+FIN(4)+FIN(5)+FIN(6)+FIN(7)               MB3010
      TCFIN=CFIN(1,K)+CFIN(2,K)+CFIN(3,K)+CFIN(4,K)+CFIN(5,K)+            MB3011
     &CFIN(6,K)+CFIN(7,K)                                                 MB3020
      TSSIN=SSIN(1)+SSIN(2)+SSIN(3)+SSIN(4)+SSIN(5)+SSIN(6)+SSIN(7)       MB3030
      TCSIN=CSIN(1,K)+CSIN(2,K)+CSIN(3,K)+CSIN(4,K)+CSIN(5,K)+            MB3031
     &CSIN(6,K)+CSIN(7,K)                                                 MB3040
      TFOUT=FOUT(2)+FOUT(3)+FOUT(4)+FOUT(5)                               MB3050
      TCFOUT=CFOUT(2,K)+CFOUT(3,K)+CFOUT(4,K)+CFOUT(5,K)                  MB3060
      TSOUT=SOUT(2)+SOUT(3)+SOUT(4)+SOUT(5)                               MB3070
      TCSOUT=CSOUT(2,K)+CSOUT(3,K)+CSOUT(4,K)+CSOUT(5,K)                  MB3080
C                                                                         MB3090
C  COMPUTE AQUIFER MASS BALANCE ERROR                                     MB3100
      FDIF=TFIN-TFOUT                                                     MB3110
      SDIF=TSSIN-TSOUT                                                    MB3120
      CFDIF=TCFIN-TCFOUT                                                  MB3130
      CSDIF=TCSIN-TCSOUT                                                  MB3140
      FERR=FSTOR-FDIF                                                     MB3150
      SERR=SSTOR-SDIF                                                     MB3160
      CFERR=CFSTR(K)-CFDIF                                                MB3170
      CSERR=CSSTR(K)-CSDIF                                                MB3180
      IF(TFIN.NE.0.)FRERF=FERR/TFIN*100.                                  MB3190
      IF(TSSIN.NE.0.)SRERF=SERR/TSSIN*100.                                MB3200
      IF(FSTOR.NE.0.)FRERS=FERR/FSTOR*100.                                MB3210
      IF(SSTOR.NE.0.)SRERS=SERR/SSTOR*100.                                MB3220
      IF(TCFIN.NE.0.)CFRERF=CFERR/TCFIN*100.                              MB3230
      IF(TCSIN.NE.0.)CSRERF=CSERR/TCSIN*100.                              MB3240
      IF(CFSTR(K).NE.0.)CFRERS=CFERR/CFSTR(K)*100.                        MB3250
      IF(CSSTR(K).NE.0.)CSRERS=CSERR/CSSTR(K)*100.                        MB3260
C                                                                         MB3270
      RETURN                                                              MB3280
      END                                                                 MB3290
