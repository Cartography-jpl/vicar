      SUBROUTINE LAMCEN(PAR1,PAR2,CPSI,APSI,APHI,XARB,ZARB,XC,ZC)
      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      REAL*8 D,PHI1,PHI2,R1,R2,TAN1,TAN2,DK,DC,RP1,RP2
      REAL*8 EPS,E1,E2,TANA,RHO,EA,PHIA,NRAD,DPHI,GEODET
      LOGICAL*4 EFLAG
      CHARACTER*3600 LABI
      REAL*4 LIN1,LIN2
      EQUIVALENCE (DK,D(1)),(DC,D(2))
      DATA EFLAG/.FALSE./
      NRAD(X,Y,DPHI)=X*X/DSQRT(X*X*DCOS(DPHI)*DCOS(DPHI)+Y*Y*DSIN(DPHI)
     &*DSIN(DPHI))
80    PHI1=DBLE(PAR1)*DBLE(PI)/1.8D2
C     CHANGE YOUR LATITUDES TO GEODETIC FROM GEOCENTRIC
      PHI1=DABS(GEODET(PHI1))
      PHI2=DBLE(PAR2)*DBLE(PI)/1.8D2
      PHI2=DABS(GEODET(PHI2))
      CAS=SIGN(1.,PAR1)
      R1=NRAD(REQ,FL,PHI1)*DCOS(PHI1)
      R2=NRAD(REQ,FL,PHI2)*DCOS(PHI2)
      EPS=DSQRT(1.D0-FL*FL/REQ/REQ)
      E1=((1.D0+EPS*DSIN(PHI1))/(1.D0-EPS*DSIN(PHI1)))**(EPS/2.D0)
      TAN1=DTAN(PI/4.D0-PHI1/2.D0)*E1
      E2=((1.D0+EPS*DSIN(PHI2))/(1.D0-EPS*DSIN(PHI2)))**(EPS/2.D0)
      TAN2=DTAN(PI/4.D0-PHI2/2.D0)*E2
      DK=DLOG(R1/R2)/DLOG(TAN1/TAN2)
      IF(EFLAG)GO TO 81
      EFLAG=.TRUE.
      DC=R1/(DBLE(F)*DK*TAN1**DK)
      PHIA=DBLE(APHI)*DBLE(PI)/1.8D2
      PHIA=GEODET(PHIA)
      PHIA=PHIA*CAS
1000  EA=((1.D0+EPS*DSIN(PHIA))/(1.D0-EPS*DSIN(PHIA)))**(EPS/2.D0)
      TANA=DTAN(PI/4.D0-PHIA/2.D0)*EA
      RHO=DC*TANA**DK
C     FIND DLAM SO THAT -180.)DLAM)+180.
      DLAM=APSI-CPSI
      DLAM=AMOD(720.+DLAM,360.)
      IF(DLAM.GT.180.)DLAM=DLAM-360.
      DLAM=DLAM*PI/180.
      XC=XARB+RHO*DSIN(DK*DLAM)
      IF(XC.EQ.0.)GO TO 1200
      I=XC+SIGN(.5,XC)
      XC=I
1200  ZC=ZARB-CAS*RHO*DCOS(DK*DLAM)
      IF(ZC.EQ.0.)RETURN
      I=ZC+SIGN(.5,ZC)
      ZC=I
      RETURN
      ENTRY GETCK(PAR1,PAR2,LIN1,LIN2,ZPOL,NCASE)

C     NCASE DETERMINES WAY TO CALCULATE DC AND DK CONSTANTS FOR
C     TWO-STANDARD OTHOMORPHIC CONIC (LAMBERT CONFORMAL) PROJECTION
C     FOR NCASE=...      PARAMETERS GIVEN..  CONTROL PASSES TO...
C     1,2                F,CPHI              70
C     3,6                F,LIN1              10
C     4,7                F,LIN2              20
C     5,8                F,ZPOL              30
C     9                  LIN1,LIN2           40
C     10                 LIN1,ZPOL           50
C     11                 LIN2,ZPOL           60


C     PARAMETERS IN THIS SUBROUTINE ARE AS FOLLOWS...
C     PARI=LATITUDE OF PARALLEL I,I=1,2
C     LINI=PIXEL LINE LOCATION OF PARALLELI IN OUTPUT PIX,I=1,2
C     ZPOL=LINE VALUE OF POLE IN OUTPUT PIX
C     PHII=PARI IN RADIANS,I=1,2
C     RI=RADIUS OF PARI IN KM,I=1,2
C     TANI=TAN((90.-PARI)/2),I=1,2
C     DK=CONSTANT K FOR CONE...SEE SUPERMAP WRITE-UP...
C     DC=CONSTANT C FOR CONE
C     NOTE...DK DEPENDS ONLY ON PAR1 AND PAR2
C      BUT DC DEPENDS ON F.
C      LIN1,LIN2,AND ZPOL DEPEND ON DC AND F.IF ONE OF THE THREE IS
C      SPECIFIED,THE OTHERS CAN BE FOUND

C     NOTE...DK CAN ALWAYS BE CALCULATED AS FOLLOWS...

      IF(EFLAG)GO TO 81
      EFLAG=.TRUE.
      GO TO 80
81    GO TO (70,70,10,20,30,10,20,30,40,50,60), NCASE

C     IN THIS CASE, F (KM/PXL) IS SPECIFIED.TO FIND DC, WE NEED
C      RADIUS OF PAR1 IN PIXELS. WE CAN CALCULATE IT IN KM, THEREBY
C      GETTING RESULT BY USING F...

   10 DC=R1/(DBLE(F)*DK*TAN1**DK)
      RP1=R1/(DBLE(F)*DK)
      ZPOL=LIN1-CAS*SNGL(RP1)
      RP2=DC*TAN2**DK
      LIN2=ZPOL+CAS*SNGL(RP2)
      GO TO 100

C     IN THIS CASE,F AND LIN2 ARE SPECIFIED.
C     CALCULATE DC THE SAME AS ABOVE,THEN LIN1,ZPOL

   20 DC=R2/(DBLE(F)*DK*TAN2**DK)
      RP2=R2/(DBLE(F)*DK)
      ZPOL=LIN2-CAS*SNGL(RP2)
      RP1=DC*TAN1**DK
      LIN1=ZPOL+CAS*SNGL(RP1)
      GO TO 100

C     IN THIS CASE, F AND ZPOL ARE SPECIFIED. WE CAN AGAIN CALCULATE
C      DC FROM F, THEN LIN1,LIN2

   30 DC=R1/(DBLE(F)*DK*TAN1**DK)
      RP1=R1/(DBLE(F)*DK)
      RP2=DC*TAN2**DK
      LIN1=ZPOL+CAS*SNGL(RP1)
      LIN2=ZPOL+CAS*SNGL(RP2)
      GO TO 100

C     HERE,WE ARE GIVEN LIN1 AND LIN2.
C     CALCULATE DC FROM EQUATION...ABS(LIN1-LIN2)=RP2-RP1
C                                                =DC*(TAN1**DK-TAN2**DK)

   40 DC=DABS(DBLE(LIN2-LIN1)/(TAN2**DK-TAN1**DK))
      RP1=DC*TAN1**DK
      RP2=DC*TAN2**DK

C     R1 IS IN KM AT THIS POINT

      F=SNGL(R1/RP1*DK)
      ZPOL=LIN1-CAS*SNGL(RP1)
      GO TO 100

C     HERE,WE ARE GIVEN LIN1 AND ZPOL
C     RP1 IS THEREFORE IMMEDIATE

   50 RP1=DBLE(ABS(LIN1-ZPOL))
      DC=RP1/TAN1**DK
      RP2=DC*TAN2**DK
      LIN2=ZPOL+CAS*SNGL(RP2)
      F=SNGL(R1/RP1*DK)
      GO TO 100

C     HERE,LIN2 AND ZPOL ARE SPECIFIED...ANALOGOUS TO ABOVE

   60 RP2=DBLE(ABS(LIN2-ZPOL))
      DC=RP2/TAN2**DK
      LIN1=CAS*SNGL(RP1)+ZPOL
      F=SNGL(R2/RP2*DK)
      GO TO 100
C     THE FOLLOWING CODE IS THE OLD CRUDE CENTERING TECHNIQUE USED WHEN
C     NONE OF LIN1,LIN2,AND LINE WERE SPECIFIED
C     IT WILL BE EXECUTED NOW ONLY IF RECENTERING WAS ALSO UNSPECIFIED
   70 DC=R1/(DBLE(F)*DK*TAN1**DK)
      LIN1=LIN1*PI/180.
      LIN1=GEODET(DBLE(LIN1))
      LIN1=LIN1*CAS
      EA=((1.D0+EPS*SIN(LIN1))/(1.D0-EPS*SIN(LIN1)))**(EPS/2.D0)
      LIN2=TAN(PI/4.-LIN1/2.)*EA
      ZPOL=ZPOL-CAS*SNGL(DC)*LIN2**SNGL(DK)
      RP1=R1/(DBLE(F)*DK)
      RP2=DC*TAN2**DK
      LIN2=ZPOL+CAS*SNGL(RP2)
      LIN1=ZPOL+CAS*SNGL(RP1)
  100 CONTINUE
      RETURN
      END

c *********************************************************************
      SUBROUTINE VECTOR
      REAL R(3),RMAG,LAT,LONG
      REAL A(3),B(3),C(3)
      REAL D(3,3),E(3,3),F(3,3)
      ENTRY      SPHREC(R,RMAG,LAT,LONG)
C  CONVERT SPHERCAL COORDINATES TO RECTANGULAR VECTOR
      R(1)=RMAG*COS(LAT)*COS(LONG)
      R(2)=RMAG*COS(LAT)*SIN(LONG)
      R(3)=RMAG*SIN(LAT)
      RETURN
      ENTRY      RECSPH(R,RMAG,LAT,LONG)
C  CONVERT KECTANGULAR VECTOR COMPONENTS T SPHERICAL
      RMAG=SQRT(R(1)*R(1)+R(2)*R(2)+R(3)*R(3))
      LAT=ATAN2(R(3),SQRT(R(1)**2+R(2)**2))
      LONG=ATAN2(R(2),R(1))
      RETURN
      ENTRY      UNIT(R)
C  MAKE R A UNIT VECTOR IN SMAE DIRECTION
      X=SQRT(R(1)*R(1)+R(2)*R(2)+R(3)*R(3))
      DO 10 J=1,3
10    R(J)=R(J)/X
      RETURN
      ENTRY      CROSS(A,B,C)
C  VECTOR CROSS PRODUCT C=A X B
      C(1)=A(2)*B(3)-A(3)*B(2)
      C(2)=A(3)*B(1)-A(1)*B(3)
      C(3)=A(1)*B(2)-A(2)*B(1)
      RETURN
      ENTRY MMUL(D,E,F)
C  3 X 3 MATRIX PRODUCT F=D*E
      DO 20 I=1,3
      DO 15 J=1,3
      S=0.
      DO 12 K=1,3
12    S=S+D(I,K)*E(K,J)
      F(I,J)=S
15    CONTINUE
20    CONTINUE
      RETURN
      ENTRY VMUL(A,D,R)
C        VECTOR (3 X 1)=MATRIX (3 X 3) * VECTOR (3 X 1)  R=D*A
      DO 50 I=1,3
      S=0.
      DO 40 J=1,3
40    S=S+D(I,J)*A(J)
50    R(I)=S
      RETURN

      ENTRY mTRANS(D,E)
C        TRANSPOSE 3 X 3 MATRIX  E=TRANSPOSE(D)
      DO 60 I=1,3
      DO 60 J=1,3
   60 E(J,I)=D(I,J)
      RETURN
      END

c ***************************************************************
      REAL FUNCTION GEODET*8(PHI)
      REAL*8 PHI,PI/3.141592653589793/,D,APHI,GEOCEN
      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PIE,FLAG,LABI
      CHARACTER*3600 LABI

C     GIVEN GEOCENTRIC LATITUDE PHI IN RADIANS
C     COMPUTE GEODETIC LATITUDE IN RADIANS

      IF(DABS(PHI).GT.PI/2.D0-1.D-6)GO TO 1
      IF(PHI.EQ.0.D0)GO TO 1
      APHI=PI/2.D0-DABS(DATAN(-FL*FL/REQ/REQ/DTAN(PHI)))
      GEODET=APHI
      IF(PHI.LT.0.D0)GEODET=-APHI
      RETURN
1     GEODET=PHI
      RETURN

C     GIVEN GEODETIC LATITUDE PHI IN RADIANS
C     COMPUTE GEOCENTRIC LATITUDE

      ENTRY GEOCEN(PHI)
      IF(DABS(PHI).GT.PI/2.D0-1.D-6)GO TO 2
      APHI=DABS(PHI)
      APHI=DATAN(-FL*FL/REQ/REQ/DTAN(APHI-PI/2.D0))
      GEOCEN=APHI
      IF(PHI.LT.0.D0)GEOCEN=-APHI
      RETURN
2     GEOCEN=PHI
      RETURN
      END
