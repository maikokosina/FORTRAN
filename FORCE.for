C  ���� FORCE.for
C  ��������� ����������� ��������� ������ �6�-309�-18, ����������� ����������,
C  ������� 20
C �������� ������������ FORCE
C     �O��PO�PAMMA PA�HECEH�� HA�P��OK �� �����
C     B���BAETC� �� MAIN, ���������� ������� ���.
C ==================================================================
* IPR ������ ���������� �������. 
*CORD - ���������� ������ ���������� ��������� ����� (������ ��� �������� ����� ������� 
*�������� ������������ ���� 1 �� ��� � � Y ��������������, ������ ���� ��������� - ���� 2 � �.�.).
*R - ������ �������� ������ � ����� (���������� �� ��������� ������� CORD, ������ ������ ���������
* � ��� ����������� �������� �������� �� ��� � � Y � �����);
C NP - ����� �����. NR- ����� ����������� ����� 
C ==================================================================
C ================== ������ ���� FORCE =============================
C �� ���� �������� ��������, ����� ������ ��������� ����������� ��������
C �������������� q=1000�/��, ������������ �� ��������. �� ������ ������� 
C ��������� ������� ������������ �������� �� �������� 2000�/�� � �������
C ����� ��������� ������� �� �������� (-2000�/��) � ����� ���� ��������� �������.                                         
C ==================================================================
C ������������, �������������� ���� �� �������� ������������� �������� �����
      SUBROUTINE  FDUGI (NDF, CORD, IFORCE, RRA, XRAD, YRAD, NI, PRM, R)
C IFORCE - ������ �������� �� ��������������� �������     
C RRA - ������ ����������
C XRAD - ���������� ������ ���� �� X
C YRAD - ���������� ������ ���� �� Y
C NI - ���������� �����, � ������� �������������� ��������
C PRM - �������� ���������� ����������

       DIMENSION R(1), CORD(1), IFORCE(100)
      DO 402 I=1,NI
        J=IFORCE(I)
        S=0.
C ���������� ������� ����� 
            X1=CORD(2*(IFORCE(I)-1)+1)
            Y1=CORD(2*(IFORCE(I)-1)+2)
C ���������� ���������� �����    
        IF (I.NE.1) THEN 
            X0=CORD(2*(IFORCE(I-1)-1)+1)
            Y0=CORD(2*(IFORCE(I-1)-1)+2)
            ELSE
            X0=X1
            Y0=Y1
        ENDIF
C ���������� ��������� �����
        IF (I.NE.NI) THEN
            X2=CORD(2*(IFORCE(I+1)-1)+1)
            Y2=CORD(2*(IFORCE(I+1)-1)+2)
            ELSE
            X2=X1
            Y2=Y1
        ENDIF
        
C S=aR, SIN(a/2)=HORD/2R, ������ S=R*2*arcsin(HORD/2R) 
C  ��������� ����� ����� 
C ����� ����� ����� ��������� �� ���������� �� ��������� �����
C � ����� ������� �������    
         HORD=SQRT((X2-X0)**2+(Y2-Y0)**2)
C  ��������� ���� (� ��������)        
         UGOL = 2.*ASIN(HORD/(2.*RRA))
C  ��������� �������� ����� ����
        S1=RRA*UGOL/2.
        !PRINT*, '�������� ����� ���� � ��������',RRA,' � ����',I,' =',S1
        
C � ������� �������� S - ������ ��������, 
C ��� ����������� - ����� ����
C ������������ R1 �������� ��������� (����, ����������� � ����).
        R1=S1*1000
       !PRINT*, 'R1=', R1
C ������� � ������ R ������������ ��������, ����������� �� X � Y
C ���� ������������� ����������� ����, �� ������������ ��������������
C ������������ ��������
      IF (ABS(PRM-1).LT.0.01) THEN
      IF (ABS(RRA-45.).LT.0.01) THEN
        R(NDF*(IFORCE(I)-1)+1)=R(NDF*(IFORCE(I)-1)+1)-R1*(X1-XRAD)/
     >(SQRT((X1-XRAD)**2+(Y1-YRAD)**2))
        R(NDF*(IFORCE(I)-1)+2)=R(NDF*(IFORCE(I)-1)+2)-R1*(Y1-YRAD)/
     >(SQRT((X1-XRAD)**2+(Y1-YRAD)**2))
        ELSE
        R(NDF*(IFORCE(I)-1)+1)=R(NDF*(IFORCE(I)-1)+1)+R1*(X1-XRAD)/
     >(SQRT((X1-XRAD)**2+(Y1-YRAD)**2))
        R(NDF*(IFORCE(I)-1)+2)=R(NDF*(IFORCE(I)-1)+2)+R1*(Y1-YRAD)/
     >(SQRT((X1-XRAD)**2+(Y1-YRAD)**2))  
      ENDIF   
      ENDIF
 402   CONTINUE
      RETURN
      END

      SUBROUTINE FORCE(NB,CORD,NRC,DB,IPR,NP,RSUM,R,NDF,DH,NR,
     >PRM1,PRM2,PRM3,PRM4,PRM5,PRM6)
C  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  �O��PO�PAMMA PA�HECEH�� HA�P��OK, B���BAETC� �� ++MAIN+
C  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       LOGICAL*1 IPR(50)
       DIMENSION CORD(1),R(1), IFORCE1(100), IFORCE2(100), IFORCE3(100)
       DIMENSION IFORCE4(100), IFORCE5(100)
C IFORCE1 - ������ ����� ����� �������, � ������� ��������� ���� q1=1000 �/��
C IFORCE2 - ������ ����� ����� ����
C IFORCE3 - ������ ����� ����������� ����
C IFORCE4 - ������ ����� ������ ����
C IFORCE5 - ������ ����� ��������� � ��������� q2 �� 2000 �� -2000 �/��

C ��������-�������, ����������� ���������� ����� ����� �������
      DLINA (A1, B1, A2, B2)=SQRT(((A1-A2)**2)+((B1-B2)**2))

C       ��� ������ ������������ ������ �����������
       IF(IPR(22)) WRITE(6,22)PRM1,PRM2,PRM3,PRM4,PRM5,PRM6
       IF(IPR(23)) WRITE(6,20)
       
C �������� ����      
       DO 100 I=1,NP
         R(NDF*(I-1)+1)=0
         R(NDF*(I-1)+2)=0
 100   CONTINUE 

C �������� ��������� 
       NR=0
       I1=0
       I2=0
       I3=0
       I4=0
       I5=0
       
        DO 56 I=1,NP
        
C ��� �������� ������ ���������� XT � YT  
         XT=CORD(NDF*(I-1)+1)
         YT=CORD(NDF*(I-1)+2)
      
C ����� �����, ������������� ����� ������������ �������         
          IF (ABS(PRM1-1).LT.0.01) THEN
             IF ((XT-9.79).LT.0.01) THEN
                IF (((YT-39.79).LT.0.01) 
     >.AND. ((YT-9.79).GT.-0.01)) THEN
                      NR=NR+1
                      I1=I1+1
                      IFORCE1(I1)=I
                ENDIF
             ENDIF
          ENDIF  

C ����� �����, ������������� ����� ������� ����        
          IF (ABS(PRM2-1).LT.0.01) THEN
             IF ((ABS(SQRT((XT-25.79)**2+(YT-39.79)**2)-16.).LT.0.6) 
     >.AND. ((XT-9.79).GT.-0.01) .AND. ((XT-35.91).LT.0.1) 
     >.AND. ((YT-39.79).GT.-0.01) .AND. ((YT-60.).LT.0.01)) THEN
                NR=NR+1
                I2=I2+1
                IFORCE2(I2)=I
             ENDIF
          ENDIF
          
C ����� �����, ������������� ����������� ����
          IF (ABS(PRM3-1).LT.0.01) THEN
             IF ((ABS(SQRT((XT-64.896)**2+(YT-87.146)**2)-45.).LT.0.5)
     >.AND. ((XT-35.43).GT.-0.01) .AND. ((XT-86.41).LT.0.01)) THEN
                NR=NR+1
                I3=I3+1
                IFORCE3(I3)=I
             ENDIF
          ENDIF
          
C ����� �����, ������������� ������ ������� ����
          IF (ABS(PRM4-1).LT.0.01) THEN
             IF ((ABS(SQRT((XT-94.703)**2+(YT-33.939)**2)-16.).LT.0.5)
     >.AND. ((XT-86.41).GT.-0.01) .AND. ((XT-108.48).LT.0.01) 
     >.AND. ((YT-42.).GT.-0.01) .AND. ((YT-50.).LT.0.01)) THEN
                NR=NR+1
                I4=I4+1
                IFORCE4(I4)=I
             ENDIF
          ENDIF
          
C ����� �����, ������������� ��������� ������� ��� ������������� ��������
          IF ((ABS(PRM5-1).LT.0.01) .OR. (ABS(PRM5-3).LT.0.01)) THEN
             IF ((ABS(16.13*XT+8.65*YT-2113.6014).LT.5.)
     >.AND. ((XT-117.13).LT.0.01)) THEN 
                NR=NR+1
                I5=I5+1
                IFORCE5(I5)=I
             ENDIF
          ENDIF   
          
C ����� �����, ������������� ��������� ������� ��� ������������� ��������
C ��������� ������ ����� ��� 16.13*X+8.65*Y-2113.6014 = 0
          IF ((ABS(PRM5-2).LT.0.01) .OR. (ABS(PRM5-3).LT.0.01)) THEN
             IF ((ABS(16.13*XT+8.65*YT-2113.6014).LT.5.)
     >.AND. ((XT-117.13).GT.0.)) THEN
                NR=NR+1
                I5=I5+1
                IFORCE5(I5)=I
             ENDIF
          ENDIF

C          
          IF (ABS(PRM5-2).LT.0.01) THEN
           IF ((ABS(16.13*XT+8.65*YT-2113.6014).LT.5.)
     >.AND. (ABS(XT-117.13).LT.0.01)) THEN
                NR=NR+1
                I5=I5+1
                IFORCE5(I5)=I
             ENDIF
          ENDIF
                    
  56    CONTINUE 
  
C ========================================================================  
C ��� �, ���� �� �����, �������� ��������� � ��� ��������. 
C ��� ������ ����������� ���������� �������

C ������ IFORCE1 ����������� �� �������� ���������� Y
      DO 301 I=I1,2,-1
        DO 302 J=1,I-1
            IF (CORD(NDF*(IFORCE1(J+1)-1)+2).GT.
     >CORD(NDF*(IFORCE1(J)-1)+2)) THEN
                ITMP=IFORCE1(J+1)
                IFORCE1(J+1)=IFORCE1(J)
                IFORCE1(J)=ITMP
            ENDIF
  302   CONTINUE
  301 CONTINUE
  
C ������ IFORCE2 ����������� �� ����������� ���������� X
      DO 303 I=I2,2,-1
        DO 304 J=1,I-1
            IF (CORD(NDF*(IFORCE2(J)-1)+1).GT.
     >CORD(NDF*(IFORCE2(J+1)-1)+1)) THEN
                ITMP=IFORCE2(J+1)
                IFORCE2(J+1)=IFORCE2(J)
                IFORCE2(J)=ITMP
            ENDIF
  304   CONTINUE
  303 CONTINUE 
  
C ������ IFORCE3 ����������� �� ����������� ���������� X
      DO 305 I=I3,2,-1
        DO 306 J=1,I-1
            IF (CORD(NDF*(IFORCE3(J)-1)+1).GT.
     >CORD(NDF*(IFORCE3(J+1)-1)+1)) THEN
                ITMP=IFORCE3(J+1)
                IFORCE3(J+1)=IFORCE3(J)
                IFORCE3(J)=ITMP
            ENDIF
  306   CONTINUE
  305 CONTINUE 
  
C ������ IFORCE4 ����������� �� ����������� ���������� X
      DO 307 I=I4,2,-1
        DO 308 J=1,I-1
            IF (CORD(NDF*(IFORCE4(J)-1)+1).GT.
     >CORD(NDF*(IFORCE4(J+1)-1)+1)) THEN
                ITMP=IFORCE4(J+1)
                IFORCE4(J+1)=IFORCE4(J)
                IFORCE4(J)=ITMP
            ENDIF
  308   CONTINUE
  307 CONTINUE 
  
C ������ IFORCE5 ����������� �� ����������� ���������� X
      DO 309 I=I5,2,-1
        DO 310 J=1,I-1
            IF (CORD(NDF*(IFORCE5(J)-1)+1).GT.
     >CORD(NDF*(IFORCE5(J+1)-1)+1)) THEN
                ITMP=IFORCE5(J+1)
                IFORCE5(J+1)=IFORCE5(J)
                IFORCE5(J)=ITMP
            ENDIF
  310   CONTINUE
  309 CONTINUE
  
C =======================================================================       
C ��� ������ ����� �������� ���� �� ��������
C ����������� ����, ����������� � �������� ���� (����������� ����� ������� ��������)
C S - ������ ������� ����������� ����� ������
C R1 - �������������� ���� ����������� � �������� ����

C ������� ��� � ����� ����� ������� (������ IFORCE1)
       DO 401 I=1,I1
         J=IFORCE1(I)
C ���������� ������� ����� 
            X1=CORD(2*(IFORCE1(I)-1)+1)
            Y1=CORD(2*(IFORCE1(I)-1)+2)
C ���������� ���������� �����    
        IF (I.NE.1) THEN 
            X0=CORD(2*(IFORCE1(I-1)-1)+1)
            Y0=CORD(2*(IFORCE1(I-1)-1)+2)
            ELSE
            X0=X1
            Y0=Y1
        ENDIF
C ���������� ��������� �����
        IF (I.NE.I1) THEN
            X2=CORD(2*(IFORCE1(I+1)-1)+1)
            Y2=CORD(2*(IFORCE1(I+1)-1)+2)
            ELSE
            X2=X1
            Y2=Y1
        ENDIF
         
      S=DLINA (X0, Y0, X2, Y2)
         !PRINT*, '����� �������= ', S
      R1=S*1000
      R((J-1)*NDF+1)=R((J-1)*NDF+1)-R1
  401  CONTINUE 
  
C========================================================================  
C ������� ��� � ����� �� ����� ���� (������ IFORCE2) 
      CALL  FDUGI (NDF, CORD, IFORCE2, 16., 25.79, 39.79, I2, PRM2, R)
      
C ������� ��� � ����� �� ����������� ���� (������ IFORCE3) 
      CALL  FDUGI (NDF, CORD, IFORCE3, 45., 64.35, 87.06, I3, PRM3, R) 
      
C ������� ��� � ����� �� ������ ���� (������ IFORCE4) 
      CALL  FDUGI (NDF, CORD, IFORCE4, 16., 94.703, 33.939, I4, PRM4, R)
      
C ========================================================================      
C ������� ��� � ����� ������ ��������� ������� 
      DO 404 I=1,I5
        J=IFORCE5(I)
        J1=IFORCE5(I+1)-1
C ���������� ��������� ����� ���������       
        X00=108.48
        Y00=42.06 
C  ���������� ������� �����
        X1=CORD(2*(IFORCE5(I)-1)+1)
        Y1=CORD(2*(IFORCE5(I)-1)+2)
C ���������� ���������� �����    
        IF (I.NE.1) THEN 
            X0=CORD(2*(IFORCE5(I-1)-1)+1)
            Y0=CORD(2*(IFORCE5(I-1)-1)+2)
            ELSE
            X0=X1
            Y0=Y1
        ENDIF
C ���������� ��������� �����
        IF (I.NE.I5) THEN
            X2=CORD(2*(IFORCE5(I+1)-1)+1)
            Y2=CORD(2*(IFORCE5(I+1)-1)+2)
            ELSE
            X2=X1
            Y2=Y1
        ENDIF                            
        
        !PRINT*, 'X0=', X0, 'X1=', X1, 'X2=', X2
        !PRINT*, 'Y0=', Y0, 'Y1=', Y1, 'Y2=', Y2
C ������� ���������� �� �������� ���� �� ���������, ���������� � ��������� �������     
      S=DLINA(X00, Y00, X1, Y1)
      S0=DLINA(X1, Y1, X0, Y0)
      S1=DLINA(X1, Y1, X2, Y2)
      !PRINT*, '���������� �� ������� ����� �� �������=', S
      !PRINT*, '���������� �� ������� �� ����������', S0
      !PRINT*, '���������� �� ������� �� ���������', S1 
      
      PREDNIZ=S-(S0/2.)
      PREDVERH=S+(S1/2.) 
      !PRINT*, '������ ������=', PREDNIZ, '   ������� ������=', PREDVERH
      !PRINT*
      
C  q2 = -109.2299x+2000,
C ��� x - ����� ���������, ������� ���������� �� 0 �� 36.62 
         R1=((-109.23*(PREDVERH**2)/2.)+2000*PREDVERH+
     >(109.23*(PREDNIZ**2)/2.)-2000*PREDNIZ)

C ������������ ��������, ������� �� ����� ���� ��� �������� �� X
C � �� ������� ���� ��� �������� �� Y
C ������� ���������� ����� ������� (���� ��� ��������� ���������)
C ����� ���������� ����� �������
C ��������� ������ ������� ����� ���: Y=-1.8647*X+244.34698 
      IF ((CORD(2*(IFORCE5(I)-1)+1)-125.79).LT.0.) THEN
        R(NDF*(IFORCE5(I)-1)+1)=R(NDF*(IFORCE5(I)-1)+1)+R1
     >*(SQRT(1-(1/(1+(-1.8647)**2))))
        R(NDF*(IFORCE5(I)-1)+2)=R(NDF*(IFORCE5(I)-1)+2)+R1
     >*(SQRT(1/(1+(-1.8647)**2)))
      ELSE
C ������� �������� ��� ������ ����� 
       R2=(R1-R1*SQRT((1/(1+(-1.8647)**2))))
      R(NDF*(IFORCE5(I)-1)+1)=R(NDF*(IFORCE5(I)-1)+1)
     >+R1*SQRT((1/(1+(-1.8647)**2)))      
      R(NDF*(J1)+2)=R(NDF*(J1)+2)
     >+R2*SQRT(1-(1/(1+(-1.8647)**2)))
      R(NDF*(IFORCE5(I)-1)+1)=R(NDF*(IFORCE5(I)-1)+1)
     >+R2*SQRT(1-(1/(1+(-1.8647)**2)))
      ENDIF        
  404 CONTINUE             
           
C ������� ����������� ����, �� ����������
        IF(IPR(23)) THEN
         IF((ABS(PRM1-1).LT.0.01)) THEN 
         DO 420 I=1,I1
          WRITE(6,21) I,IFORCE1(I),
     >CORD(2*(IFORCE1(I)-1)+1), CORD(2*(IFORCE1(I)-1)+2),
     >R((IFORCE1(I)-1)*NDF+1),R((IFORCE1(I)-1)*NDF+2)
  420 CONTINUE
         ENDIF
         
         IF((ABS(PRM2-1).LT.0.01)) THEN
         DO 421 I=1,I2
          WRITE(6,21) I+I1, IFORCE2(I), 
     >CORD(NDF*(IFORCE2(I)-1)+1),CORD(NDF*(IFORCE2(I)-1)+2),
     >R((IFORCE2(I)-1)*NDF+1),R((IFORCE2(I)-1)*NDF+2)
  421 CONTINUE 
         ENDIF
         
         IF((ABS(PRM3-1).LT.0.01)) THEN
         DO 422 I=1,I3
          WRITE(6,21) I+I1+I2, IFORCE3(I), 
     >CORD(NDF*(IFORCE3(I)-1)+1), CORD(NDF*(IFORCE3(I)-1)+2),
     >R((IFORCE3(I)-1)*NDF+1), R((IFORCE3(I)-1)*NDF+2)
  422 CONTINUE   
         ENDIF
         
         
         IF((ABS(PRM4-1).LT.0.01)) THEN
         DO 423 I=1,I4
          WRITE(6,21) I+I1+I2+I3, IFORCE4(I), 
     >CORD(NDF*(IFORCE4(I)-1)+1), CORD(NDF*(IFORCE4(I)-1)+2),
     >R((IFORCE4(I)-1)*NDF+1), R((IFORCE4(I)-1)*NDF+2)
  423 CONTINUE 
       ENDIF
       
         IF (PRM5.GT.0.) THEN
         DO 424 I=1,I5
          WRITE(6,21) NR-I5+I, IFORCE5(I), 
     >CORD(NDF*(IFORCE5(I)-1)+1), CORD(NDF*(IFORCE5(I)-1)+2),
     >R((IFORCE5(I)-1)*NDF+1),R((IFORCE5(I)-1)*NDF+2)
  424 CONTINUE  
        ENDIF  
      ENDIF
  
      
C       ����������� ����� �����������
        IF(IPR(23)) PRINT *,"����� ����������� ����� NR=",NR
C     ------------------------------------------------------------   
C      ����������� ����� ���� ��� �������� ������ ���������
C       ����������� ������������� �������� ���� R666
       R666=-2000.
       DO 101 I=1,NP
          IF (R(NDF*(I-1)+1).GT.R666) R666 = R(NDF*(I-1)+1) 
          IF (R(NDF*(I-1)+2).GT.R666) R666 = R(NDF*(I-1)+2)
 101   CONTINUE 
        
        IF (R666.GT.-2000.) GOTO 122 

        IF(IPR(23)) PRINT *,"�������� ������������ ���� R666=",R666
C  � ������, ����� �� ��������� ���� �� � ������ �� �����, �����������
C  ����������� ����������� ������� �� ���������� � ��������� ���������:
C  "���� ����������� �������� ������: �� ��������� �������� ����������� ���"
C  ������� ������ ��� ������  ������������ �����������, � ���� 1 �������������� 
C  ���� ��������� 666, ������������ ����� ��� � (��������� ����).
        R(1)=666.

        WRITE(6,6) (I,(R((I-1)*NDF+J),J=1,NDF),I=1,NP)
   6    FORMAT ('   HE ��������� ���� �� � ������ �� ����� '/ 
     >  '� 1-�� �������� ������� R �������� ��������� ���� 666 '/
     >  4(I4,2G10.4))
C      -----------------------------------------------------------------     
  20     FORMAT (' PAC�PE�E�EH�E HA�P��OK'/ '     N    ��E�   X-COORD',
     >        '    Y-COORD       * RX *     * RY *')
  21     FORMAT  (2I6,1X,2F10.3,4X,2F9.2)     
  22     FORMAT (10X,'��������� ��������� (�������� �  FORCE)'
     > /6X,'PRM1',6X,'PRM2',
     > 6X,'PRM3',6X,'PRM4',6X,'PRM5',6X,'PRM6'/(2X,6F10.4))
 122      RETURN
      END 
C ==================================================================
C =================== ����� ���� FORCE  ===================
C ==================================================================
C ����� ����� FORCE.for
