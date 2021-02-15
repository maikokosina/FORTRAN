C ���� BOUND.for
C  ��������� ����������� ��������� ������ �6�-309�-18, ����������� ����������,
C  ������� 20
C �������� ������������ BOUND
C       ����PO�PA��� HA�O�E��� �P������� �C�OB��
C          B���BAETC� �� MAIN , B���BAEM�X MO���E� HET.       
C ==================================================================
C ================== ������ ���� BOUND =============================
C  ������������  �� ���� ���� ��� ���� ������, 
C  � ����� ������������ ���� �� ��� X, ������� �� ��� ���������
C ==================================================================
      SUBROUTINE BOUND(NN,CORD,NBC,NFIX,DB,IPR,NP)
C 
C       �PO�P. HA�O�. �P��. �C�OB��,B���BAETC� �� MAIN1
C
       DIMENSION CORD(1),NBC(1),NFIX(1)
       CHARACTER*5 ZON
       LOGICAL*1 IPR(50)    
C CORD - ���������� ������ ���������� ��������� �����
C NBC - ������ ������� ������������ ����� 
C NFIX - ������ ��������� ����������� �� ����       
C ZON - ����������, � ������� ������������ ����� ���� (���)    
C IPR - ������ ���������� �������  
    
       NB=0
       IX=0
       IXY=0
C NB - ���-�� �����, ������� �����������
C IX - ���-�� �����, ������������ �� ��� X
C IXY - ���-�� �����, ������������ �� ��� X � Y
C ���������� IY ������� �� �����, ��� ��� ����� �� ����������� ����,
C ������������ �� ��� Y
       
       WRITE(6,18)
       
C NP - ����� ���������� ����� 
C I - ����� ���������������� ����       
       DO 56 I=1,NP 
C ��� ���������� ������������ ���������� ������ 
C X1 - ���������� ������� ����� �� X
C Y1 - ���������� ������� ����� �� Y
          X1 = CORD(2*(I-1)+1)
          Y1 = CORD(2*(I-1)+2)      
          
C ����������� ����� �� ��� X, ��������������� �� ��� ���������
       IF (((X1.LT.28.79) .AND. (X1.GT.9.78)) .OR.
     >((X1.GT.106.80) .AND. (X1.LT.125.80))) THEN
             IF ((Y1-9.79).LT.0.01) THEN
               IX=IX+1
               NB=NB+1
               NBC(NB)=I
               NFIX(NB)=1
               IF ((X1-28.79).LT.0.) THEN
                  ZON='7'
               ENDIF
               IF ((X1-106.79).GT.0.01) THEN
                  ZON='1'
               ENDIF
               IF(IPR(21)) THEN
                  WRITE(6,19) I,X1,Y1,NFIX(NB),ZON
               ENDIF 
             ENDIF
       ENDIF
       
C ����������� ����� ������ ����� ������ �� ���� ���� 
          IF ((X1.GT.42.80) .AND. (X1.LT.92.78) .AND. 
     >(Y1.LT.23.80))  THEN
                   IXY=IXY+1
                   NB=NB+1
                   NBC(NB)=I
                   NFIX(NB)=11
                   IF ((X1.GT.42.80) .AND. (X1.LT.55.28)) THEN
                      ZON='5'
                   ENDIF
                   IF (ABS(X1-55.29).LT.0.1) THEN
                      ZON='4 5'
                   ENDIF
                   IF ((X1.GT.55.30) .AND. (X1.LT.80.28)) THEN
                      ZON='4'
                   ENDIF
                   IF (ABS(X1-80.29).LT.0.01) THEN
                      ZON='3 4'
                   ENDIF
                   IF (X1.GT.80.30) THEN
                      ZON='3'
                   ENDIF
                   IF(IPR(21)) THEN
                   WRITE(6,19) I,X1,Y1,NFIX(NB),ZON
                   ENDIF
          ENDIF
          
C ����������� ����� ����� ���� ������ �� ���� ���� 
           IF ((ABS(SQRT((X1-42.79)**2+(Y1-9.79)**2)-14.).LT.0.05) .AND. 
     >(X1.LT.42.80)) THEN
              IXY=IXY+1
              NB=NB+1
              NBC(NB)=I
              NFIX(NB)=11
              IF (X1.LT.32.88) THEN
                  ZON='7'
               ENDIF
               IF (ABS(X1-32.89).LT.0.01) THEN
                  ZON='6 7'
               ENDIF
               IF ((X1.GT.32.90) .AND. (X1.LT.42.78)) THEN
                  ZON='6'
               ENDIF   
               IF (ABS(X1-42.79).LT.0.1) THEN
                  ZON='5 6'
               ENDIF
              IF(IPR(21)) THEN
              WRITE(6,19) I,X1,Y1,NFIX(NB),ZON
              ENDIF
           ENDIF  
           
C ����������� ����� ������ ���� ������ �� ���� ����
           IF ((ABS(SQRT((X1-92.79)**2+(Y1-9.79)**2)-14.).LT.0.05) .AND. 
     >(X1.GT.92.78)) THEN
              IXY=IXY+1
              NB=NB+1
              NBC(NB)=I
              NFIX(NB)=11
              IF (ABS(X1-92.79).LT.0.01) THEN
                 ZON='2 3'
              ENDIF
              IF ((X1.GT.92.80) .AND. (X1.LT.102.68)) THEN
                 ZON='2'
              ENDIF
              IF (ABS(X1-102.69).LT.0.01) THEN
                 ZON='1 2'
              ENDIF
              IF ((X1-102.70).GT.0.) THEN
                 ZON='1'
              ENDIF
              IF(IPR(21)) THEN
              WRITE(6,19) I,X1,Y1,NFIX(NB),ZON
              ENDIF
           ENDIF    
                 
  56   CONTINUE
  
      IF(IPR(21)) PRINT *,"���������� ����� ������������ �� ��� OX=",IX
      IF(IPR(21)) 
     >PRINT *,"���������� ����� ������������ �� ��� OX � OY=",IXY
      IF(IPR(21)) PRINT *,"����� ������������ ����� NFIX=",NB  
  18  FORMAT('  �PAH��H�E �C�OB��'/' ��E�    X      Y    
     >����   ����')
  19  FORMAT(' ',I4,' ',F7.2,' ',F7.2,'  ',I2, '     ',A5)
      NN=NB
      RETURN
      END
C ==================================================================
C ================== ����� ���� BOUND  =============================
C ==================================================================
C ����� ����� BOUND.for