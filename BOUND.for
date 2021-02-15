C Файл BOUND.for
C  Программа разработана студентом группы М6О-309Б-18, Богатыревой Елизаветой,
C  Вариант 20
C Содержит подпрограмму BOUND
C       ПОДПPOГPAММА HAЛOЖEНИЯ ГPАНИЧНЫХ УCЛOBИЙ
C          BЫЗЫBAETCЯ ИЗ MAIN , BЫЗЫBAEMЫX MOДУЛEЙ HET.       
C ==================================================================
C ================== начало кода BOUND =============================
C  Закрепляются  по двум осям все узлы выреза, 
C  а также закрепляются узлы по оси X, лежащие на оси симметрии
C ==================================================================
      SUBROUTINE BOUND(NN,CORD,NBC,NFIX,DB,IPR,NP)
C 
C       ПPOГP. HAЛOЖ. ГPАН. УCЛOBИЙ,BЫЗЫBAETCЯ ИЗ MAIN1
C
       DIMENSION CORD(1),NBC(1),NFIX(1)
       CHARACTER*5 ZON
       LOGICAL*1 IPR(50)    
C CORD - одномерный массив глобальных координат узлов
C NBC - массив номеров закрепленных узлов 
C NFIX - массив признаков закреплений по осям       
C ZON - переменная, в которую записывается номер зоны (зон)    
C IPR - массив управления печатью  
    
       NB=0
       IX=0
       IXY=0
C NB - кол-во узлов, имеющих закрепление
C IX - кол-во узлов, закрепленных по оси X
C IXY - кол-во узлов, закрепленных по оси X и Y
C Переменную IY вводить не будем, так как нигде не встречаются узлы,
C закрепленные по оси Y
       
       WRITE(6,18)
       
C NP - общее количество узлов 
C I - номер рассматриваемого узла       
       DO 56 I=1,NP 
C Для исключения громоздкости произведем замену 
C X1 - координата текущей точки по X
C Y1 - координата текущей точки по Y
          X1 = CORD(2*(I-1)+1)
          Y1 = CORD(2*(I-1)+2)      
          
C Закрепления узлов по оси X, располагающихся на оси симметрии
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
       
C Закрепления узлов прямой части выреза по двум осям 
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
          
C Закрепления узлов левой дуги выреза по двум осям 
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
           
C Закрепления узлов правой дуги выреза по двум осям
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
  
      IF(IPR(21)) PRINT *,"КОЛИЧЕСТВО УЗЛОВ ЗАКРЕПЛЕННЫХ ПО ОСИ OX=",IX
      IF(IPR(21)) 
     >PRINT *,"КОЛИЧЕСТВО УЗЛОВ ЗАКРЕПЛЕННЫХ ПО ОСИ OX И OY=",IXY
      IF(IPR(21)) PRINT *,"ЧИСЛО ЗАКРЕПЛЕННЫХ УЗЛОВ NFIX=",NB  
  18  FORMAT('  ГPAHИЧHЫE УCЛOBИЯ'/' УЗEЛ    X      Y    
     >ЗАКР   ЗОНА')
  19  FORMAT(' ',I4,' ',F7.2,' ',F7.2,'  ',I2, '     ',A5)
      NN=NB
      RETURN
      END
C ==================================================================
C ================== конец кода BOUND  =============================
C ==================================================================
C конец файла BOUND.for