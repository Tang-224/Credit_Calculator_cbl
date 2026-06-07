       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREDIT_CALC.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT STD-FILE ASSIGN TO "STD.csv"
       ORGANIZATION IS LINE SEQUENTIAL.
       SELECT SCHYEAR-FILE ASSIGN TO "SCHOOLYEAR.csv"
       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD STD-FILE.
       01 STD-LINE PIC X(100).
       FD SCHYEAR-FILE.
       01 SCHYEAR-LINE PIC X(50).
       WORKING-STORAGE SECTION.
       01 READ-NUM PIC 9(2).
       01 GRADUATION-TABLE.
           05 ACADEMIC OCCURS 3 TIMES.
               10 ADMISSION-YEAR PIC 9(3).
               10 MIN-TOTAL1 PIC 9(3).
               10 MIN-ELEC1 PIC 9(2).
               10 MIN-GEN1 PIC 9(1).
       01 GRADUATION-NUM PIC 9(1).
       01 ENTER-YEAR PIC 9(3).
       01 MIN-TOTAL PIC 9(3).
       01 MIN-ELEC PIC 9(2).
       01 MIN-GEN PIC 9(1).
       01 TEMP-IN-YEAR PIC 9(3).
       01 TEMP-MIN-TOTAL PIC 9(3).
       01 TEMP-MIN-ELEC PIC 9(2).
       01 TEMP-MIN-GEN PIC 9(1).
       01 CREDIT-TOTAL PIC 9(3) VALUE 0.
       01 CREDIT-ELEC PIC 9(2) VALUE 0.
       01 CREDIT-GEN PIC 9(1) VALUE 0.
       01 DIFF-ANS PIC S9(3) VALUE 0.
       01 DIFF-CHK PIC X VALUE "N".
       01 DIFF-CREDIT-TOTAL PIC 9(3) VALUE 0.
       01 DIFF-CREDIT-ELEC PIC 9(2) VALUE 0.
       01 DIFF-CREDIT-GEN PIC 9(1) VALUE 0.
       01 STD-INFO.
         05 STD-YEAR PIC 9(3).
         05 STD-SEM PIC 9.
         05 STD-TRASH1 PIC X.
         05 STD-CNAME PIC X(40).
         05 STD-TRASH2 PIC X.
         05 STD-COURSE PIC X(6).
           88 REQUIRE VALUE "必修".
           88 ELECTIVE VALUE "選修".
           88 GENARAL VALUE "通識".
         05 STD-CREDIT PIC 9(1).
         05 STD-SCORE PIC 9(3).
         05 STD-TRASH3 PIC X.
       01 STD-EOF PIC X VALUE "N".
       01 YEAR-EOF PIC X VALUE "N".
       PROCEDURE DIVISION.
           MOVE 0 TO GRADUATION-NUM.
           OPEN INPUT SCHYEAR-FILE.
           PERFORM 100-YEAR UNTIL YEAR-EOF = "Y".
           CLOSE SCHYEAR-FILE.
           MOVE 1 TO GRADUATION-NUM.
           MOVE 0 TO READ-NUM.
           OPEN INPUT STD-FILE.
           PERFORM 200-READ-STD UNTIL STD-EOF = "Y".
           CLOSE STD-FILE.
           PERFORM 400-DIFFERENCE THRU 400-EXIT.
           DISPLAY "入學學年：" , ENTER-YEAR.
           DISPLAY "畢業所需最低 總學分：" , MIN-TOTAL , 
                   "、選修學分：" , MIN-ELEC , 
                   "、通識學分：" , MIN-GEN.
           IF DIFF-CHK = "Y"
               DISPLAY "未達成畢業門檻！！"
               DISPLAY "總學分差" , DIFF-CREDIT-TOTAL , "分"
               DISPLAY "選修學分差" , DIFF-CREDIT-ELEC , "分"
               DISPLAY "通識學分差" , DIFF-CREDIT-GEN , "分"
           ELSE
               DISPLAY "恭喜您已達成畢業門檻所需學分"
           END-IF.
       STOP RUN.
       100-YEAR.
           READ SCHYEAR-FILE NEXT RECORD
           AT END
               MOVE "Y" TO YEAR-EOF
           NOT AT END
               PERFORM 150-UNSTR THRU 150-EXIT
               IF GRADUATION-NUM > 0
                   MOVE TEMP-IN-YEAR TO ADMISSION-YEAR(GRADUATION-NUM)
                   MOVE TEMP-MIN-TOTAL TO MIN-TOTAL1(GRADUATION-NUM)
                   MOVE TEMP-MIN-ELEC TO MIN-ELEC1(GRADUATION-NUM)
                   MOVE TEMP-MIN-GEN TO MIN-GEN1(GRADUATION-NUM)
               END-IF
               ADD 1 TO GRADUATION-NUM
           END-READ.
       100-EXIT.
       EXIT.
       150-UNSTR.
           UNSTRING SCHYEAR-LINE 
           DELIMITED BY ","
           INTO TEMP-IN-YEAR , TEMP-MIN-TOTAL ,
                TEMP-MIN-ELEC , TEMP-MIN-GEN
           end-unstring.
       150-EXIT.
       EXIT.
       200-READ-STD.
           READ STD-FILE NEXT RECORD
           AT END
               MOVE "Y" TO STD-EOF
           NOT AT END
               PERFORM 250-UNSTR THRU 250-EXIT
               IF READ-NUM = 1
                   MOVE "N" TO YEAR-EOF
                   PERFORM 290-SEARCH UNTIL YEAR-EOF = "Y"
               END-IF
               ADD 1 TO READ-NUM
               IF STD-SCORE >= 60
                   PERFORM 300-CREDIT THRU 300-EXIT
               END-IF
           END-READ.
       200-EXIT.
       EXIT.
       250-UNSTR.
           UNSTRING STD-LINE 
               DELIMITED BY ","
               INTO STD-YEAR , STD-SEM , STD-TRASH1 , STD-CNAME ,
                    STD-TRASH2 , STD-COURSE , STD-CREDIT ,
                    STD-SCORE , STD-TRASH3
           end-unstring.
       250-EXIT.
       EXIT.
       290-SEARCH.
           IF GRADUATION-NUM > 3
               MOVE "Y" TO YEAR-EOF
           ELSE IF STD-YEAR = ADMISSION-YEAR(GRADUATION-NUM)
               MOVE ADMISSION-YEAR(GRADUATION-NUM) TO ENTER-YEAR
               MOVE MIN-TOTAL1(GRADUATION-NUM) TO MIN-TOTAL
               MOVE MIN-ELEC1(GRADUATION-NUM) TO MIN-ELEC
               MOVE MIN-GEN1(GRADUATION-NUM) TO MIN-GEN
               MOVE "Y" TO YEAR-EOF
           ELSE
               ADD 1 TO GRADUATION-NUM
           END-IF.
       290-EXIT.
       EXIT.
       300-CREDIT.
           IF REQUIRE
               COMPUTE CREDIT-TOTAL = CREDIT-TOTAL + STD-CREDIT.
           IF ELECTIVE
               COMPUTE CREDIT-TOTAL = CREDIT-TOTAL + STD-CREDIT
               COMPUTE CREDIT-ELEC = CREDIT-ELEC + STD-CREDIT.
           IF GENARAL
               COMPUTE CREDIT-TOTAL = CREDIT-TOTAL + STD-CREDIT
               COMPUTE CREDIT-GEN = CREDIT-GEN + STD-CREDIT.
       300-EXIT.
       EXIT.
       400-DIFFERENCE.
           COMPUTE DIFF-ANS = MIN-TOTAL - CREDIT-TOTAL.
           MOVE DIFF-ANS TO DIFF-CREDIT-TOTAL.
           IF DIFF-ANS > 0
               MOVE "Y" TO DIFF-CHK.
           COMPUTE DIFF-ANS = MIN-ELEC - CREDIT-ELEC.
           MOVE DIFF-ANS TO DIFF-CREDIT-ELEC.
           IF DIFF-ANS > 0
               MOVE "Y" TO DIFF-CHK.
           COMPUTE DIFF-ANS = MIN-GEN - CREDIT-GEN.
           MOVE DIFF-ANS TO DIFF-CREDIT-GEN.
           IF DIFF-ANS > 0
               MOVE "Y" TO DIFF-CHK.
       400-EXIT.
       EXIT.
       END PROGRAM CREDIT_CALC.
