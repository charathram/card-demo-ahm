      ******************************************************************
      * Program     : COCRDDLC.CBL
      * Application : CardDemo
      * Type        : CICS COBOL Program
      * Function    : Soft-delete a credit card in CARDDAT file
      *               (marks CARD-ACTIVE-STATUS = 'D')
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.
      * All Rights Reserved.
      *
      * Licensed under the Apache License, Version 2.0 (the "License").
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at
      *
      *    http://www.apache.org/licenses/LICENSE-2.0
      *
      * Unless required by applicable law or agreed to in writing,
      * software distributed under the License is distributed on an
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
      * either express or implied. See the License for the specific
      * language governing permissions and limitations under the License
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COCRDDLC.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COCRDDLC'.
         05 WS-TRANID                  PIC X(04) VALUE 'CCDD'.
         05 WS-MAPSET                  PIC X(07) VALUE 'COCRDDL'.
         05 WS-MAPNAME                 PIC X(07) VALUE 'CCRDDLA'.
         05 WS-CARDFILE                PIC X(08) VALUE 'CARDDAT '.
         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-CARD-FETCHED            PIC X(01) VALUE 'N'.
           88 CARD-FETCHED-YES                   VALUE 'Y'.
           88 CARD-FETCHED-NO                    VALUE 'N'.
         05 WS-ACCT-INPUT              PIC X(11) VALUE SPACES.
         05 WS-ACCT-INPUT-N REDEFINES WS-ACCT-INPUT
                                       PIC 9(11).
         05 WS-CARD-INPUT              PIC X(16) VALUE SPACES.
         05 WS-CARD-INPUT-N REDEFINES WS-CARD-INPUT
                                      PIC 9(16).
         05 WS-ACCT-NUMERIC-FLG        PIC X(01) VALUE 'N'.
           88 WS-ACCT-NUMERIC                    VALUE 'Y'.
         05 WS-CARD-NUMERIC-FLG        PIC X(01) VALUE 'N'.
           88 WS-CARD-NUMERIC                    VALUE 'Y'.
         05 WS-EXPMON-DISP             PIC X(02) VALUE SPACES.
         05 WS-EXPYEAR-DISP            PIC X(04) VALUE SPACES.
         05 WS-SOFT-DELETED-MARKER     PIC X(01) VALUE 'D'.

       COPY COCOM01Y.

       COPY COCRDDL.

       COPY COTTL01Y.
       COPY CSDAT01Y.
       COPY CSMSG01Y.
       COPY CSUSR01Y.

      *CARD RECORD LAYOUT
       COPY CVACT02Y.

       COPY DFHAID.
       COPY DFHBMSCA.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  DFHCOMMAREA.
         05  LK-COMMAREA                           PIC X(01)
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       MAIN-PARA.

           SET ERR-FLG-OFF     TO TRUE

           MOVE SPACES TO WS-MESSAGE
                          ERRMSGO OF CCRDDLAO
                          INFOMSGO OF CCRDDLAO

           IF EIBCALEN = 0
               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA
               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER    TO TRUE
                   MOVE LOW-VALUES          TO CCRDDLAO
                   SET CARD-FETCHED-NO      TO TRUE
                   IF CDEMO-ACCT-ID NOT = ZEROES
                       MOVE CDEMO-ACCT-ID      TO ACCTSIDI OF
                                                     CCRDDLAI
                   END-IF
                   IF CDEMO-CARD-NUM NOT = ZEROES
                       MOVE CDEMO-CARD-NUM     TO CARDSIDI OF
                                                     CCRDDLAI
                   END-IF
                   IF  CDEMO-ACCT-ID  NOT = ZEROES
                   AND CDEMO-CARD-NUM NOT = ZEROES
                       PERFORM FETCH-CARD-DETAILS
                   ELSE
                       MOVE -1 TO ACCTSIDL OF CCRDDLAI
                       MOVE 'Enter Account and Card Number to fetch'
                         TO WS-MESSAGE
                       PERFORM SEND-CARDDEL-SCREEN
                   END-IF
               ELSE
                   PERFORM RECEIVE-CARDDEL-SCREEN
                   EVALUATE EIBAID
                       WHEN DFHENTER
                           PERFORM PROCESS-ENTER-KEY
                       WHEN DFHPF3
                           IF CDEMO-FROM-PROGRAM = SPACES OR LOW-VALUES
                               MOVE 'COMEN01C' TO CDEMO-TO-PROGRAM
                           ELSE
                               MOVE CDEMO-FROM-PROGRAM TO
                                    CDEMO-TO-PROGRAM
                           END-IF
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN DFHPF4
                           PERFORM CLEAR-CURRENT-SCREEN
                       WHEN DFHPF5
                           PERFORM DELETE-CARD-INFO
                       WHEN DFHPF12
                           MOVE 'COMEN01C' TO CDEMO-TO-PROGRAM
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN OTHER
                           MOVE 'Y'                   TO WS-ERR-FLG
                           MOVE CCDA-MSG-INVALID-KEY  TO WS-MESSAGE
                           PERFORM SEND-CARDDEL-SCREEN
                   END-EVALUATE
               END-IF
           END-IF

           EXEC CICS RETURN
                     TRANSID (WS-TRANID)
                     COMMAREA (CARDDEMO-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      PROCESS-ENTER-KEY
      *----------------------------------------------------------------*
       PROCESS-ENTER-KEY.

           PERFORM VALIDATE-INPUT-KEYS

           IF NOT ERR-FLG-ON
               PERFORM FETCH-CARD-DETAILS
           END-IF.

      *----------------------------------------------------------------*
      *                      DELETE-CARD-INFO
      *----------------------------------------------------------------*
       DELETE-CARD-INFO.

           PERFORM VALIDATE-INPUT-KEYS

           IF NOT ERR-FLG-ON
               PERFORM FETCH-CARD-DETAILS
           END-IF

           IF NOT ERR-FLG-ON AND CARD-FETCHED-YES
               PERFORM SOFT-DELETE-CARD-IN-FILE
           END-IF.

      *----------------------------------------------------------------*
      *                      VALIDATE-INPUT-KEYS
      *----------------------------------------------------------------*
       VALIDATE-INPUT-KEYS.

           SET CARD-FETCHED-NO TO TRUE

           EVALUATE TRUE
               WHEN ACCTSIDI OF CCRDDLAI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Account number can NOT be empty...'
                     TO WS-MESSAGE
                   MOVE -1 TO ACCTSIDL OF CCRDDLAI
               WHEN CARDSIDI OF CCRDDLAI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Card number can NOT be empty...'
                     TO WS-MESSAGE
                   MOVE -1 TO CARDSIDL OF CCRDDLAI
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF NOT ERR-FLG-ON
               MOVE ACCTSIDI OF CCRDDLAI TO WS-ACCT-INPUT
               MOVE CARDSIDI OF CCRDDLAI TO WS-CARD-INPUT

               IF WS-ACCT-INPUT IS NUMERIC
                   SET WS-ACCT-NUMERIC TO TRUE
               ELSE
                   MOVE 'N' TO WS-ACCT-NUMERIC-FLG
               END-IF

               IF WS-CARD-INPUT IS NUMERIC
                   SET WS-CARD-NUMERIC TO TRUE
               ELSE
                   MOVE 'N' TO WS-CARD-NUMERIC-FLG
               END-IF

               EVALUATE TRUE
                   WHEN NOT WS-ACCT-NUMERIC
                       MOVE 'Y' TO WS-ERR-FLG
                       MOVE
                       'Account number must be an 11 digit number'
                         TO WS-MESSAGE
                       MOVE -1 TO ACCTSIDL OF CCRDDLAI
                   WHEN WS-ACCT-INPUT-N = ZEROES
                       MOVE 'Y' TO WS-ERR-FLG
                       MOVE 'Account number must be a non zero value'
                         TO WS-MESSAGE
                       MOVE -1 TO ACCTSIDL OF CCRDDLAI
                   WHEN NOT WS-CARD-NUMERIC
                       MOVE 'Y' TO WS-ERR-FLG
                       MOVE 'Card number must be a 16 digit number'
                         TO WS-MESSAGE
                       MOVE -1 TO CARDSIDL OF CCRDDLAI
                   WHEN WS-CARD-INPUT-N = ZEROES
                       MOVE 'Y' TO WS-ERR-FLG
                       MOVE 'Card number must be a non zero value'
                         TO WS-MESSAGE
                       MOVE -1 TO CARDSIDL OF CCRDDLAI
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF

           IF ERR-FLG-ON
               PERFORM SEND-CARDDEL-SCREEN
           END-IF.

      *----------------------------------------------------------------*
      *                      FETCH-CARD-DETAILS
      *----------------------------------------------------------------*
       FETCH-CARD-DETAILS.

           MOVE ACCTSIDI OF CCRDDLAI TO WS-ACCT-INPUT
           MOVE CARDSIDI OF CCRDDLAI TO CARD-NUM

           EXEC CICS READ
                DATASET   (WS-CARDFILE)
                INTO      (CARD-RECORD)
                LENGTH    (LENGTH OF CARD-RECORD)
                RIDFLD    (CARD-NUM)
                KEYLENGTH (LENGTH OF CARD-NUM)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   EVALUATE TRUE
                       WHEN CARD-ACCT-ID NOT = WS-ACCT-INPUT-N
                           MOVE 'Y' TO WS-ERR-FLG
                           MOVE
                         'Card does not belong to the account entered'
                             TO WS-MESSAGE
                           MOVE -1 TO ACCTSIDL OF CCRDDLAI
                           PERFORM SEND-CARDDEL-SCREEN
                       WHEN CARD-ACTIVE-STATUS =
                                             WS-SOFT-DELETED-MARKER
                           MOVE 'Y' TO WS-ERR-FLG
                           MOVE 'Card has already been deleted...'
                             TO WS-MESSAGE
                           MOVE -1 TO CARDSIDL OF CCRDDLAI
                           PERFORM SEND-CARDDEL-SCREEN
                       WHEN OTHER
                           PERFORM POPULATE-DETAILS-ON-SCREEN
                           SET CARD-FETCHED-YES TO TRUE
                           MOVE 'Press F5 to CONFIRM delete...'
                             TO WS-MESSAGE
                           MOVE DFHNEUTR  TO ERRMSGC OF CCRDDLAO
                           PERFORM SEND-CARDDEL-SCREEN
                   END-EVALUATE
               WHEN DFHRESP(NOTFND)
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Card NOT found for key provided...'
                     TO WS-MESSAGE
                   MOVE -1 TO CARDSIDL OF CCRDDLAI
                   PERFORM SEND-CARDDEL-SCREEN
               WHEN OTHER
                   DISPLAY 'READ CARDDAT RESP:' WS-RESP-CD
                           ' REAS:' WS-REAS-CD
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to read card file...'
                     TO WS-MESSAGE
                   MOVE -1 TO ACCTSIDL OF CCRDDLAI
                   PERFORM SEND-CARDDEL-SCREEN
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      POPULATE-DETAILS-ON-SCREEN
      *----------------------------------------------------------------*
       POPULATE-DETAILS-ON-SCREEN.

           MOVE CARD-ACCT-ID          TO ACCTSIDO OF CCRDDLAO
           MOVE CARD-NUM              TO CARDSIDO OF CCRDDLAO
           MOVE CARD-EMBOSSED-NAME    TO CRDNAMEO OF CCRDDLAO
           MOVE CARD-ACTIVE-STATUS    TO CRDSTCDO OF CCRDDLAO

           MOVE CARD-EXPIRAION-DATE(6:2) TO WS-EXPMON-DISP
           MOVE CARD-EXPIRAION-DATE(1:4) TO WS-EXPYEAR-DISP
           MOVE WS-EXPMON-DISP        TO EXPMONO  OF CCRDDLAO
           MOVE WS-EXPYEAR-DISP       TO EXPYEARO OF CCRDDLAO.

      *----------------------------------------------------------------*
      *                      SOFT-DELETE-CARD-IN-FILE
      *                      READ UPDATE then REWRITE with
      *                      CARD-ACTIVE-STATUS = 'D'
      *----------------------------------------------------------------*
       SOFT-DELETE-CARD-IN-FILE.

           MOVE CARDSIDI OF CCRDDLAI TO CARD-NUM

           EXEC CICS READ
                DATASET   (WS-CARDFILE)
                INTO      (CARD-RECORD)
                LENGTH    (LENGTH OF CARD-RECORD)
                RIDFLD    (CARD-NUM)
                KEYLENGTH (LENGTH OF CARD-NUM)
                UPDATE
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Card NOT found during delete...'
                     TO WS-MESSAGE
                   MOVE -1 TO CARDSIDL OF CCRDDLAI
                   PERFORM SEND-CARDDEL-SCREEN
               WHEN OTHER
                   DISPLAY 'READ UPDATE CARDDAT RESP:' WS-RESP-CD
                           ' REAS:' WS-REAS-CD
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to lock card for delete...'
                     TO WS-MESSAGE
                   MOVE -1 TO ACCTSIDL OF CCRDDLAI
                   PERFORM SEND-CARDDEL-SCREEN
           END-EVALUATE

           IF NOT ERR-FLG-ON
               IF CARD-ACTIVE-STATUS = WS-SOFT-DELETED-MARKER
                   EXEC CICS UNLOCK
                        DATASET(WS-CARDFILE)
                   END-EXEC
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'Card has already been deleted...'
                     TO WS-MESSAGE
                   MOVE -1 TO CARDSIDL OF CCRDDLAI
                   PERFORM SEND-CARDDEL-SCREEN
               ELSE
                   MOVE WS-SOFT-DELETED-MARKER TO CARD-ACTIVE-STATUS

                   EXEC CICS REWRITE
                        DATASET   (WS-CARDFILE)
                        FROM      (CARD-RECORD)
                        LENGTH    (LENGTH OF CARD-RECORD)
                        RESP      (WS-RESP-CD)
                        RESP2     (WS-REAS-CD)
                   END-EXEC

                   EVALUATE WS-RESP-CD
                       WHEN DFHRESP(NORMAL)
                           PERFORM INITIALIZE-ALL-FIELDS
                           MOVE DFHGREEN  TO ERRMSGC OF CCRDDLAO
                           STRING 'Card '    DELIMITED BY SIZE
                                  CARD-NUM   DELIMITED BY SPACE
                                  ' has been deleted ...'
                                             DELIMITED BY SIZE
                             INTO WS-MESSAGE
                           SET CARD-FETCHED-NO TO TRUE
                           PERFORM SEND-CARDDEL-SCREEN
                       WHEN OTHER
                           DISPLAY 'REWRITE CARDDAT RESP:' WS-RESP-CD
                                   ' REAS:' WS-REAS-CD
                           MOVE 'Y'     TO WS-ERR-FLG
                           MOVE 'Unable to delete card record...'
                             TO WS-MESSAGE
                           MOVE -1 TO ACCTSIDL OF CCRDDLAI
                           PERFORM SEND-CARDDEL-SCREEN
                   END-EVALUATE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      *                      RETURN-TO-PREV-SCREEN
      *----------------------------------------------------------------*
       RETURN-TO-PREV-SCREEN.

           IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
               MOVE 'COMEN01C' TO CDEMO-TO-PROGRAM
           END-IF
           MOVE WS-TRANID    TO CDEMO-FROM-TRANID
           MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM
           MOVE ZEROS        TO CDEMO-PGM-CONTEXT
           EXEC CICS
               XCTL PROGRAM(CDEMO-TO-PROGRAM)
               COMMAREA(CARDDEMO-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      SEND-CARDDEL-SCREEN
      *----------------------------------------------------------------*
       SEND-CARDDEL-SCREEN.

           PERFORM POPULATE-HEADER-INFO

           MOVE WS-MESSAGE TO ERRMSGO OF CCRDDLAO

           EXEC CICS SEND
                     MAP(WS-MAPNAME)
                     MAPSET(WS-MAPSET)
                     FROM(CCRDDLAO)
                     ERASE
                     CURSOR
           END-EXEC.

      *----------------------------------------------------------------*
      *                      RECEIVE-CARDDEL-SCREEN
      *----------------------------------------------------------------*
       RECEIVE-CARDDEL-SCREEN.

           EXEC CICS RECEIVE
                     MAP(WS-MAPNAME)
                     MAPSET(WS-MAPSET)
                     INTO(CCRDDLAI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      POPULATE-HEADER-INFO
      *----------------------------------------------------------------*
       POPULATE-HEADER-INFO.

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF CCRDDLAO
           MOVE CCDA-TITLE02           TO TITLE02O OF CCRDDLAO
           MOVE WS-TRANID              TO TRNNAMEO OF CCRDDLAO
           MOVE WS-PGMNAME             TO PGMNAMEO OF CCRDDLAO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF CCRDDLAO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF CCRDDLAO.

      *----------------------------------------------------------------*
      *                      CLEAR-CURRENT-SCREEN
      *----------------------------------------------------------------*
       CLEAR-CURRENT-SCREEN.

           PERFORM INITIALIZE-ALL-FIELDS
           PERFORM SEND-CARDDEL-SCREEN.

      *----------------------------------------------------------------*
      *                      INITIALIZE-ALL-FIELDS
      *----------------------------------------------------------------*
       INITIALIZE-ALL-FIELDS.

           MOVE LOW-VALUES    TO CCRDDLAO
           MOVE -1            TO ACCTSIDL OF CCRDDLAI
           MOVE SPACES        TO ACCTSIDI OF CCRDDLAI
                                 CARDSIDI OF CCRDDLAI
                                 CRDNAMEO OF CCRDDLAO
                                 CRDSTCDO OF CCRDDLAO
                                 EXPMONO  OF CCRDDLAO
                                 EXPYEARO OF CCRDDLAO
                                 INFOMSGO OF CCRDDLAO
                                 WS-MESSAGE.
