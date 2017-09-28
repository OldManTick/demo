000100161027     h dftactgrp(*NO) bnddir('QC2LE')
000101161027     d ProgStatus     sds
000102161027     d  Parms            *PARMS
000103161027     d  ProgName         *PROC
000104161027     d  ErrMsgID              40     46
000105161027     d  ErrMsg                91    169
000106161027     d  JobName              244    253
000107161027     d  UserId               254    263
000108161027     d  JobNumber            264    269
000109161027
000119161027
000120161027     d SetUserProfile  PR                  ExtPgm('QWTSETP')
000121161027     d  $handle                      12A   const
000122161027     d  $errorCode                32766A   options(*varsize: *nopass)
000123161027
000124161027     d ErrorDS         DS
000125161027     d  $bytesPrv              1      4I 0 INZ(256)
000126161027     d  $bytesAvl              5      8I 0 INZ(0)
000127161027     d  $errMsgID              9     15
000128161027     d  $reserved             16     16
000129161027     d  $errMsgDta            17    256
000130161027
000133161027     d  nUserId        s             10a   inz('ARCAD_PGMR')
000134161027     d  nPassword      s             10a   inz('*NOPWDCHK')
000137161027     d CurUSR         UDS                  DTAARA('CURUSER')
000138161027     d  curUserID                    10a
000139161027     D Curdta         UDS                  DTAARA('CURHNDL')
000140161027     d  curHandle                    12a
000143161027
000144161027      /free
000145161027        In CURUSR;
000146161027        In CURDTA;
000148161027
000169161027       //Switch to new user profile
000170161027         SetUserProfile(curHandle: ErrorDs);
000171161027         if $bytesAvl > 0;
000172161027         //error
000173161027        DSPLY $errMsgID;
000174161027         // SNDPGMMSG 'get arcad handle failed'
000175161027          else;
000176161027       endif;
000179161027
000180161027           eval *inlr = *on;
000181161027        return;
000182161027
000183161027             /end-free
000184161027
