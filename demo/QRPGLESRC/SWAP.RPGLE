000100161027     h dftactgrp(*NO) bnddir('QC2LE')
000101161027     d ProgStatus     sds
000102161027     d  Parms            *PARMS
000103161027     d  ProgName         *PROC
000104161027     d  ErrMsgID              40     46
000105161027     d  ErrMsg                91    169
000106161027     d  JobName              244    253
000107161027     d  UserId               254    263
000108170309-->  d  JOBNUMBER            264    269
000109161027
000112161027     d GetUserProfile  PR                  ExtPgm('QSYGETPH')
000113161027     d  $userID                      10A   const
000114161027     d  $password                    10A   const
000115161027     d  $handle                      12A
000116161027     d  $errorCode                32766A   options(*varsize: *nopass)
000117161027     d  $length                      10I 0 const options(*nopass)
000118161027     d  $ccsid                       10I 0 const options(*nopass)
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
000135161027     d  length         s              4B 0 inz(10)
000136161027     d  ccsid          s              4B 0 inz(37)
000137161027     D Curdta         UDS                  DTAARA('CURHNDL')
000138161027     d  curHandle                    12a
000140161027     D Arcdta         UDS                  DTAARA('ARCHNDL')
000141161027     d  newHandle                    12a
000143161027
000144161027      /free
000145161027        In CURDTA;
000146161027        In ARCDTA;
000147161027
000148161027       //Get handle on the current user profile, password not needed
000149161027        GetUserprofile(UserId: '*NOPWDCHK': curHandle: ErrorDS);
000150161027        if $bytesAvl > 0;
000151161027        //error
000152161027        DSPLY $errMsgID;
000153161027       // SNDPGMMSG 'get cur handle failed'
000154161027       else;
000155161027        Out CURDTA;
000156161027        //Get hanlde on the new user profile, password is needed here
000157161027         GetUserprofile(nUserId: '*NOPWDCHK': newHandle: ErrorDS);
000158161027
000163161027         if $bytesAvl > 0;
000164161027       //error
000165161027        DSPLY $errMsgID;
000166161027       // SNDPGMMSG 'get arcad handle failed'
000167161027         else;
000168161027        Out ARCDTA;
000169161027       //Switch to new user profile
000170161027         SetUserProfile(newHandle: ErrorDs);
000171161027         if $bytesAvl > 0;
000172161027         //error
000173161027        DSPLY $errMsgID;
000174161027         // SNDPGMMSG 'get arcad handle failed'
000175161027          else;
000176161027       endif;
000177161027        endif;
000178161027       endif;
000179161027
000180161027           eval *inlr = *on;
000181161027        return;
000182161027
000183161027             /end-free
000184161027
