000100161027     h dftactgrp(*NO) bnddir('QC2LE')
000200161027     d ProgStatus     sds
000300161027     d  Parms            *PARMS
000400161027     d  ProgName         *PROC
000500161027     d  ErrMsgID              40     46
000600161027     d  ErrMsg                91    169
000700161027     d  JobName              244    253
000800170203     d  UserId               254    363
000900161027     d  JobNumber            264    269
001000161027
001100161027     d GetUserProfile  PR                  ExtPgm('QSYGETPH')
001200161027     d  $userID                      10A   const
001300161027     d  $password                    10A   const
001400161027     d  $handle                      12A
001500161027     d  $errorCode                32766A   options(*varsize: *nopass)
001600161027     d  $length                      10I 0 const options(*nopass)
001700161027     d  $ccsid                       10I 0 const options(*nopass)
001800161027
001900161027     d SetUserProfile  PR                  ExtPgm('QWTSETP')
002000161027     d  $handle                      12A   const
002100161027     d  $errorCode                32766A   options(*varsize: *nopass)
002200161027
002300161027     d ErrorDS         DS
002400161027     d  $bytesPrv              1      4I 0 INZ(256)
002500161027     d  $bytesAvl              5      8I 0 INZ(0)
002600161027     d  $errMsgID              9     15
002700161027     d  $reserved             16     16
002800161027     d  $errMsgDta            17    256
002900161027
003000161027     d  nUserId        s             10a   inz('ARCAD_PGMR')
003100161027     d  nPassword      s             10a   inz('*NOPWDCHK')
003200161027     d  length         s              4B 0 inz(10)
003300161027     d  ccsid          s              4B 0 inz(37)
003400161027     D CurUSR         UDS                  DTAARA('CURUSER')
003500161027     d  curUserID                    10a
003600161027     D Curdta         UDS                  DTAARA('CURHNDL')
003700161027     d  curHandle                    12a
003800161027     D Arcdta         UDS                  DTAARA('ARCHNDL')
003900161027     d  newHandle                    12a
004000161027
004100161027      /free
004200161027         curUserID = UserID;
004300161027        In CURUSR;
004400161027        In CURDTA;
004500161027        In ARCDTA;
004600161027
004700161027       //Get handle on the current user profile, password not needed
004800161027        GetUserprofile(UserId: '*NOPWDCHK': curHandle: ErrorDS);
004900161027        if $bytesAvl > 0;
005000161027        //error
005100161027        DSPLY $errMsgID;
005200161027       // SNDPGMMSG 'get cur handle failed'
005300161027       else;
005400161027        Out CURDTA;
005500161027        //Get hanlde on the new user profile, password is needed here
005600161027         GetUserprofile(nUserId: '*NOPWDCHK': newHandle: ErrorDS);
005700161027
005800161027         if $bytesAvl > 0;
005900161027       //error
006000161027        DSPLY $errMsgID;
006100161027       // SNDPGMMSG 'get arcad handle failed'
006200161027         else;
006300161027        Out ARCDTA;
006400161027       //Switch to new user profile
006500161027         SetUserProfile(newHandle: ErrorDs);
006600161027         if $bytesAvl > 0;
006700161027         //error
006800161027        DSPLY $errMsgID;
006900161027         // SNDPGMMSG 'get arcad handle failed'
007000161027          else;
007100161027       endif;
007200161027        endif;
007300161027       endif;
007400161027
007500161027           eval *inlr = *on;
007600161027        return;
007700161027
007800161027             /end-free
007900161027
