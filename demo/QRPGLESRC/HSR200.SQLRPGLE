000100888888      *%CSTD===========================================================*
000200888888      ** Application. : SAM        Arcad Sample application            *
000300888888      ** Component. . : HSR200                        Type: RPG        *
000400888888      **===============================================================*
000500888888      ** Sub-system . : TRD Trade                                      *
000600888888      ** Function . . : SAL Sales                                      *
000700888888      ** Sub-function :                                                *
000800888888      **%S=============================================================*
000900888888      ** Description of functions:                                     *
001000888888      **                                                               *
001100888888      **                                                               *
001200888888      **                                                               *
001300888888      **%E=============================================================*
001400888888      ** AUTHOR:                          00:00                        *
001500888888      ** MODIFS: 04 MMARREL    09/15/2014 10:36  V 1.00.L MR 14/   30  *
001600888888      **           Rld change                                          *
001700888888      **         03 MMARREL    07/03/2014 11:50  V 1.00.I MR 14/   29  *
001800888888      **           genius                                              *
001900888888      **         02 MMARREL    05/23/2014 10:03  V 1.00.G MR 14/   22  *
002000888888      **           MCH0001 when loading program                        *
002100888888      **         ** RBERNARDI  09/15/2014   :    V 1.00.E MR 14/   22  *
002200888888      **         01 ARCAD_PGMR 07/28/2000 12:13  V 1.00.B MR 14/   22  *
002300888888      *%ECSTD==========================================================*
002400990122      */TITLE Sales Order Entry / Maintenance / Inquiry
002500990106      *
002600990106      * System        : HSV Control & Tracking
002700990122      * Author        : Bob Lee (Intec Systems)
002800990106      * Date          : January 1999
002900990106      *
003000990106      *================================================================
003100990106      * Indicator usage:
003200990219      *  20 - Order Valid for Despatch
003300990128      *  30 - 45 Input fields- DSPATR.
003400990128      *  62 - Control display/non-display of F10-Update.
003500990210      *  75 - 78 Program mode - Dispatch/Entry/Inquiry/Maintenance
003600990128      *  79 - MSGSFL (SFLCLR ETC...).
003700990106      *  99 - General CHAIN/SETLL result.
003800990106      *
003900990106      *================================================================
004000990106      * Maintenance   :
004100990106      * Fix/Chg Ref. Date       Description.
004200990106      * ------------ ---------- -----------------------------------
004300990106      *================================================================
004400140523      *%EXEC OVRDBF INTFILE EXTFILE
004500140523      *%ATTR TGTRLS(V7R1M0)
004600140523      *
004700990222     H DATEDIT(*YMD)
004800990125     FHSLCUSTB  IF   E           K DISK
004900140523     F                                     RENAME(HSFCUST:HSFCUSTB)
005000990125      * Customer Master by Post Code.
005100990125      *
005200140915     FHSLCUSTA  IF   E           K DISK
005300990125      * Customer Master by Customer Account.
005400990106      *
005500990318     F*LTCUSTAIF  E           K        DISK
005600990318     F*           HSFCUST                           KRENAMEALTCUSTF
005700990216      * Customer Master by Customer Account.
005800990216      *
005900990122     FHSLORDPA  IF   E           K DISK
006000990122      * Sales Order Parameters.
006100990114      *
006200990128     FHSLTRACB  UF   E           K DISK
006300990203      * Voucher Tracking by Product/Series/Serial No./Element No.
006400990302      *
006500990302     FHSLEXPAA  UF A E           K DISK    USROPN
006600990302      * Unbanded Order line file by Product/Series/Serial No.
006700990122      *
006800990127     FHSLVCTLA  UF   E           K DISK
006900990122      * Voucher Control.
007000990122      *
007100990223     FHSLVXRFB  IF   E           K DISK
007200990122      * Agency Product Cross-Reference.
007300990122      *
007400990203     FHSLORDHA  UF A E           K DISK
007500990122      * Sales Order Header.
007600990122      *
007700990211     FHSLORDDB  UF A E           K DISK
007800990122      * Sales Order Detail.
007900990122      *
008000990204     FHSLODELA  UF A E           K DISK
008100990122      * Sales Order Delivery Detail.
008200990122      *
008300990125     FHSLSHPOA  IF   E           K DISK
008400990122      * Ship Only.
008500990302      *
008600990128     FHSLINVMA  UF   E           K DISK
008700990122      * Inventory Master.
008800990122      *
008900990125     FHSLWHSEA  IF   E           K DISK
009000990122      * Warehouse Master.
009100990122      *
009200990125     FHSLCOMPA  IF   E           K DISK
009300990122      * Company Master.
009400990122      *
009500990126     FHSLPRODA  IF   E           K DISK
009600990122      * Product Master.
009700990122      *
009800990215     FHSLDISCA  IF   E           K DISK
009900990215      * Customer Discount File
010000990215      *
010100990217BL   FHSPINVT   UF A E           K DISK
010200990122      * Inventory Transactions.
010300990122      *
010400990122     FHSS200    CF   E             WORKSTN INFDS(INFDS)
010500000728     F                                     SFILE(HSS200C:RRNX)
010600990106      * Screen file.
010700990106      *================================================================
010800990107      * Arrays.
010900990107      *
011000990215      * Days in Month for date validation.
011100990215     D DIM             S              2  0 DIM(12) CTDATA PERRCD(12)
011200990107      *================================================================
011300990301      * Data Structures.
011400990106      *
011500990106      * Screen Information DS.
011600990106     D INFDS           DS
011700990106     D  KEY                  369    369
011800990127     D  CSRLOC               370    371B 0
011900990106      *
012000000728     D XXXSDS         SDS           429
012100000728     D  XPGMID           *PROC
012200000728     D  XJOBNO               244    253
012300000728     D  XUSRID               254    263
012400990125      *
012500990121      * General DDMMCCYY date format.
012600990121     D                 DS
012700990204     D  DD                     1      2  0 INZ
012800990204     D  MM                     3      4  0 INZ
012900990204     D  CC                     5      6  0 INZ
013000990204     D  YY                     7      8  0 INZ
013100990125     D  DMCY                   1      8  0
013200990128      *
013300990128      * General CCYYMMDD date format.
013400990128     D                 DS
013500990204     D  CCC                    1      2  0 INZ
013600990204     D  YYY                    3      4  0 INZ
013700990204     D  MMM                    5      6  0 INZ
013800990204     D  DDD                    7      8  0 INZ
013900990128     D  CYMD                   1      8  0
014000990106      *
014100990128      * Input order date format.
014200990125     D                 DS
014300990126     D  JDD                    1      2  0 INZ
014400990126     D  JMM                    3      4  0 INZ
014500990126     D  JCC                    5      6  0 INZ
014600990126     D  JYY                    7      8  0 INZ
014700000728     D  XJORDD                 1      8  0
014800990125      *
014900990128      * Output order date format.
015000990128     D                 DS
015100990204     D  WKC                    1      2  0 INZ
015200990204     D  WKY                    3      4  0 INZ
015300990204     D  WKM                    5      6  0 INZ
015400990204     D  WKD                    7      8  0 INZ
015500990128     D  WKORDD                 1      8  0
015600990128      *
015700990106      *----------------------------------------------------------------
015800990106      * Constants.
015900990106      *
016000990106      * Named hexidecimal constants for function keys.
016100990106     D F03             C                   CONST(X'33')
016200990106     D F04             C                   CONST(X'34')
016300990106     D F06             C                   CONST(X'36')
016400990219     D F07             C                   CONST(X'37')
016500990203     D F10             C                   CONST(X'3A')
016600990106     D F11             C                   CONST(X'3B')
016700990113     D F12             C                   CONST(X'3C')
016800990119      *
016900990119      * Zeros to "SETOF" error indicators.
017000000728     D XZEROS          C                   CONST('0000000000000000000')
017100990106      *
017200990106      *================================================================
017300990106      * M A I N L I N E
017400990106      *================================================================
017500990210      *
017600990210      * Dispatch mode
017700000728     C     YMODE         IFEQ      'D'
017800990210     C                   MOVEL     *ON           *IN75
017900990210     C                   ELSE
018000990210     C                   MOVEL     *OFF          *IN75
018100990210     C                   ENDIF
018200160922      *                  READ      ARTICLE                                99
018300990210      *
018400990210      * Entry mode
018500000728     C     YMODE         IFEQ      'E'
018600990210     C                   MOVEL     *ON           *IN76
018700990210     C                   ELSE
018800990210     C                   MOVEL     *OFF          *IN76
018900990210     C                   ENDIF
019000990120      *
019100990203      * Inquiry mode
019200000728     C     YMODE         IFEQ      'I'
019300990203     C                   MOVEL     *ON           *IN77
019400990203     C                   ELSE
019500990203     C                   MOVEL     *OFF          *IN77
019600990203     C                   ENDIF
019700990205      *
019800990205      * Maintenance mode
019900000728     C     YMODE         IFEQ      'M'
020000990205     C                   MOVEL     *ON           *IN78
020100990205     C                   ELSE
020200990205     C                   MOVEL     *OFF          *IN78
020300990205     C                   ENDIF
020400990205      *
020500990203      * Program mode
020600990226     C     START         TAG
020700000728     C     YMODE         IFEQ      'I'
020800000728     C     YMODE         OREQ      'D'
020900000728     C     YMODE         OREQ      'M'
021000000728     C     YORDNO        IFNE      *BLANKS
021100000728     C                   MOVEL     YORDNO        XJSORD
021200000728     C                   MOVEL     YORDNO        WKORDN            8 0
021300990203     C     WKORDN        CHAIN     HSLORDHA                           99
021400990203     C                   ENDIF
021500990203     C                   ENDIF
021600990203      *
021700000728     C     YMODE         IFEQ      'D'
021800000728     C                   MOVE      JTYPE         XJTYPE
021900000728     C                   MOVE      JCUST         XJCUST
022000990219     C                   Z-ADD     JORDD         WKORDD
022100990219     C                   Z-ADD     WKC           JCC
022200990219     C                   Z-ADD     WKY           JYY
022300990219     C                   Z-ADD     WKM           JMM
022400990219     C                   Z-ADD     WKD           JDD
022500000728     C                   MOVE      JORDR         XJORDR
022600000728     C                   MOVE      JSORD         XJSORD
022700990219     C     JSTAT         IFEQ      'A'
022800990219     C     JSTAT         OREQ      ' '
022900990219     C                   MOVE      *ON           *IN20
023000990219     C                   ELSE
023100990219     C                   MOVE      *OFF          *IN20
023200990219     C                   MOVEL     'HSV2015'     P0MSID
023300000728     C                   EXSR      YSNDMG
023400990219     C                   ENDIF
023500990216     C     JCUST         CHAIN     HSLCUSTA                           13
023600000728     C                   MOVE      EPCDE         XJPCDE
023700000728     C                   MOVE      ENAM1         XENAM1
023800990216     C     JSORD         SETLL     HSLORDDB
023900990216     C     JSORD         READE     HSLORDDB                               99
024000990216     C     *IN99         DOWEQ     *OFF
024100000728     C                   ADD       KQTYN         XVQTY
024200990216     C     JSORD         READE     HSLORDDB                               99
024300990216     C                   ENDDO
024400000728     C                   Z-ADD     KDISP         XJDISP
024500990216     C                   ELSE
024600000728     C                   EXSR      YCLRSC
024700990216     C                   ENDIF
024800990219      *
024900990225      * Dispatch mode.
025000000728XXX  C     YMODE         IFEQ      'D'
025100990225XXX  C     JSORD         CHAIN     HSLODELA                           99
025200990225XXX  C     VNAM1         IFEQ      *BLANKS
025300000728XXX  C                   MOVEL(P)  ENAM1         XXNAM1
025400000728XXX  C                   MOVEL(P)  ENAM2         XXNAM2
025500000728XXX  C                   MOVEL(P)  EADR1         XXADR1
025600000728XXX  C                   MOVEL(P)  EADR2         XXADR2
025700990225XXX  C                   ELSE
025800000728XXX  C                   MOVEL(P)  VNAM1         XXNAM1
025900000728XXX  C                   MOVE      *BLANKS       XXNAM2
026000000728XXX  C                   MOVEL(P)  VADR1         XXADR1
026100000728XXX  C                   MOVEL(P)  VADR2         XXADR2
026200990225XXX  C                   ENDIF
026300990225XXX  C                   ENDIF
026400990225      *
026500990225      * Dispatch, or Inquiry, or Maintenance mode.
026600000728XXX  C     YMODE         IFEQ      'I'
026700000728XXX  C     YMODE         OREQ      'D'
026800000728XXX  C     YMODE         OREQ      'M'
026900000728XXX  C                   MOVEL     YCUST         XJCUST
027000000728XXX  C                   MOVEL     YTYPE         XJTYPE
027100990225XXX  C                   ENDIF
027200990219     C     HDR           TAG
027300990225XXX  C                   MOVE      *OFF          *IN47
027400990122      * Display screen to receive Customer No. until F03/F10
027500990128     C     KEY           DOUEQ     F03
027600990122     C     KEY           OREQ      F10
027700000728     C     XERROR        ANDEQ     XNO
027800990121      *
027900990302      * Exit if F03 pressed.
028000990302     C     KEY           IFEQ      F03
028100990302     C                   MOVE      *ON           *INLR
028200990302     C                   LEAVE
028300990302     C                   ENDIF
028400990302      *
028500160922     C/exec sql
028600160922     c+ Declare hsr200_csr cursor for
028700160922     C+ Select * from article
028800160922     C/end-exec
028900160922      * Di:play messages.
029000990121     C                   WRITE     MSGCTL
029100990121      *
029200000728     C                   TIME                    XXTME
029300990122     C                   EXFMT     HSS200A
029400990225      *
029500000728ZZZ  C     XJTYPE        CHAIN     HSLORDPA                           99
029600990302ZZZ  C     MSALE         IFEQ      'C'
029700990302ZZZ  C                   MOVE      *ON           *IN30
029800000728ZZZ  C                   MOVE      XYES          XERROR
029900990302ZZZ  C                   MOVEL     'HSV2027'     P0MSID
030000000728ZZZ  C                   EXSR      YSNDMG
030100990302ZZZ  C                   ITER
030200990302ZZZ  C                   ENDIF
030300990302      *
030400000728XXX  C                   CLEAR                   XVQTY
030500990225XXX  C     *IN20         IFEQ      *OFF
030600000728XXX  C     YMODE         ANDEQ     'D'
030700990225XXX  C                   ITER
030800990225XXX  C                   ENDIF
030900990225      *
031000990217BL   C                   Z-ADD     1             RCDNBR
031100990121      *
031200990121      * Clear messages.
031300000728     C                   EXSR      YCLRMG
031400990120      *
031500990120      * Process the various function keys available on the screen.
031600990219      *
031700990219      * Process Despatch if F07 pressed.
031800990226     C     KEY           IFEQ      F07
031900990226      *
032000990226      * Validate that Order Type is a valid Type.
032100000728     C     XJTYPE        CHAIN     HSLORDPA                           99
032200990226     C     *IN99         IFEQ      *ON
032300990226     C                   MOVE      *ON           *IN30
032400000728     C                   MOVE      XYES          XERROR
032500990226     C                   MOVEL     'HSV2001'     P0MSID
032600000728     C                   EXSR      YSNDMG
032700990226     C                   GOTO      START
032800990226     C                   ENDIF
032900990226     C                   ENDIF
033000990219      *
033100000728     C     YMODE         IFEQ      'D'
033200000728     C     WKORDN        ANDNE     XJSORD
033300000728     C                   MOVEL     XJSORD        WKORDN            8 0
033400000728     C     XJSORD        CHAIN     HSLORDHA                           99
033500990219     C     *IN99         IFEQ      *ON
033600990219     C                   MOVE      *OFF          *IN20
033700990219     C                   MOVEL     'HSV2017'     P0MSID
033800000728     C                   EXSR      YSNDMG
033900990219     C                   ELSE
034000000728     C                   MOVE      JTYPE         XJTYPE
034100000728     C                   MOVE      JCUST         XJCUST
034200990219     C                   Z-ADD     JORDD         WKORDD
034300990219     C                   Z-ADD     WKC           JCC
034400990219     C                   Z-ADD     WKY           JYY
034500990219     C                   Z-ADD     WKM           JMM
034600990219     C                   Z-ADD     WKD           JDD
034700000728     C                   MOVE      JORDR         XJORDR
034800000728     C                   MOVE      JSORD         XJSORD
034900990219     C     JSTAT         IFEQ      'A'
035000990219     C     JSTAT         OREQ      ' '
035100990219     C                   MOVE      *ON           *IN20
035200990219     C                   ELSE
035300990219     C                   MOVE      *OFF          *IN20
035400990219     C                   MOVEL     'HSV2015'     P0MSID
035500000728     C                   EXSR      YSNDMG
035600990219     C                   ENDIF
035700990219     C                   ENDIF
035800990219     C     JCUST         CHAIN     HSLCUSTA                           13
035900000728     C                   MOVE      EPCDE         XJPCDE
036000000728     C                   MOVE      ENAM1         XENAM1
036100990219     C     JSORD         SETLL     HSLORDDB
036200990219     C     JSORD         READE     HSLORDDB                               99
036300990219     C     *IN99         DOWEQ     *OFF
036400000728     C                   ADD       KQTYN         XVQTY
036500990219     C     JSORD         READE     HSLORDDB                               99
036600990219     C                   ENDDO
036700000728     C                   Z-ADD     KDISP         XJDISP
036800990222     C                   GOTO      HDR
036900990219     C                   ENDIF
037000990204      *
037100990211      * Program in inquiry, dispatch or maintenance mode
037200000728     C     YMODE         IFEQ      'I'
037300000728     C     YMODE         OREQ      'M'
037400000728     C     YMODE         OREQ      'D'
037500990204      *
037600990204      * Initialise subfile
037700990215     C                   MOVEA     '1001'        *IN(70)
037800990204     C                   WRITE     HSS200B
037900000728     C     XJSORD        IFNE      0
038000000728     C                   Z-ADD     0             RRNX
038100000728     C     XJSORD        SETLL     HSLORDDB
038200990204     C     *IN97         DOUEQ     *ON
038300000728     C     XJSORD        READE     HSLORDDB                               97
038400990204      *
038500990204      * Build subfile from existing order.
038600990204     C     *IN97         IFEQ      *OFF
038700000728     C                   ADD       1             RRNX
038800000728     C     RRNX          CHAIN     HSS200C                            99
038900000728     C                   Z-ADD     KLINE         XOLIN
039000000728     C                   MOVEL     KLTYP         XSELC
039100000728     C                   MOVEL     KPROD         XKPROD
039200000728     C                   MOVEL     KDESC         XNDESC
039300000728     C                   MOVEL     KSERC         XKSERC
039400000728     C                   Z-ADD     KSERNF        XKSERF
039500000728     C                   Z-ADD     KSERNT        XKSERT
039600000728     C                   Z-ADD     KQTYN         XKQTYN
039700000728     C                   Z-ADD     KPRIC         XKPRIC
039800000728     C                   Z-ADD     KVALU         XKVALU
039900000728XXX  C                   MOVE      XKQTYN        XXQTYN
040000990218BL    *
040100990218BL    * Ship Only - set line value to zero.
040200990218BL   C     SHONLY        IFEQ      'Y'
040300000728BL   C     XSELC         ANDEQ     'S'
040400000728BL   C                   Z-ADD     0             XKVALU
040500990218BL   C                   ENDIF
040600990218BL    *
040700000728BL   C*          £KVALU    MULT MVATP     £KVATA
040800990204      *
040900990204      * Extended value
041000000728     C     XSELC         IFEQ      'S'
041100000728     C     XSELC         OREQ      'N'
041200000728     C     XKPRIC        MULT      XKQTYN        XKVALU
041300990218BL    *
041400990218BL    * Ship Only - set line value to zero.
041500990218BL   C     SHONLY        IFEQ      'Y'
041600000728BL   C     XSELC         ANDEQ     'S'
041700000728BL   C                   Z-ADD     0             XKVALU
041800990218BL   C                   ENDIF
041900990218BL    *
042000000728BL   C*          £KVALU    MULT MVATP     £KVATA
042100990215      *
042200990204     C                   ENDIF
042300990204      *
042400990215      * Write subfile record.
042500990215     C     *IN99         IFEQ      *OFF
042600990215     C                   UPDATE    HSS200C
042700990215     C                   ELSE
042800990215     C                   WRITE     HSS200C
042900990215     C                   ENDIF
043000990204     C                   ENDIF
043100990204     C                   ENDDO
043200990204     C                   ENDIF
043300990204     C                   LEAVE
043400990204     C                   ENDIF
043500990215      *
043600990215      * Program mode - Entry
043700000728     C     YMODE         IFEQ      'E'
043800990120      *
043900990120      * Execute search if F04 pressed.
044000990128     C     KEY           IFEQ      F04
044100000728     C                   EXSR      YSERCH
044200990225XXX  C                   ITER
044300000728XXX  C                   EXSR      YCLRMG
044400990128     C                   ENDIF
044500990121      *
044600990122      * Validate header screen fields.
044700990122     C                   EXSR      VALIDH
044800990121      *
044900990122      * Validate ship only requirements.
045000990122     C                   EXSR      SHPONL
045100990106      *
045200990204     C                   ENDIF
045300990225      *
045400990225      * Process Despatch if F07 pressed.
045500990225RT   C     KEY           IFEQ      F07
045600990225      *
045700990225RT   C                   LEAVE
045800990225RT   C                   ENDIF
045900990225      *
046000990122      * End of DO loops for F03/F10 key checks.
046100990128     C                   ENDDO
046200990302     C                   MOVE      *OFF          *IN61
046300990106      *
046400990127      * Clear messages.
046500000728     C                   EXSR      YCLRMG
046600990204      *
046700990204      * Exit if F03 pressed.
046800990204     C     KEY           IFEQ      F03
046900990204      * Exit program.
047000990204     C                   MOVE      *ON           *INLR
047100990204     C                   RETURN
047200990204     C                   ENDIF
047300990127      *
047400990128      *-------------------------------------------------------
047500990215      *
047600990215      * Program mode - Entry
047700990128      * Retrieve order number.
047800000728     C     YMODE         IFEQ      'E'
047900000728     C     *DTAARA       DEFINE    HSAORDERNO    ORDERX            8 0
048000000728     C     *LOCK         IN        ORDERX
048100000728     C                   ADD       1             ORDERX
048200000728     C                   OUT       ORDERX
048300000728     C                   Z-ADD     ORDERX        XJSORD
048400990204     C                   ENDIF
048500990128      *
048600990122      * Display screen to receive Order Details until F10/F12
048700990127     C     DET           TAG
048800990204      *
048900990215      * Program in inquiry, or dispatch, or maintenance mode
049000000728     C     YMODE         IFEQ      'I'
049100000728     C     YMODE         OREQ      'M'
049200000728     C     YMODE         OREQ      'D'
049300990205     C     KEY           OREQ      F12
049400990205     C                   MOVEL     *OFF          *IN73
049500990205     C                   ELSE
049600990205     C                   MOVEL     *ON           *IN73
049700990204     C                   ENDIF
049800990204      *
049900990128     C     KEY           DOUEQ     F10
050000000728     C     XERROR        ANDEQ     XNO
050100990126     C     KEY           OREQ      F12
050200990122      *
050300990122      * Display messages.
050400990122     C                   WRITE     MSGCTL
050500990122      *
050600990211      * Read subfile to accumulate order value.
050700000728     C     RRNX          IFGT      0
050800990211     C                   Z-ADD     1             RRNVAL
050900000728     C                   Z-ADD     0             XKTVAL
051000990211     C     *IN98         DOUEQ     *ON
051100990217BL   C     RRNVAL        OREQ      100
051200000728BL   C     XSELC         OREQ      *BLANK
051300990211     C     RRNVAL        CHAIN     HSS200C                            98
051400000728     C     XSELC         IFNE      *BLANK
051500000728     C     XKPROD        ANDNE     *BLANK
051600990211     C     *IN98         ANDEQ     *OFF
051700000728     C     XSELC         ORNE      *BLANK
051800000728     C     XNDESC        ANDNE     *BLANK
051900990211     C     *IN98         ANDEQ     *OFF
052000000728     C                   ADD       XKVALU        XKTVAL
052100990211     C                   ENDIF
052200990211     C                   ADD       1             RRNVAL
052300990211     C                   ENDDO
052400990211     C                   ENDIF
052500990225      *
052600990225      * Dispatch mode.
052700000728XXX  C     YMODE         IFEQ      'D'
052800000728XXX  C     XJCUST        CHAIN     HSLCUSTA                           99
052900990225XXX   * ...load Customer record into screen 2 fields.
053000000728XXX  C                   MOVEL     ENAM1         XJNAM1                         Name 1
053100000728XXX  C                   MOVEL     ENAM2         XJNAM2                         Name 2
053200000728XXX  C                   MOVEL     EADR1         XJADR1                         Address 1
053300000728XXX  C                   MOVEL     EADR2         XJADR2                         Address 2
053400990225XXX  C                   ENDIF
053500990225      *
053600990211      *
053700990211      * Display footer.
053800990225RT   C     KEY           IFNE      F07
053900990127     C                   WRITE     HSS200E
054000990225RT   C                   ENDIF
054100990204      *
054200000728     C                   TIME                    XXTME
054300990205     C                   MOVEA     '011'         *IN(70)
054400990204      *
054500990225RT   C     KEY           IFNE      F07
054600990127     C                   EXFMT     HSS200B
054700000728XXX  C                   MOVE      XNO           XJUMP1
054800990302      *
054900990302     C     KEY           IFNE      F10
055000000728     C                   EXSR      YDELT
055100990302     C                   ENDIF
055200990302      *
055300990302RT   C                   ENDIF
055400990302      *
055500990127     C                   MOVEL     *OFF          *IN73
055600990302     C                   MOVEL     *OFF          *IN86
055700990122      *
055800990223OWE   * Clear messages (Only if an error has not been repeated)
055900990223      *
056000000728OWE  C     XERROR        IFEQ      XNO
056100990122      * Clear messages.
056200000728     C                   EXSR      YCLRMG
056300990223OWE  C                   ENDIF
056400990122      *
056500990122      * Process the various function keys available on the screen.
056600990302      *
056700990122      * Exit if F12 pressed.
056800990128     C     KEY           IFEQ      F12
056900990127     C                   GOTO      HDR
057000990128     C                   ENDIF
057100990122      *
057200990122      * Execute search if F04 pressed.
057300990128     C     KEY           IFEQ      F04
057400000728     C                   EXSR      YSERSF
057500990126     C                   ITER
057600990128     C                   ENDIF
057700990122      *
057800990122      * Check for agency cross-references.
057900990217BL   C     MXREF         IFEQ      'Y'
058000990122     C                   EXSR      AGXREF
058100990217BL   C                   ENDIF
058200990122      *
058300990223OWE   * Validate order detail screen fields (only re-validate data
058400990223OWE   * if it has been changed. Otherwise re-display error message).
058500000728OWE  C     XERROR        IFEQ      XYES
058600990223OWE  C     RRNS          CHAIN     HSS200C                            98
058700000728OWE  C     XSELC         IFNE      TSELC
058800000728OWE  C     XKPROD        ORNE      TPROD
058900000728OWE  C     XKPRIC        ORNE      TPRIC
059000000728OWE  C     XKQTYN        ORNE      TQTYN
059100000728OWE  C     XNDESC        ORNE      TDESC
059200990223OWE  C                   MOVE      *ON           *IN93
059300990223OWE  C                   ENDIF
059400990223OWE  C                   ENDIF
059500990225      *
059600990223OWE  C     *IN93         IFEQ      *ON
059700000728     C     XERROR        OREQ      XNO
059800990223      *
059900990223      * Validate order detail screen fields.
060000990223     C                   EXSR      VALIDD
060100990223OWE  C                   ENDIF
060200990122      *
060300990122      * Discount retrieval.
060400990122     C                   EXSR      DISCNT
060500990122      *
060600990122      * Stock allocation preview
060700990122      *                 - with banded allocation and manual allocation.
060800990223BL   C*          MSALE     IFEQ 'S'
060900990223     C*                    EXSR ALLOCS
061000990223BL   C*                    ENDIF
061100990122      *
061200990302      * Process Dispatch if F07 pressed.
061300990225RT   C     KEY           IFEQ      F07
061400990225      *
061500990225RT   C                   LEAVE
061600990225RT   C                   ENDIF
061700990225      *
061800990122      * End of DO loops for F10/F12 key checks.
061900990128     C                   ENDDO
062000990122      *
062100990122      *-------------------------------------------------------
062200990122      * Confirm order - prompt for delivery details.
062300990122      *               - file updates when F10 pressed.
062400990128     C     KEY           DOUEQ     F10
062500000728     C     XERROR        ANDEQ     XNO
062600990127     C     KEY           OREQ      F12
062700990127      *
062800990127      * Clear messages.
062900000728     C                   EXSR      YCLRMG
063000990127      *
063100990127      * Process the various function keys available on the screen.
063200990127      *
063300990127      * Exit if F12 pressed.
063400990223OOO  C*          KEY       IFEQ F12
063500990223OOO  C*                    GOTO DET
063600990223OOO  C*                    ENDIF
063700990122      *
063800990122      * Check for minimum order value.
063900990225RT   C     KEY           IFNE      F07
064000990127     C     MORDV         IFGT      0
064100990122     C                   EXSR      MINVAL
064200990127     C                   ENDIF
064300990225      *
064400990225XXX   * Credit limit check.... Warning only.
064500000728XXX  C     XKTVAL        ADD       XJTYSL        TOT              20 5
064600000728XXX  C     TOT           IFGT      XJCLIM
064700990225XXX  C                   MOVE      *ON           *IN46
064800990225XXX  C                   MOVEL     'HSV2019'     P0MSID
064900000728XXX  C                   EXSR      YSNDMG
065000990225XXX  C                   ENDIF
065100990225RT   C                   ENDIF
065200990225      *
065300990122      * Display messages.
065400990122     C                   WRITE     MSGCTL
065500990127      *
065600990128      * Display key text footer.
065700990225XXXM C*                    WRITEHSS200E
065800990225RT   C     KEY           IFNE      F07
065900990225XXXM C                   WRITE     HSS200F
066000990225RT   C                   ENDIF
066100990122      *
066200990128      * Delivery address prompt screen.
066300000728     C                   TIME                    XXTME
066400990204      *
066500990211      * Program in inquiry, dispatch or maintenance mode
066600000728     C     YMODE         IFEQ      'I'
066700000728     C     YMODE         OREQ      'M'
066800000728     C     YMODE         OREQ      'D'
066900990204     C     WKORDN        CHAIN     HSLODELA                           99
067000990204      *
067100990204      * Write fields for order delivery detail record.
067200990204     C     *IN99         IFEQ      *OFF
067300000728     C                   MOVEL     VNAM1         XVNAM1
067400000728     C                   MOVEL     VADR1         XVADR1
067500000728     C                   MOVEL     VADR2         XVADR2
067600000728     C                   MOVEL     VADR3         XVADR3
067700000728     C                   MOVEL     VPCDE         XVPCDE
067800000728     C                   MOVEL     VDIN1         XVDIN1
067900000728     C                   MOVEL     VDIN2         XVDIN2
068000000728     C                   MOVEL     VDIN3         XVDIN3
068100990204     C                   ENDIF
068200990204     C                   ENDIF
068300990204      *
068400990204      * Write order delivery details record.
068500990225RT   C     KEY           IFNE      F07
068600990127     C                   EXFMT     HSS200D
068700990225RT   C                   ENDIF
068800990223      *
068900990223OWE   * Exit if F12 pressed.
069000990223OWE  C     KEY           IFEQ      F12
069100000728     C                   EXSR      YDELT
069200990223OWE  C                   GOTO      DET
069300990223OWE  C                   ENDIF
069400990122      *
069500990122      * Clear messages.
069600990225XXX  C     *IN47         IFEQ      *OFF
069700000728     C                   EXSR      YCLRMG
069800990225XXX  C                   ENDIF
069900990122      *
070000990128      * Update all files, if no errors exist.
070100990201     C     KEY           IFEQ      F10
070200000728     C     XERROR        ANDEQ     XNO
070300990225RT   C     KEY           OREQ      F07
070400000728     C                   Z-ADD     1             RRNX
070500990217     C     *IN98         DOUEQ     *ON
070600000728BL   C     RRNX          OREQ      100
070700000728BL   C     XSELC         OREQ      *BLANK
070800000728     C     RRNX          CHAIN     HSS200C                            98
070900000728     C     XSELC         IFNE      *BLANK
071000000728     C     XKPROD        ANDNE     *BLANK
071100990211     C     *IN98         ANDEQ     *OFF
071200000728     C     XSELC         ORNE      *BLANK
071300000728     C     XNDESC        ANDNE     *BLANK
071400990211     C     *IN98         ANDEQ     *OFF
071500990225      *
071600990225      * Save RRN for current order line.
071700000728     C                   Z-ADD     RRNX          RRNLIN
071800990223      *
071900990225OWE   * Update Voucher Control if in entry mode....
072000000728OWE  C     YMODE         IFEQ      'E'
072100990223BL   C     MSALE         ANDEQ     'S'
072200990223OWE  C                   EXSR      UPPVCT
072300990223OWE  C                   ENDIF
072400990223      *
072500990225      * Re-position to required subfile line
072600000728     C                   Z-ADD     RRNLIN        RRNX
072700000728     C     RRNX          CHAIN     HSS200C                            98
072800990122      *
072900990218BL    * Create/Update order details if in Entry,Maint,or Dispatch mode.
073000000728BL   C     YMODE         IFEQ      'E'
073100000728BL   C     YMODE         OREQ      'D'
073200000728BL   C     YMODE         OREQ      'M'
073300990122     C                   EXSR      UPORDD
073400990218BL   C                   ENDIF
073500990218      *
073600990218BL    * Update voucher tracking if in Entry or Dispatch mode.
073700990218BL    * Stock lines only.
073800000728     C     YMODE         IFEQ      'E'
073900000728     C     XSELC         ANDEQ     'S'
074000000728BL   C     YMODE         OREQ      'D'
074100000728BL   C     XSELC         ANDEQ     'S'
074200990211     C                   EXSR      UPTRAC
074300990217BL   C                   ENDIF
074400990219      *
074500990219BL    * Allocate inventory if in Entry mode.
074600990219BL    * Stock lines only.
074700000728BL   C     YMODE         IFEQ      'E'
074800000728BL   C     XSELC         ANDEQ     'S'
074900990219      * Update inventory allocations.
075000990219     C                   EXSR      UPINVA
075100990219     C                   ENDIF
075200990218      *
075300990219BL    * Sell inventory if in Dispatch mode.
075400990218BL    * Stock lines only.
075500000728BL   C     YMODE         IFEQ      'D'
075600000728BL   C     XSELC         ANDEQ     'S'
075700990219      * Update inventory sales.
075800990219BL   C                   EXSR      UPINVS
075900990211      *
076000990211      * Create inventory sales transaction.
076100990128     C                   EXSR      UPINVT
076200990211     C                   ENDIF
076300990128     C                   ENDIF
076400000728     C                   ADD       1             RRNX
076500990128     C                   ENDDO
076600990211      *
076700990218BL    * Create/Update order header if in Entry,Maint,or Dispatch mode.
076800000728BL   C     YMODE         IFEQ      'E'
076900000728BL   C     YMODE         OREQ      'D'
077000000728BL   C     YMODE         OREQ      'M'
077100990211     C                   EXSR      UPORDH
077200990211      *
077300990218      * Create/update delivery record if in Entry,Maint or Dispat.mode.
077400990211     C                   EXSR      UPODEL
077500990218BL   C                   ENDIF
077600990128      *
077700990218BL    * Ship to all delivery points if in Entry mode.
077800000728BL   C     YMODE         IFEQ      'E'
077900990218BL   C     MALLP         ANDEQ     'Y'
078000990128     C                   EXSR      SHPALL
078100990218BL   C                   ENDIF
078200990128      *
078300990218BL    * Print delivery note if in Entry mode, and charges only not set.
078400000728BL   C     YMODE         IFEQ      'E'
078500990218BL   C     MINVC         ANDNE     'Y'
078600990128     C                   EXSR      PRTDEL
078700990218BL   C                   ENDIF
078800990128      *
078900990218BL    * Print Invoice if in Entry mode, and Immediate print required.
079000000728BL   C     YMODE         IFEQ      'E'
079100990218BL   C     MINVP         ANDEQ     'I'
079200990128     C                   EXSR      PRTINV
079300990217BL   C                   ENDIF
079400990218      *
079500990128     C                   ENDIF
079600990225      * Process Despatch if F07 pressed.
079700990225RT   C     KEY           IFEQ      F07
079800000728     C                   EXSR      YCLRSC
079900990225      *
080000990225RT   C                   LEAVE
080100990225RT   C                   ENDIF
080200990128      *
080300990122      * End of Confirmation.
080400990128     C                   ENDDO
080500990205      *
080600990205      * Return to order details if F12 pressed.
080700990205     C     KEY           IFEQ      F12
080800990205     C                   GOTO      DET
080900990205     C                   ENDIF
081000990205      *
081100990205      * Clear Subfile
081200990205     C                   MOVEA     '1000'        *IN(70)
081300990205     C                   WRITE     HSS200B
081400990302      *
081500990302      * Clear contents of HSPEXPA.
081600000728     C                   EXSR      YDELT
081700990205      *
081800990205      * Return to order header.
081900990205     C                   GOTO      HDR
082000990122      *
082100990106      *****************************************************************
082200990120      *----------------------------------------------------------------
082300000728      * ‡CLRSC - Clear screen fields.
082400990120      *----------------------------------------------------------------
082500990120      *
082600000728     C     YCLRSC        BEGSR
082700990120      *
082800990217BL    * Clear header screen fields.
082900000728BL   C     YMODE         IFNE      'E'
083000000728     C                   CLEAR                   XJTYPE
083100990217BL   C                   ENDIF
083200000728BL   C     YMODE         IFEQ      'D'
083300000728     C                   CLEAR                   XJSORD
083400000728     C                   CLEAR                   XENAM1
083500000728     C                   CLEAR                   XVQTY
083600990219BL   C                   ENDIF
083700000728     C                   CLEAR                   XJCOMP
083800000728     C                   CLEAR                   XJWHSE
083900000728     C                   CLEAR                   XJPCDE
084000000728     C                   CLEAR                   XJCUST
084100000728     C                   CLEAR                   XJDISP
084200000728     C                   CLEAR                   XJORDD
084300000728     C                   CLEAR                   XJORDR
084400990120      *
084500990217BL    * Clear delivery screen fields.
084600000728BL   C                   CLEAR                   XVNAM1
084700000728BL   C                   CLEAR                   XVADR1
084800000728BL   C                   CLEAR                   XVADR2
084900000728BL   C                   CLEAR                   XVADR3
085000000728BL   C                   CLEAR                   XVPCDE
085100000728BL   C                   CLEAR                   XVDIN1
085200000728BL   C                   CLEAR                   XVDIN2
085300000728BL   C                   CLEAR                   XVDIN3
085400990120     C                   ENDSR
085500990122      *
085600990122      *----------------------------------------------------------------
085700990122      * SHPONL - Validate ship only requirements.
085800990122      *----------------------------------------------------------------
085900990122     C     SHPONL        BEGSR
086000990125      *
086100990125      * Check if a ship only record exists for this customer
086200000728     C     XJCUST        CHAIN     HSLSHPOA                           99
086300990125     C     *IN99         IFEQ      *OFF
086400990125     C                   MOVEL     'Y'           SHONLY            1
086500990218BL   C                   ELSE
086600990218BL   C                   MOVEL     ' '           SHONLY
086700990125     C                   ENDIF
086800990122     C                   ENDSR
086900990122      *
087000990122      *----------------------------------------------------------------
087100990122      * AGXREF - Check for agency cross-references.
087200990122      *----------------------------------------------------------------
087300990122     C     AGXREF        BEGSR
087400990223     C     KEYXRF        CHAIN     HSLVXRFB                           99
087500990126     C     *IN99         IFEQ      *OFF
087600990128     C                   MOVEL     HPROD         KPROD
087700000728     C                   MOVEL     HPROD         XKPROD
087800990126     C                   ENDIF
087900990122     C                   ENDSR
088000990122      *
088100990122      *----------------------------------------------------------------
088200990122      * VALIDD - Validate order detail screen fields.
088300990122      *----------------------------------------------------------------
088400990122     C     VALIDD        BEGSR
088500990126      *
088600990126      * Order Detail validation:
088700990126      *
088800990126      * Reset work values.
088900000728     C*                    Z-ADD0         RRN£
089000990223OWEM C                   Z-ADD     0             RRNS              5 0
089100990223OWE  C                   Z-ADD     1             RRNT              5 0
089200000728     C                   RESET                   XERROR
089300990126     C                   MOVE      *OFF          *IN61                          Hide F10/F11
089400990126      *
089500990126      * Reset error indicators.
089600000728     C                   MOVEA     XZEROS        *IN(30)
089700990126      *
089800990126      * Read all Records from subfile and validate contents.
089900990127     C     *IN99         DOUEQ     *ON                                          ..Do1
090000990223OWE   *
090100990223OWE   * Process the subfile in one of two modes depending on whether
090200990223OWE   * a previous change to the subfile has been made....
090300990223OWE   *
090400990223OWE  C     *IN82         IFEQ      *OFF
090500990223OWE  C                   READC     HSS200C                                99
090600000728OWE  C                   Z-ADD     RRNX          RRNT
090700990223OWE  C                   ADD       1             RRNT
090800990223OWE  C                   ELSE
090900990223OWE  C     RRNT          IFEQ      101
091000990223OWE  C                   LEAVE
091100990223OWE  C                   ENDIF
091200990223OWE  C     RRNT          CHAIN     HSS200C                            99
091300990223OWE  C                   ADD       1             RRNT
091400990223OWE  C                   ENDIF
091500990223      *
091600990223     C*                    READCHSS200C                  99
091700990126      * Retrieve subfile record.
091800990126     C     *IN99         IFEQ      *OFF                                         ...If2
091900000728OWE  C                   MOVE      XKPROD        TPROD1           13
092000000728OWE  C                   MOVE      XKQTYN        TQTYN1           15 0
092100990223OWE  C                   MOVE      *ON           *IN82
092200990126      *
092300990126      * Reset subfile error indicators.
092400990126     C                   MOVEA     '00000000'    *IN(36)
092500990126      *
092600000728     C     XSELC         IFNE      *BLANK                                       ....If3
092700000728     C     XKPROD        ORNE      *BLANK                                       ....If3
092800000728     C     XNDESC        ORNE      *BLANK                                       ....If3
092900000728     C     XKPRIC        ORNE      *ZEROS                                       ....If3
093000000728     C     XKQTYN        ORNE      *ZEROS                                       ....If3
093100000728     C     XKVALU        ORNE      *ZEROS                                       ....If3
093200000728     C     XKVATA        ORNE      *ZEROS                                       ....If3
093300000728     C     XKSERC        ORNE      *BLANKS                                      ....If3
093400000728     C     XKSERF        ORNE      *ZEROS                                       ....If3
093500000728     C     XKSERT        ORNE      *ZEROS                                       ....If3
093600990128      *
093700990128      * Count active subfile records.
093800000728OWE  C*                    ADD  1         RRN£
093900990223OWE  C                   ADD       1             RRNS
094000990126      *
094100990126      * Validate Selection code.
094200000728     C     XSELC         IFNE      'S'
094300000728     C     XSELC         ANDNE     'N'
094400000728     C     XSELC         ANDNE     'T'
094500990127      *
094600990127      * Validate Selection code / Product code / Text combination
094700000728     C     XSELC         OREQ      *BLANK
094800000728     C     XKPROD        ANDNE     *BLANKS
094900000728     C     XSELC         OREQ      *BLANK
095000000728     C     XNDESC        ANDNE     *BLANKS
095100000728     C     XSELC         OREQ      *BLANK
095200000728     C     XKPRIC        ANDNE     0
095300000728     C     XSELC         OREQ      *BLANK
095400000728     C     XKQTYN        ANDNE     0
095500000728     C     XSELC         OREQ      'T'
095600000728     C     XKPROD        ANDNE     *BLANKS
095700000728     C     XSELC         OREQ      'T'
095800000728     C     XKPRIC        ANDNE     0
095900000728     C     XSELC         OREQ      'T'
096000000728     C     XKQTYN        ANDNE     0
096100000728     C     XSELC         OREQ      'T'
096200000728     C     XKSERC        ANDNE     *BLANKS
096300000728     C     XSELC         OREQ      'T'
096400000728     C     XKSERF        ANDNE     0
096500000728     C     XSELC         OREQ      'T'
096600000728     C     XKSERT        ANDNE     0
096700000728     C     XSELC         OREQ      'S'
096800000728     C     XKQTYN        ANDEQ     0
096900000728     C*          £SELC     OREQ 'S'
097000000728     C*          £KPRIC    ANDEQ0
097100000728     C     XSELC         OREQ      'N'
097200000728     C     XKQTYN        ANDEQ     0
097300000728     C     XSELC         OREQ      'N'
097400000728     C     XKPRIC        ANDEQ     0
097500990126     C                   MOVE      *ON           *IN36
097600000728     C                   MOVE      XYES          XERROR
097700990126     C                   MOVEL     'HSV2006'     P0MSID
097800000728     C                   EXSR      YSNDMG
097900990126     C                   ENDIF
098000990218      *
098100990218      * Validate Selection code for Invoice Charge Only order type.
098200000728     C     XSELC         IFEQ      'S'
098300990218     C     MINVC         ANDEQ     'Y'
098400990218     C                   MOVE      *ON           *IN36
098500000728     C                   MOVE      XYES          XERROR
098600990218     C                   MOVEL     'HSV2014'     P0MSID
098700000728     C                   EXSR      YSNDMG
098800990218     C                   ENDIF
098900990225      *
099000000728XXX  C     YMODE         IFEQ      'D'
099100000728XXX  C     XKQTYN        ANDGT     XXQTYN
099200000728XXX  C                   MOVE      XYES          XERROR
099300990225XXX  C                   MOVEL     'HSV2020'     P0MSID
099400000728XXX  C                   EXSR      YSNDMG
099500990225XXX  C                   LEAVE
099600990225XXX  C                   ELSE
099700000728XXX  C                   EXSR      YCLRMG
099800990225XXX  C                   ENDIF
099900990126      *
100000990126      * Validate Product.
100100000728     C     XSELC         IFEQ      'S'
100200000728     C     XKPROD        IFNE      *BLANKS
100300000728     C     XKPROD        CHAIN     HSLPRODA                           99
100400990126     C     *IN99         IFEQ      *ON
100500990126     C                   MOVE      *ON           *IN37
100600000728     C                   MOVE      XYES          XERROR
100700990126     C                   MOVEL     'HSV2007'     P0MSID
100800000728     C                   EXSR      YSNDMG
100900990127     C                   ELSE
101000000728     C                   MOVEL     NDESC         XNDESC
101100000728     C                   Z-ADD     NPRIC         XKPRIC
101200990201     C                   ENDIF
101300990201     C                   ENDIF
101400990201     C                   ENDIF
101500990127      *
101600990127      * Reverse price for Credit
101700990127     C     MSALE         IFEQ      'C'
101800000728     C                   Z-SUB     XKPRIC        XKPRIC
101900990127     C                   ENDIF
102000990127      *
102100990127      * Extended value
102200000728     C     XSELC         IFEQ      'S'
102300000728     C     XSELC         OREQ      'N'
102400000728     C     XKPRIC        MULT      XKQTYN        XKVALU
102500990218BL    *
102600000728BL   C*          £KVALU    MULT MVATP     £KVATA
102700990126     C                   ENDIF
102800990223      *
102900990223OWE   * Save Price, Value and Description field contents....
103000000728OWE  C                   MOVE(P)   XKPRIC        TKPRIC            9 2
103100000728OWE  C                   MOVE(P)   XKVALU        TKVALU            9 2
103200000728OWE  C                   MOVE(P)   XNDESC        TNDESC           19
103300990223      *
103400990211      * Check quantity against allocated serial numbers.
103500000728OOO  C*          £KQTYN    IFNE 0
103600000728OOO  C*          £KSERF    ANDNE0
103700000728OOO  C*          £KSERT    ANDNE0
103800000728OOO  C*          £KSERT    SUB  £KSERF    WKQTYN
103900990223OOO  C*                    ADD  1         WKQTYN
104000000728OOO  C*          £KQTYN    IFNE WKQTYN
104100990223OOO  C*                    MOVEA'1011'    *IN,40
104200000728OOO  C*                    MOVE £YES      £ERROR
104300990223OOO  C*                    MOVEL'HSV2013' P0MSID
104400000728OOO  C*                    EXSR ‡SNDMG
104500990223OOO  C*                    ENDIF
104600990223OOO  C*                    ENDIF
104700990127      *
104800990128      * Check voucher control if not yet allocated
104900000728     C     XSELC         IFEQ      'S'
105000000728     C     XKQTYN        ANDGT     0
105100000728OOO  C*          £KSERC    IFEQ *BLANKS
105200000728OOO  C*          £KSERF    ANDEQ0
105300000728OOO  C*          £KSERT    ANDEQ0
105400990128     C                   MOVEL     'U'           WKVCTL
105500990127     C                   EXSR      UPVCTL
105600990128     C                   ENDIF
105700990223OOO  C*                    ENDIF
105800990126      *
105900990126      * Update subfile record with DSPATR settings and set SFLRCDNBR.
106000000728     C                   Z-ADD     RRNX          RCDNBR
106100000728OWE  C                   MOVE      TKPRIC        XKPRIC
106200000728OWE  C                   MOVE      TKVALU        XKVALU
106300000728OWE  C                   MOVE      TNDESC        XNDESC
106400990127     C                   UPDATE    HSS200C                                      SFL
106500990126     C                   MOVEA     '00000000'    *IN(36)
106600990126      *
106700990126      * If errors encountered in subfile then exit subfile validation.
106800000728     C     XERROR        IFEQ      XYES
106900000728OWE  C                   MOVEL(P)  XSELC         TSELC             1
107000000728OWE  C                   MOVEL(P)  XKPROD        TPROD            13
107100000728OWE  C                   MOVEL(P)  XKPRIC        TPRIC             9 2
107200000728OWE  C                   MOVEL(P)  XKQTYN        TQTYN             9 0
107300000728OWE  C                   MOVEL(P)  XNDESC        TDESC            19
107400000728OOO  C*                    Z-ADDRRN£      RRNS    50
107500990126     C                   LEAVE
107600990126     C                   ENDIF
107700990126      *
107800990126      * End of non-blank subfile check.
107900990126     C                   ENDIF                                                  ....EndIf3
108000990126      *
108100990126      * End of Subfile CHAIN check.
108200990126     C                   ENDIF                                                  ...EndIf2
108300990126      *
108400990128      * End of loop for next subfile record.
108500990126     C                   ENDDO                                                  ..EndDo1
108600990128      *
108700990217      * Save last RRN number used.
108800000728BL   C*                    Z-ADDRRN£      SAVRRN
108900990126      *
109000990126      * If no errors found...
109100000728     C     XERROR        IFEQ      XNO
109200990126      * ...display "Data valid.Press F10 to update" message...
109300990126     C                   MOVEL     'HSV9006'     P0MSID
109400000728     C                   EXSR      YSNDMG
109500990126      * ...activate F10 key (*IN61=*ON).
109600990126     C                   MOVE      *ON           *IN61
109700990126     C                   ENDIF
109800000728XXX  C     XJUMP1        IFEQ      XYES
109900990303XXX  C     *IN52         ANDEQ     *ON
110000990303     C                   CALL      'HSR342'
110100990303     C                   ENDIF
110200990303XXX  C                   MOVE      *OFF          *IN52
110300990122     C                   ENDSR
110400990122      *
110500990122      *----------------------------------------------------------------
110600990122      * DISCNT - Discount retrieval.
110700990122      *----------------------------------------------------------------
110800990216      *
110900990122     C     DISCNT        BEGSR
111000990216      *
111100990216      * If customer discount is not entered on header screen either
111200990216      * caluclate from HSPDISC file keyed on Order Value or default
111300990216      * according to Customer Type.
111400990216      *
111500000728     C     XJDISP        IFEQ      0
111600990216      *
111700990216      * Setup key list for HSPDISC
111800990216      *
111900000728     C                   Z-ADD     XKTVAL        PFAMT
112000990216      *
112100990216      * SETLL on HSLDISCA
112200990216      *
112300990216     C     DSCKEY        SETLL     HSLDISCA                               99
112400990216      *
112500990216      * If record not found READP
112600990216      *
112700990216     C     *IN99         IFEQ      *OFF
112800990215     C                   READP     HSLDISCA                               99
112900990216      *
113000990216      * If not BOF
113100990216      *
113200990216     C     *IN99         IFEQ      *OFF
113300990223BL   C     PCTYP         ANDEQ     ECTYP
113400990215     C                   Z-ADD     PDISC         KDISP
113500990216      *
113600990216      * Else calculate discount according to Customer Type
113700990216      *
113800990215     C                   ELSE
113900990216      *
114000990215     C                   SELECT
114100990216      *
114200990216      * If Customer Type is '001' - Z-ADD 10 to discount
114300990216      *
114400990215     C     ECTYP         WHENEQ    '001'
114500990215     C                   Z-ADD     10            KDISP
114600990216      *
114700990216      * If Customer Type is '002' - Z-ADD 20 to discount
114800990216      *
114900990215     C     ECTYP         WHENEQ    '002'
115000990215     C                   Z-ADD     20            KDISP
115100990216      *
115200990215     C                   ENDSL
115300990216      *
115400990215     C                   ENDIF
115500990216      *
115600990216      * Record found on SETLL
115700990216      *
115800990216     C                   ELSE
115900990216      *
116000990216     C                   Z-ADD     PDISC         KDISP
116100990216      *
116200990216     C                   ENDIF
116300990216      *
116400000728      * £JDISP not equal to 0
116500990216      *
116600990215     C                   ELSE
116700000728     C                   Z-ADD     XJDISP        KDISP
116800990216      *
116900990215     C                   ENDIF
117000990216      *
117100990122     C                   ENDSR
117200990122      *
117300990122      *----------------------------------------------------------------
117400990122      * Check for minimum order value.
117500990122      *----------------------------------------------------------------
117600990122     C     MINVAL        BEGSR
117700990128     C                   Z-ADD     0             WKTOTV
117800990225     C                   Z-ADD     1             RRNP
117900990127      *
118000990127      * Accumulate order total value
118100990127     C     *IN99         DOUEQ     *ON
118200990225BL   C     RRNP          OREQ      100
118300000728BL   C     XSELC         OREQ      *BLANK
118400990225     C     RRNP          CHAIN     HSS200C                            99
118500990127     C     *IN99         IFEQ      *OFF
118600000728     C                   ADD       XKVALU        WKTOTV
118700990127     C                   ENDIF
118800990225     C                   ADD       1             RRNP
118900990127     C                   ENDDO
119000990127      *
119100990127      * Compare order total value and minimum order value
119200990128     C     WKTOTV        IFLT      MORDV
119300000728     C                   MOVE      XYES          XERROR
119400990201     C                   MOVEL     'HSV2011'     P0MSID
119500000728     C                   EXSR      YSNDMG
119600990201     C                   ENDIF
119700990225     C                   Z-ADD     1             RRNP
119800000728     C                   Z-ADD     1             RRNX
119900990122     C                   ENDSR
120000990122      *
120100990122      *----------------------------------------------------------------
120200990122      * Ship to all delivery points.
120300990122      *----------------------------------------------------------------
120400990122     C     SHPALL        BEGSR
120500990216     C*
120600990216     C* SETLL on HSLCUSTA with Customer Number
120700990216     C*
120800000728     C     XJCUST        SETLL     HSLCUSTA
120900990216     C*
121000990216     C* READE on HSLCUSTA with Customer Number
121100990216     C*
121200000728     C     XJCUST        READE     HSLCUSTA                               99
121300990216     C*
121400990216     C* If not EOF
121500990216     C*
121600990216     C     *IN99         DOWEQ     *OFF
121700990216     C*
121800990216     C* If ECGRP not equal to blanks
121900990216     C*
122000990216     C     ECGRP         IFNE      *BLANKS
122100990216     C*
122200990216     C* CHAIN to Customer File for Delivery Name and Address
122300990216     C*
122400990318     C*          ECGRP     CHAINALTCUSTF             13
122500990216     C*
122600990318     C*          *IN13     IFEQ *OFF
122700990216     C*
122800990216     C* Write Order Header Record
122900990216     C*
123000990216     C                   EXSR      UPORDH
123100990216     C*
123200990216     C* Set up Delivery Name and Address
123300990216     C*
123400000728     C                   MOVEL     ENAM1         XVNAM1
123500000728     C                   MOVEL     EADR1         XVADR1
123600000728     C                   MOVEL     EADR2         XVADR2
123700000728     C                   MOVEL     EADR3         XVADR3
123800000728     C                   MOVEL     EPCDE         XVPCDE
123900990216     C*
124000990216     C* Write Order Delivery Record
124100990216     C*
124200990216     C                   EXSR      UPODEL
124300990216     C*
124400990216     C* Write Order Detail Record
124500990216     C*
124600990216     C                   EXSR      UPORDD
124700990216     C*
124800990216     C* ECGRP equal to blanks
124900990216     C*
125000990318     C*                    ENDIF
125100990216     C*
125200990216     C* Customer Record not found
125300990216     C*
125400990216     C                   ENDIF
125500990216     C*
125600990216     C* Increment order number
125700990216     C*
125800000728     C     *LOCK         IN        ORDERX
125900000728     C                   ADD       1             ORDERX
126000000728     C                   OUT       ORDERX
126100000728     C                   Z-ADD     ORDERX        XJSORD
126200990216     C*
126300990216     C* READE on HSLCUSTA with Customer Number
126400990216     C*
126500000728     C     XJCUST        READE     HSLCUSTA                               99
126600990216     C*
126700990216     C                   ENDDO
126800990216     C*
126900990216     C* EOF on HSLCUSTA
127000990216     C*
127100990122     C                   ENDSR
127200990122      *
127300990122      *----------------------------------------------------------------
127400990122      * Update voucher control.
127500990122      *----------------------------------------------------------------
127600990302     C     UPVCTL        BEGSR
127700990128      *
127800990302     C                   MOVE      *ON           *IN85
127900990302      * Save order line values
128000990302      *
128100990301     C                   Z-ADD     0             XQUAN             9 0
128200000728     C                   MOVE      XNO           XJUMP             1
128300990128      * Clear flag for successful range allocation.
128400990128     C                   MOVEL     *BLANK        WKRANG
128500990301      *
128600990301      * Clear flag for first tracking record found.
128700000728     C                   MOVEL     XNO           XFIRST            1
128800990127      *
128900990127      * Get Serial number range.
129000990128     C                   MOVEL     *BLANKS       WKSERC
129100990128     C     *IN92         DOUEQ     *ON
129200990128     C     WKRANG        OREQ      'Y'
129300990223      *
129400990223OWE   * Work back through the subfile checking for duplicate
129500990223OWE   * product codes.... If found validate data under DOUB subroutine.
129600990223      *
129700000728OWE  C                   MOVE      RRNX          RRNXX             5 0
129800000728OWE  C     RRNX          DOUEQ     0
129900000728OWE  C                   SUB       1             RRNX
130000000728OWE  C     RRNX          IFEQ      0
130100990223OWE  C                   LEAVE
130200990223OWE  C                   ENDIF
130300000728OWE  C     RRNX          CHAIN     HSS200C                            90
130400000728OWE  C     XKPROD        IFEQ      TPROD1
130500000728OWE  C                   MOVE      RRNXX         RRNX
130600990223OWE  C                   EXSR      DOUB
130700990223OWE  C                   MOVE      *ON           *IN81
130800990223OWE  C                   LEAVE
130900990223OWE  C                   ENDIF
131000990223OWE  C                   ENDDO
131100990223      *
131200990223OWE   * Reset subfile position.....
131300000728OWE  C                   MOVE      RRNXX         RRNX
131400000728OWE  C     RRNX          CHAIN     HSS200C                            90
131500990223      *
131600990223      * Exit from subroutine if DOUB sr has been processed....
131700990223      *
131800990223OWE  C     *IN81         IFEQ      *ON
131900000728OWE  C                   MOVE      XKSERF        XKSERF
132000000728OWE  C                   MOVE      XKSERC        XKSERC
132100000728OWE  C                   MOVE      XKSERT        XKSERT
132200990223OWE  C                   MOVE      *OFF          *IN81
132300990223OWE  C                   LEAVE
132400990223OWE  C                   ENDIF
132500990223      *
132600990128     C     VCHKEY        SETLL     HSLVCTLA
132700000728     C     XKPROD        READE     HSLVCTLA                               92
132800990301      *
132900990301      * Move pointer file values into temporary fields....
133000990301      *
133100990301     C                   MOVE      BPROD         OPROD            13
133200990302     C                   MOVE      BSERCC        OSERC             3
133300990302     C                   MOVE      BSERNN        OSERN             9 0
133400990302     C                   Z-ADD     BSERNN        WKSERN
133500990302     C                   MOVE      BSERCC        WKSERC
133600000728     C                   Z-ADD     BSERNN        XXSERF            9 0
133700000728     C********             SUB  1         ££SERF
133800990301      *
133900990301      *
134000990127     C     *IN92         IFEQ      *OFF
134100990128      *
134200990301      * Scan voucher tracking file for first available voucher....
134300990301      *
134400000728     C                   MOVE      BSERCC        XKSERC
134500000728     C                   MOVE      OSERN         XKSERF
134600990301      *
134700990301     C     *IN94         DOUEQ     *ON
134800000728     C     XQUAN         OREQ      XKQTYN
134900990301      *
135000990303     C     *IN85         IFEQ      *ON
135100990302     C     TRCKEY        SETLL     HSLTRACB
135200990303     C                   MOVE      *OFF          *IN85
135300990303     C                   ENDIF
135400990302      *
135500000728     C     XKPROD        READE     HSLTRACB                               94
135600990301      *
135700990301     C     *IN94         IFEQ      *ON
135800990301     C                   LEAVE
135900990301     C                   ENDIF
136000990301      *
136100990301     C     ASERC         IFNE      WKSERC
136200990301     C     ASERN         ORNE      WKSERN
136300990302     C     AALLD         ORNE      *ZEROS
136400990303     C                   ADD       1             DSERN
136500990303      *
136600990303     C     *IN87         IFEQ      *OFF
136700990303     C     XXSERN        ADD       1             XXRES             9 0
136800990303PPP  C*          XXRES     IFNE ASERN
136900000728PPP  C*                    MOVE ASERN     ££SERF
137000990303PPP  C*                    ELSE
137100000728     C                   MOVE      ASERN         XXSERF
137200000728     C                   ADD       1             XXSERF
137300990303     C                   ADD       1             WKSERN
137400990303RRR  C                   MOVE      *ON           *IN85
137500000728RRR  C                   ADD       1             XKSERF
137600990303PPP  C*                    ENDIF
137700990303     C                   ELSE
137800990303      *
137900990303      * No previous error so add to HSPEXPA.....
138000990303     C                   MOVE(P)   WKSERC        XXSERC            3
138100990303     C                   MOVE(P)   WKSERN        XXSERN            9 0
138200990302     C                   SUB       1             WKSERN
138300000728     C                   MOVE      XYES          XJUMP
138400000728XXX  C                   MOVE      XYES          XJUMP1            1
138500990303      *
138600990303     C****       *IN87     IFEQ *ON
138700000728     C                   MOVE      XXSERF        USERNS
138800990301     C                   MOVE      WKSERN        USERNE
138900990301     C                   MOVE      WKSERC        USERCC
139000000728     C                   MOVE      XKPROD        UPROD
139100990302     C                   OPEN      HSLEXPAA
139200990302     C                   WRITE     HSFEXPA
139300990302     C                   CLOSE     HSLEXPAA
139400990303     C*****                ENDIF
139500990303      *
139600000728     C                   MOVE      ASERN         XXSERF
139700990303     C                   ADD       1             DSERN
139800990303     C     DSERN         IFNE      ASERN
139900990303     C     ASERN         SUB       DSERN         DIF               9 0
140000000728     C                   ADD       DIF           XXSERF
140100990303     C                   ENDIF
140200990303      *
140300000728     C                   ADD       1             XKSERF
140400990302     C                   ADD       2             WKSERN
140500990302     C                   MOVE      *ON           *IN85
140600990302     C                   MOVE      *OFF          *IN87
140700990302      *
140800990303XXX  C     ASERC         IFNE      WKSERC
140900990303XXX  C                   MOVE      ASERC         WKSERC
141000990303XXX  C                   MOVE      ASERN         WKSERN
141100000728XXX  C                   MOVE      WKSERC        XKSERC
141200000728XXX  C                   MOVE      WKSERN        XKSERF
141300000728XXX  C                   MOVE      WKSERN        XXSERF
141400990303XXX  C                   MOVE      *ON           *IN85
141500990303XXX  C                   ENDIF
141600990303      *
141700990301     C                   ITER
141800990301     C                   ENDIF
141900990303      *
142000990303     C     ASERC         IFNE      WKSERC
142100990303     C                   MOVE      ASERC         WKSERC
142200990303     C                   MOVE      ASERN         WKSERN
142300000728     C                   MOVE      WKSERC        XKSERC
142400000728XXX  C                   MOVE      WKSERN        XKSERF
142500000728XXX  C                   MOVE      WKSERN        XXSERF
142600990303     C                   MOVE      *ON           *IN85
142700990303     C                   ENDIF
142800990303      *
142900990303     C                   ENDIF
143000990301      *
143100990303      *
143200990302     C     AALLD         IFEQ      *ZEROS
143300990302     C***        *IN87     IFEQ *OFF
143400000728     C***                  ADD  1         ££SERF
143500990302     C***                  ENDIF
143600990302     C                   MOVE      *ON           *IN87
143700990301     C                   ADD       1             XQUAN
143800000728     C                   MOVE      XYES          XFIRST
143900990302     C                   MOVE      ASERC         WKSERC
144000990303     C                   MOVE(P)   ASERN         DSERN             9 0
144100990301     C                   ADD       1             WKSERN
144200000728     C                   ADD       1             XKSERF
144300990303     C                   MOVE      *ON           *IN85
144400990301     C                   ITER
144500990301     C                   ELSE
144600990303     C                   ITER
144700990301     C                   ENDIF
144800990301      *
144900990301     C                   ENDDO
145000990301      *
145100000728     C     XFIRST        IFEQ      XNO
145200000728     C                   MOVE      XYES          XERROR
145300990301     C                   MOVEL     'HSV2024'     P0MSID
145400000728     C                   EXSR      YSNDMG
145500000728     C                   CLEAR                   XKSERC
145600000728     C                   CLEAR                   XKSERT
145700000728     C                   CLEAR                   XKSERF
145800990301     C                   LEAVE
145900990301     C                   ENDIF
146000990128      *
146100000728     C     XQUAN         IFNE      XKQTYN
146200000728     C                   MOVE      XYES          XERROR
146300990301     C                   MOVEL     'HSV2025'     P0MSID
146400000728     C                   EXSR      YSNDMG
146500000728     C                   CLEAR                   XKSERC
146600000728     C                   CLEAR                   XKSERT
146700000728     C                   CLEAR                   XKSERF
146800990303     C*****                MOVE *ON       *IN52
146900990301     C                   LEAVE
147000990301     C                   ENDIF
147100990301      *
147200990303     C                   MOVE      *ON           *IN52
147300990303      *
147400000728     C     XJUMP         IFEQ      XYES
147500000728     C***                  MOVE £YES      £ERROR
147600990301     C                   MOVEL     'HSV2026'     P0MSID
147700000728     C                   EXSR      YSNDMG
147800990302      *
147900990302     C                   SUB       1             WKSERN
148000990303XXX  C                   MOVEL     'Y'           WKRANG
148100990302      *
148200000728     C*****                MOVE XXSERC    £KSERC
148300000728     C*****                MOVE XXSERN    £KSERF
148400990303     C*****      TRCKEY    CHAINHSLTRACB             99
148500990303     C*****      *IN99     IFEQ *OFF
148600000728     C*****                ADD  1         ££SERF
148700990303     C*****                ENDIF
148800990302      *
148900000728     C                   MOVE      XXSERF        USERNS
149000990302     C                   MOVE      WKSERN        USERNE
149100990302     C                   MOVE      WKSERC        USERCC
149200990302     C                   OPEN      HSLEXPAA
149300990302     C                   WRITE     HSFEXPA
149400990302     C                   CLOSE     HSLEXPAA
149500990302      *
149600000728     C                   CLEAR                   XKSERC
149700000728     C                   CLEAR                   XKSERT
149800000728     C                   CLEAR                   XKSERF
149900000728     C                   EXSR      YJUMP
150000990302     C                   MOVE      *ON           *IN86
150100990302     C                   MOVE      *ON           *IN61
150200990301     C                   LEAVE
150300990301     C                   ENDIF
150400990301      *
150500990128      * Series code found.
150600000728     C                   MOVE      BSERCC        XKSERC
150700990127      *
150800000728     C                   Z-ADD     OSERN         XKSERF
150900990302     C                   SUB       1             WKSERN
151000000728     C                   Z-ADD     WKSERN        XKSERT
151100990128      *
151200990128      * Flag successful range allocation.
151300990128     C                   MOVEL     'Y'           WKRANG
151400990128      *
151500990128      * Update if required.
151600990127     C                   ENDIF
151700990128     C                   ENDDO
151800990127      *
151900990128      * Unsuccessful range allocation.
152000990128     C     WKRANG        IFNE      'Y'
152100990128     C                   MOVEA     '111'         *IN(41)
152200000728XXX  C****                 MOVE £YES      £ERROR
152300990128     C                   MOVEL     'HSV2012'     P0MSID
152400000728     C                   EXSR      YSNDMG
152500990128     C                   ENDIF
152600990128      *
152700990122     C                   ENDSR
152800990302      *
152900990302      *----------------------------------------------------------------
153000990302      * Display contents of HSLEXPAA
153100990302      *----------------------------------------------------------------
153200990302      *
153300000728     C     YJUMP         BEGSR
153400990302     C                   ENDSR
153500990122      *
153600990302      *
153700990302      *----------------------------------------------------------------
153800990302      * Delete contents of HSLEXPAA
153900990302      *----------------------------------------------------------------
154000990302      *
154100000728     C     YDELT         BEGSR
154200990302     C                   CALL      'HSC200A'
154300990302     C                   ENDSR
154400990302      *
154500990223      *----------------------------------------------------------------
154600990223      * Repeat of Product Code in a single order
154700990223      *----------------------------------------------------------------
154800990223      *
154900990223OWE  C     DOUB          BEGSR
155000990223      *
155100990223      * Get Serial number range.
155200990223OWE  C                   MOVEL     *BLANKS       WKSERC
155300990223OWE  C     *IN92         DOUEQ     *ON
155400990223OWE  C     WKRANG        OREQ      'Y'
155500990223      *
155600990223OWE  C     VCHKEY        SETLL     HSLVCTLA
155700000728OWE  C     XKPROD        READE     HSLVCTLA                               92
155800990223OWE  C     *IN92         IFEQ      *OFF
155900990223      *
156000990223      * Get available series code.
156100000728OWEM C     BSERNN        ADD       TQTYN1        XXSERN            9 0
156200990302OWE  C*          ££SERN    IFGT BSERNE
156300990302OWE  C*                    MOVELBSERCN    WKSERC
156400990223OWE  C                   ITER
156500990223OWE  C                   ELSE
156600990223      *
156700990223      * Work back through the subfile searching for duplicate
156800990223      * product code/series code combination records......
156900990223      *
157000000728OWE  C                   MOVE(P)   RRNX          RRNXX             5 0
157100000728OWE  C     RRNX          DOUEQ     0
157200000728OWE  C                   SUB       1             RRNX
157300000728OWE  C     RRNX          IFEQ      0
157400990223OWE  C                   MOVE      *ON           *IN83
157500990223OWE  C                   LEAVE
157600990223OWE  C                   ENDIF
157700000728OWE  C     RRNX          CHAIN     HSS200C                            90
157800000728OWE  C     XKSERC        IFEQ      BSERCC
157900000728OWE  C     XKSERT        ADD       TQTYN1        RES               9 0
158000990302OWE  C*          RES       IFLE BSERNE
158100000728OWE  C                   MOVE(P)   XKSERC        KKSERC            3
158200000728OWE  C                   MOVE(P)   XKSERT        KKSERT            9 0
158300990223OWE  C                   LEAVE
158400990223OWE  C                   ELSE
158500990223OWE  C                   MOVE      *ON           *IN84
158600990223OWE  C                   LEAVE
158700990302OWE  C                   ENDIF
158800990302OWE  C*                    ENDIF
158900990223OWE  C                   ENDDO
159000990223      *
159100990223      * Reset the subfile pointer position.....
159200000728OWE  C                   MOVE(P)   RRNXX         RRNX
159300000728OWE  C     RRNX          CHAIN     HSS200C                            90
159400990223      *
159500990223OWE  C     *IN84         IFEQ      *OFF
159600990223OWE  C     *IN83         ANDEQ     *OFF
159700990223OWE   * Series code found.
159800000728OWE  C                   MOVE(P)   KKSERC        XKSERC            3
159900990223OWE   *
160000990223OWE   * Set To serial number for the order.
160100000728OWE  C                   MOVE(P)   RES           XKSERT            9 0
160200990223OWE   *
160300990223OWE   * Set From Serial Number for the order.
160400000728OWE  C     KKSERT        ADD       1             XKSERF            9 0
160500990223OWE   *
160600990223OWE   * Set Next Serial Number for the voucher control.
160700990223OWE  C                   ADD       TQTYN1        BSERNN
160800990223OWE   *
160900990223OWE   * Flag successful range allocation.
161000990223OWE  C                   MOVEL     'Y'           WKRANG
161100990223OWE  C                   ITER
161200990223OWE  C                   ENDIF
161300990223      *
161400990223OWE  C     *IN83         IFEQ      *ON
161500990223OWE  C     *IN84         ANDEQ     *OFF
161600990223OWE  C                   MOVE      *OFF          *IN83
161700990223      *
161800990223OWE   * Series code found.
161900000728OWE  C                   MOVE      BSERCC        XKSERC            3
162000990223OWE   *
162100990223OWE   * Set To serial number for the order.
162200000728OWE  C     BSERNN        ADD       TQTYN1        XKSERT            9 0
162300000728OWE  C                   SUB       1             XKSERT
162400990223OWE   *
162500990223OWE   * Set From Serial Number for the order.
162600000728OWE  C                   Z-ADD     BSERNN        XKSERF
162700990223OWE   *
162800990223OWE   * Set Next Serial Number for the voucher control.
162900990223OWE  C                   ADD       TQTYN1        BSERNN
163000990223OWE   *
163100990223OWE   * Flag successful range allocation.
163200990223OWE  C                   MOVEL     'Y'           WKRANG
163300990223OWE  C                   ITER
163400990223OWE  C                   ENDIF
163500990223OWE  C                   MOVE      *OFF          *IN84
163600990302OWE  C*                    MOVELBSERCN    WKSERC
163700990223      *
163800990302OWE  C*                    ENDIF
163900990223OWE  C                   ENDIF
164000990223OWE  C                   ENDDO
164100990223OWE  C                   ENDSR
164200990122      *----------------------------------------------------------------
164300990122      * Update voucher tracking.
164400990122      *----------------------------------------------------------------
164500990122     C     UPTRAC        BEGSR
164600990128      *
164700990128      * Reverse order date before update.
164800000728BL   C     YMODE         IFEQ      'E'
164900990215     C                   Z-ADD     JDD           WKD
165000990215     C                   Z-ADD     JMM           WKM
165100990215     C                   Z-ADD     JCC           WKC
165200990215     C                   Z-ADD     JYY           WKY
165300990215     C                   Z-ADD     WKORDD        ADFIS
165400990217BL   C                   ENDIF
165500990128      *
165600990128      * Locate voucher record.
165700990128     C     TRCKEY        SETLL     HSLTRACB
165800000728     C     XKPROD        DOUNE     APROD
165900990128     C     *IN99         OREQ      *ON
166000990128     C                   READ      HSLTRACB                               99
166100990128     C     *IN99         IFEQ      *OFF
166200990128      *
166300990128      * Exit if product code or series code does not match.
166400000728     C     XKPROD        IFNE      APROD
166500000728     C     XKSERC        ORNE      ASERC
166600990128     C                   LEAVE
166700990128     C                   ENDIF
166800990128      *
166900990128      * Update vouchers in the serial number range.
167000000728     C     XKSERF        IFLE      ASERN
167100000728     C     XKSERT        ANDGE     ASERN
167200000728BL   C     YMODE         IFEQ      'D'
167300000728BL   C                   MOVEL     XJCUST        ACUST
167400990217BL   C                   Z-ADD     0             ADSPB
167500990217BL   C                   ELSE
167600990217      *
167700000728     C                   Z-ADD     XJSORD        ASORD
167800990215     C                   Z-ADD     WKORDD        ADFIS
167900990128     C                   UPDATE    HSFTRAC
168000990217BL   C                   ENDIF
168100990128     C                   ENDIF
168200990128     C                   ENDIF
168300990128     C                   ENDDO
168400990122     C                   ENDSR
168500990122      *
168600990122      *----------------------------------------------------------------
168700990122      * Update inventory allocations.
168800990122      *----------------------------------------------------------------
168900990122     C     UPINVA        BEGSR
169000990128      *
169100990128      * Update stock quantities.
169200990205     C     IVMKEY        CHAIN     HSLINVMA                           99
169300990128     C     *IN99         IFEQ      *OFF
169400990128      *
169500990128      * Increase allocated quantity and reduce available quantity.
169600000728     C                   ADD       XKQTYN        DQTYR
169700000728     C                   SUB       XKQTYN        DQTYF
169800990128     C                   UPDATE    HSFINVM
169900990128     C                   ENDIF
170000990122     C                   ENDSR
170100990219      *
170200990219      *----------------------------------------------------------------
170300990219      * Update inventory sales.
170400990219      *----------------------------------------------------------------
170500990219BL   C     UPINVS        BEGSR
170600990219BL    *
170700990219BL    * Update stock quantities.
170800990219BL   C     IVMKEY        CHAIN     HSLINVMA                           99
170900990219BL   C     *IN99         IFEQ      *OFF
171000990219BL    *
171100990219BL    * Reduce allocated quantity and reduce on-hand quantity.
171200000728BL   C                   SUB       XKQTYN        DQTYR
171300000728BL   C                   SUB       XKQTYN        DQTYO
171400990219BL   C                   UPDATE    HSFINVM
171500990219BL   C                   ENDIF
171600990219BL   C                   ENDSR
171700990219      *
171800990122      *----------------------------------------------------------------
171900990122      * Print delivery note.
172000990122      *----------------------------------------------------------------
172100990122     C     PRTDEL        BEGSR
172200000728BL   C                   MOVEL     XJSORD        ORDPRM
172300990217BL   C                   CALL      'HSR230'
172400990217BL   C                   PARM                    ORDPRM
172500990122     C                   ENDSR
172600990122      *
172700990122      *----------------------------------------------------------------
172800990122      * Print invoice.
172900990122      *----------------------------------------------------------------
173000990122     C     PRTINV        BEGSR
173100000728BL   C                   MOVEL     XJSORD        ORDPRM
173200990217BL   C                   CALL      'HSR220'
173300990217BL   C                   PARM                    ORDPRM
173400990122     C                   ENDSR
173500990122      *
173600990122      *----------------------------------------------------------------
173700990122      * Create order details.
173800990122      *----------------------------------------------------------------
173900990122     C     UPORDD        BEGSR
174000990215      *
174100990215      * Order line already exists
174200990215     C                   MOVEL     ' '           UPDAT
174300000728     C     XOLIN         IFGT      0
174400990215     C     KEYOLN        CHAIN     HSLORDDB                           98
174500000728     C                   Z-ADD     XOLIN         KLINE
174600990215     C                   ENDIF
174700990128      *
174800990128      * Write fields for order detail record.
174900000728     C                   MOVEL     XSELC         KLTYP
175000000728     C                   MOVEL     XJCOMP        KCOMP
175100000728     C                   MOVEL     XJTYPE        KTYPE
175200000728     C                   Z-ADD     XJSORD        KSORD
175300000728     C                   Z-ADD     RRNX          KLINE
175400000728     C                   MOVEL     XJCUST        KCUST
175500000728     C                   MOVEL     XKPROD        KPROD
175600000728     C                   MOVEL     XKSERC        KSERC
175700000728     C                   Z-ADD     XKSERF        KSERNF
175800990128     C                   Z-ADD     0             KELEMF
175900000728     C                   Z-ADD     XKSERT        KSERNT
176000990128     C                   Z-ADD     0             KELEMT
176100000728     C                   Z-ADD     XKQTYN        KQTYN
176200990128     C                   Z-ADD     NCOST         KCOST
176300000728     C                   Z-ADD     XKPRIC        KPRIC
176400000728     C                   Z-ADD     XKVALU        KVALU
176500990208      *
176600990218BL    * Obtain VAT% from Order Type record.
176700000728BL   C*          £KVALU    MULT .175      KVATA
176800000728BL   C*          £KVALU    MULT MVATP     KVATA
176900990223BL   C*          KVATA     DIV  100       KVATA     H
177000990128     C                   Z-ADD     CYMD          KCRTD
177100000728     C                   Z-ADD     XXTME         KCRTT
177200000728     C                   MOVEL     XNDESC        KDESC
177300000728     C                   MOVEL     XJOBNO        KCRTS
177400000728     C                   MOVEL     XUSRID        KCRTU
177500000728     C                   MOVEL     XPGMID        KCRTP
177600990128     C                   MOVEL     *BLANK        KDELT
177700990215      *
177800990215      * Update order detail record.
177900000728     C     XOLIN         IFGT      0
178000990211     C     *IN98         IFEQ      *OFF
178100990211     C                   MOVEL     'Y'           UPDAT
178200990211     C                   UPDATE    HSFORDD
178300990211     C                   ENDIF
178400990211     C                   ELSE
178500990215      *
178600990128      * Write order detail record.
178700990128     C                   WRITE     HSFORDD
178800990211     C                   ENDIF
178900990122     C                   ENDSR
179000990122      *
179100990122      *----------------------------------------------------------------
179200990122      * Create order header.
179300990122      *----------------------------------------------------------------
179400990122     C     UPORDH        BEGSR
179500990215      *
179600990215      * Program mode - Maintenance, or Dispatch
179700000728     C     YMODE         IFEQ      'M'
179800000728     C     YMODE         OREQ      'D'
179900990215      *
180000990215      * Access order header record.
180100000728     C     XJSORD        CHAIN     HSLORDHA                           99
180200990215     C                   ENDIF
180300990128      *
180400990128      * Write fields for order header record.
180500000728     C                   MOVEL     XJCOMP        JCOMP
180600000728     C                   MOVEL     XJCUST        JCUST
180700990128     C                   MOVEL     *BLANKS       JCUSB
180800000728     C                   MOVEL     XJORDR        JORDR
180900000728     C                   MOVEL     XJTYPE        JTYPE
181000000728     C                   Z-ADD     XJSORD        JSORD
181100990128     C                   Z-ADD     WKORDD        JORDD
181200990128     C                   MOVEL     *BLANKS       JINVN
181300990128     C                   Z-ADD     0             JINVD
181400990128     C                   MOVEL     *BLANKS       JDELN
181500990128     C                   Z-ADD     0             JDELD
181600990128     C                   Z-ADD     CYMD          JCRTD
181700000728     C                   Z-ADD     XXTME         JCRTT
181800000728     C                   MOVEL     XJOBNO        JCRTS
181900000728     C                   MOVEL     XUSRID        JCRTU
182000000728     C                   MOVEL     XPGMID        JCRTP
182100990128     C                   MOVEL     *BLANK        JDELT
182200990128      *
182300990215      * Program mode - Entry
182400000728     C     YMODE         IFEQ      'E'
182500990211      *
182600990128      * Write order header record.
182700990217BL   C                   MOVEL     'A'           JSTAT
182800990217     C                   WRITE     HSFORDH
182900990211     C                   ENDIF
183000990211      *
183100990215      * Program mode - Maintenance, or Dispatch
183200000728     C     YMODE         IFEQ      'M'
183300000728     C     YMODE         OREQ      'D'
183400000728BL   C     YMODE         IFEQ      'D'
183500990225BL   C                   Z-ADD     *DATE         JDELD
183600990217BL   C                   MOVEL     'D'           JSTAT
183700990217BL   C                   ENDIF
183800990211      *
183900990211      * Update order header record.
184000990211     C     *IN99         IFEQ      *OFF
184100990211     C                   UPDATE    HSFORDH
184200990211     C                   ENDIF
184300990211     C                   ENDIF
184400990122     C                   ENDSR
184500990122      *
184600990128      *----------------------------------------------------------------
184700990128      * Create order delivery details.
184800990128     C     UPODEL        BEGSR
184900990215      *
185000990215      * Program mode - Maintenance, or Dispatch
185100000728     C     YMODE         IFEQ      'M'
185200000728     C     YMODE         OREQ      'D'
185300990215      *
185400000728     C     XJSORD        CHAIN     HSFODEL                            99
185500990215     C                   ENDIF
185600990128      *
185700990128      * Write fields for order delivery detail record.
185800000728     C                   MOVEL     XJCOMP        VCOMP
185900000728     C                   MOVEL     XJTYPE        VTYPE
186000000728     C                   Z-ADD     XJSORD        VSORD
186100000728     C                   MOVEL     XVNAM1        VNAM1
186200000728     C                   MOVEL     XVADR1        VADR1
186300000728     C                   MOVEL     XVADR2        VADR2
186400000728     C                   MOVEL     XVADR3        VADR3
186500000728     C                   MOVEL     XVPCDE        VPCDE
186600000728     C                   MOVEL     XVDIN1        VDIN1
186700000728     C                   MOVEL     XVDIN2        VDIN2
186800000728     C                   MOVEL     XVDIN3        VDIN3
186900990128     C                   MOVEL     *BLANKS       VDELT
187000990128      *
187100990215      * Program mode - Entry
187200000728     C     YMODE         IFEQ      'E'
187300990211      *
187400990211      * Write order delivery details record.
187500990211     C                   WRITE     HSFODEL
187600990211     C                   ENDIF
187700990211      *
187800990215      * Program mode - Maintenance, or Dispatch
187900000728     C     YMODE         IFEQ      'M'
188000000728     C     YMODE         OREQ      'D'
188100990211      *
188200990211      * Update order delivery details record.
188300990211     C     *IN99         IFEQ      *OFF
188400990211     C                   UPDATE    HSFODEL
188500990211     C                   ENDIF
188600990211     C                   ENDIF
188700990211      *
188800990128     C                   ENDSR
188900990128      *----------------------------------------------------------------
189000990128      * Create inventory sales transaction.
189100990128      *----------------------------------------------------------------
189200990128     C     UPINVT        BEGSR
189300990128      *
189400990128      * Write fields for inventory sales transaction record.
189500000728     C                   MOVEL     XJCOMP        CCOMP
189600000728     C                   MOVEL     XJWHSE        CWHSE
189700000728     C                   MOVEL     XKPROD        CPROD
189800990128     C                   MOVEL     *BLANKS       CSUBCF
189900990128     C                   MOVEL     *BLANKS       CSUBCT
190000990128     C                   MOVEL     *BLANKS       CVTYP
190100000728     C                   MOVEL     XKSERC        CSERC
190200000728     C                   Z-ADD     XKSERF        CSERNF
190300990128     C                   Z-ADD     0             CELEMF
190400000728     C                   Z-ADD     XKSERT        CSERNT
190500990128     C                   Z-ADD     0             CELEMT
190600000728     C                   Z-ADD     XKQTYN        CQTYN
190700990128     C                   Z-ADD     0             CBATC
190800000728BL   C                   MOVEL(P)  XJSORD        CDELN
190900990128     C                   MOVEL     *BLANKS       CREAS
191000990128     C                   Z-ADD     CYMD          CTDAT
191100990218BL   C                   MOVEL     MTTYP         CTYPE
191200990128     C                   MOVEL     *BLANKS       CSUPP
191300000728     C                   MOVEL     XJCUST        CCUST
191400990128     C                   MOVEL     *BLANKS       CAREA
191500990128     C                   MOVEL     *BLANKS       CAISL
191600990128     C                   MOVEL     *BLANKS       CBAYR
191700990128     C                   MOVEL     *BLANKS       CLEVL
191800990128     C                   MOVEL     *BLANKS       CINVN
191900990128     C                   Z-ADD     CYMD          CCRTD
192000000728     C                   Z-ADD     XXTME         CCRTT
192100000728     C                   MOVEL     XJOBNO        CCRTS
192200000728     C                   MOVEL     XUSRID        CCRTU
192300000728     C                   MOVEL     XPGMID        CCRTP
192400990128     C                   MOVEL     *BLANK        CDELT
192500990128      *
192600990128      * Write inventory sales transaction record.
192700990128     C                   WRITE     HSFINVT
192800990128     C                   ENDSR
192900990128      *
193000990107      *----------------------------------------------------------------
193100990122      * VALIDH - Validate header screen.
193200990107      *----------------------------------------------------------------
193300990107      *
193400990122     C     VALIDH        BEGSR
193500990108      *
193600990111      * Screen Header validation:
193700990114      *
193800990125      * Reset work values.
193900000728     C                   RESET                   XERROR
194000990125     C                   MOVE      *OFF          *IN61                          Hide F10/F11
194100990125      *
194200990125      * Reset error indicators.
194300000728     C                   MOVEA     XZEROS        *IN(30)
194400990125      *
194500990125      * Validate that Order Type is a valid Type.
194600000728     C     XJTYPE        CHAIN     HSLORDPA                           99
194700990125     C     *IN99         IFEQ      *ON
194800990125     C                   MOVE      *ON           *IN30
194900000728     C                   MOVE      XYES          XERROR
195000990125     C                   MOVEL     'HSV2001'     P0MSID
195100000728     C                   EXSR      YSNDMG
195200990125     C                   ENDIF
195300990125      *
195400990125      * Validate that Company is valid.
195500000728     C     XJCOMP        IFNE      *BLANKS
195600000728     C     XJCOMP        CHAIN     HSLCOMPA                           99
195700990125     C     *IN99         IFEQ      *ON
195800990125     C                   MOVE      *ON           *IN31
195900000728     C                   MOVE      XYES          XERROR
196000990125     C                   MOVEL     'HSV2002'     P0MSID
196100000728     C                   EXSR      YSNDMG
196200990125     C                   ENDIF
196300990125     C                   ENDIF
196400990125      *
196500990125      * Validate that Warehouse is valid.
196600000728     C     XJWHSE        IFNE      *BLANKS
196700000728     C     XJWHSE        CHAIN     HSLWHSEA                           99
196800990125     C     *IN99         IFEQ      *ON
196900990125     C                   MOVE      *ON           *IN32
197000000728     C                   MOVE      XYES          XERROR
197100990125     C                   MOVEL     'HSV0006'     P0MSID
197200000728     C                   EXSR      YSNDMG
197300990125     C                   ENDIF
197400990125     C                   ENDIF
197500990125      *
197600990125      * Validate Post Code
197700000728XXX  C     XJCUST        IFEQ      *BLANKS
197800000728     C     XJPCDE        IFNE      *BLANKS
197900000728     C     XJPCDE        CHAIN     HSLCUSTB                           99
198000990125     C     *IN99         IFEQ      *ON
198100990125     C                   MOVE      *ON           *IN33
198200000728     C                   MOVE      XYES          XERROR
198300990125     C                   MOVEL     'HSV2003'     P0MSID
198400000728     C                   EXSR      YSNDMG
198500990125     C                   ENDIF
198600990125      *
198700990125      * If Customer is found
198800990125     C     *IN99         IFEQ      *OFF
198900990125      * ...load Customer record into screen fields.
199000000728XXXM C                   MOVEL     ENAM1         XJNAM1                         Name 1
199100000728XXXM C                   MOVEL     ENAM2         XJNAM2                         Name 2
199200000728XXXM C                   MOVEL     EADR1         XJADR1                         Address 1
199300000728XXXM C                   MOVEL     EADR2         XJADR2                         Address 2
199400000728XXXM C                   MOVE      ECUST         XJCUST                         Cust No.
199500000728XXX  C                   MOVE      ECLIM         XJCLIM           15 0          Credit Limit
199600000728XXX  C                   MOVE      ETYSL         XJTYSL           20 5          Last Year Sales
199700990225XXX  C                   ENDIF
199800990125      *
199900990125     C                   ENDIF
200000990126     C                   ENDIF
200100990125      *
200200990125      * Validate Customer Number
200300000728XXXD C*          £JCUST    IFNE *BLANKS
200400000728     C     XJCUST        CHAIN     HSLCUSTA                           99
200500990125     C     *IN99         IFEQ      *ON
200600990125     C                   MOVE      *ON           *IN34
200700000728     C                   MOVE      XYES          XERROR
200800990125     C                   MOVEL     'HSV2004'     P0MSID
200900000728     C                   EXSR      YSNDMG
201000990125     C                   ENDIF
201100990125      *
201200990125      * If Customer is found
201300990125     C     *IN99         IFEQ      *OFF
201400990125      * ...load Customer record into screen fields.
201500000728XXXM C                   MOVEL     ENAM1         XJNAM1                         Name 1
201600000728XXXM C                   MOVEL     ENAM2         XJNAM2                         Name 2
201700000728XXXM C                   MOVEL     EADR1         XJADR1                         Address 1
201800000728XXXM C                   MOVEL     EADR2         XJADR2                         Address 2
201900000728XXXM C                   MOVE      EPCDE         XJPCDE                         Post Code
202000000728XXX  C                   MOVE      ECLIM         XJCLIM           15 0          Credit Limit
202100000728XXX  C                   MOVE      ETYSL         XJTYSL           20 5          Last Year Sales
202200990225XXX   *
202300990225XXX   * Check customer's credit rating allows order entry......
202400990225XXX  C     ECRAT         IFEQ      '01 '
202500990225XXX  C                   MOVE      *ON           *IN34
202600000728XXX  C                   MOVE      XYES          XERROR
202700990225XXX  C                   MOVEL     'HSV2018'     P0MSID
202800000728XXX  C                   EXSR      YSNDMG
202900990225XXX  C                   ENDIF
203000990205      *
203100990205     C                   ENDIF
203200990225XXXD C*                    ENDIF
203300990205      *
203400990205      * Maximum discount is 40%
203500000728     C     XJDISP        IFGT      40
203600990205     C                   MOVE      *ON           *IN36
203700000728     C                   MOVE      XYES          XERROR
203800990205     C                   MOVEL     'HSV0142'     P0MSID
203900000728     C                   EXSR      YSNDMG
204000990205     C                   ENDIF
204100990215      *
204200990128      * Check order date
204300000728     C     XJORDD        IFEQ      0
204400000728     C                   EXSR      YDATEC
204500990125     C                   ELSE
204600990215      *
204700990215      * a) Check for leap year and adjust number of days in February
204800990215      *    in Days in Month array.
204900990215     C     JYY           DIV       4             REM               4 0
205000990215     C     REM           IFEQ      0
205100990215     C                   MOVEL     'Y'           LEAP              1
205200990215     C                   ELSE
205300990215     C                   MOVEL     ' '           LEAP
205400990215     C                   ENDIF
205500990215     C                   SELECT
205600990215     C     LEAP          WHENEQ    ' '
205700990215     C                   MOVEA     28            DIM(2)
205800990215     C     LEAP          WHENEQ    'Y'
205900990215     C                   MOVEA     29            DIM(2)
206000990215     C                   ENDSL
206100990126      * Day
206200990126     C     JDD           IFGT      31
206300990126     C     JDD           ORLT      01
206400990126      * Month
206500990125     C     JMM           ORGT      12
206600990126     C     JMM           ORLT      01
206700990215      *
206800990215      * If date error found...
206900990215     C                   MOVE      *ON           *IN35
207000000728     C                   MOVE      XYES          XERROR
207100990215     C                   MOVEL     'HSV2005'     P0MSID
207200000728     C                   EXSR      YSNDMG
207300990215     C                   ENDIF
207400990215      *
207500990215      * Validate Day within month.
207600990215     C                   Z-ADD     JMM           M                 2 0
207700990215     C     JDD           IFLT      1
207800990215     C     JMM           ORGE      1
207900990215     C     JMM           ANDLE     12
208000990215     C     JDD           ANDGT     DIM(M)
208100990128      *
208200990128      * If date error found...
208300990125     C                   MOVE      *ON           *IN35
208400000728     C                   MOVE      XYES          XERROR
208500990125     C                   MOVEL     'HSV2005'     P0MSID
208600000728     C                   EXSR      YSNDMG
208700990125     C                   ENDIF
208800990125     C                   ENDIF
208900990125      *
209000990128      * If no errors found...
209100000728     C     XERROR        IFEQ      XNO
209200990125      * ...display "Data valid.Press F10 to update" message...
209300990125     C                   MOVEL     'HSV9006'     P0MSID
209400000728     C                   EXSR      YSNDMG
209500990125      * ...activate F10 key (*IN61=*ON).
209600990302     C                   MOVE      *ON           *IN61
209700990125     C                   ENDIF
209800990125      *
209900990107     C                   ENDSR
210000990125      *
210100990125      *----------------------------------------------------------------
210200000728      * ‡DATEC - Date Check Routine
210300990125      *----------------------------------------------------------------
210400990125      *
210500000728     C     YDATEC        BEGSR
210600990125      *
210700990128      * Convert six-digit system date to eight digits, with century.
210800990125     C                   MOVEL     UDAY          DD
210900990125     C                   MOVEL     UMONTH        MM
211000990125     C                   MOVEL     UYEAR         YY
211100990125     C     YY            IFGT      80
211200990125     C                   Z-ADD     19            CC
211300990125     C                   ELSE
211400990125     C                   Z-ADD     20            CC
211500990125     C                   ENDIF
211600990223     C                   MOVEL     CC            CCYY              4 0
211700990223     C                   MOVE      YY            CCYY
211800990128      *
211900990128      * Set default date for order entry.
212000000728     C     XJORDD        IFEQ      0
212100000728     C                   Z-ADD     DMCY          XJORDD
212200990125     C                   ENDIF
212300990128      *
212400990128      * Set date for transaction audit field.
212500990128     C                   Z-ADD     DD            DDD
212600990128     C                   Z-ADD     MM            MMM
212700990128     C                   Z-ADD     CC            CCC
212800990128     C                   Z-ADD     YY            YYY
212900990125     C                   ENDSR
213000990125      *
213100990115      *----------------------------------------------------------------
213200000728      * ‡SERCH - F4 prompt control.
213300990115      *----------------------------------------------------------------
213400990115      *
213500000728     C     YSERCH        BEGSR
213600990115      *
213700990115      * Check that cursor is in a valid field for prompting.
213800990126     C     CSRRCD        IFEQ      'HSS200A'
213900000728     C     CSRFLD        ANDEQ     'XJCUST'
214000990125     C     CSRRCD        OREQ      'HSS200A'
214100000728     C     CSRFLD        ANDEQ     'XJTYPE'
214200990126     C     CSRRCD        OREQ      'HSS200A'
214300000728     C     CSRFLD        ANDEQ     'XJCOMP'
214400990125     C     CSRRCD        OREQ      'HSS200A'
214500000728     C     CSRFLD        ANDEQ     'XJWHSE'
214600990126     C     CSRRCD        OREQ      'HSS200A'
214700000728     C     CSRFLD        ANDEQ     'XJPCDE'
214800990127     C     CSRRCD        OREQ      'HSS200C'
214900000728     C     CSRFLD        ANDEQ     'XKPROD'
215000990126      *
215100990119      * Move appropriate data to parameter fields depending upon search
215200990119      * field chosen.
215300990119     C                   SELECT
215400990119      *
215500990119      * Customer No.
215600000728     C     CSRFLD        WHENEQ    'XJCUST'
215700000728     C                   MOVEL(P)  'HSLCUSTA'    FILEY
215800000728     C                   MOVEL(P)  'ENAM1'       NAMEY
215900000728     C                   MOVEL(P)  'ECUST'       NUMBRY
216000990125      *
216100990125      * Post Code
216200000728     C     CSRFLD        WHENEQ    'XJPCDE'
216300000728     C                   MOVEL(P)  'HSLCUSTB'    FILEY
216400000728     C                   MOVEL(P)  'ENAM1'       NAMEY
216500000728     C                   MOVEL(P)  'EPCDE'       NUMBRY
216600990119      *
216700990125      * Order Type.
216800000728     C     CSRFLD        WHENEQ    'XJTYPE'
216900000728     C                   MOVEL(P)  'HSLORDPA'    FILEY
217000000728     C                   MOVEL(P)  'MODES'       NAMEY
217100000728     C                   MOVEL(P)  'MOTYP'       NUMBRY
217200990119      *
217300990125      * Company Code
217400000728     C     CSRFLD        WHENEQ    'XJCOMP'
217500000728     C                   MOVEL(P)  'HSLCOMPA'    FILEY
217600000728     C                   MOVEL(P)  'WODES'       NAMEY
217700000728     C                   MOVEL(P)  'WCOMP'       NUMBRY
217800990126      *
217900990126      * Warehouse Code
218000000728     C     CSRFLD        WHENEQ    'XJWHSE'
218100000728     C                   MOVEL(P)  'HSLWHSEA'    FILEY
218200000728     C                   MOVEL(P)  'PODES'       NAMEY
218300000728     C                   MOVEL(P)  'PWHSE'       NUMBRY
218400990125      *
218500990126      * Product Code
218600000728     C     CSRFLD        WHENEQ    'XKPROD'
218700000728     C                   MOVEL(P)  'HSLPRODA'    FILEY
218800000728     C                   MOVEL(P)  'NDESC'       NAMEY
218900000728     C                   MOVEL(P)  'NPROD'       NUMBRY
219000990125      *
219100990119     C                   ENDSL
219200990115      *
219300990115      * Call system window program to retrieve data.  If Return Code is
219400990115      * *OFF then load retrieved data into screen fields.
219500000728     C                   Z-ADD     10            LINY
219600000728     C                   Z-ADD     8             COLY
219700990115     C                   CALL      'HSR341'      PL341
219800990119      *
219900990119      * If Return Code is *OFF then load retrieved data into
220000990119      * appropriate screen fields.
220100000728     C     YRTNC         IFEQ      *OFF                                         ..If2
220200990119      *
220300990119     C                   SELECT
220400990119      *
220500990119      * Customer No.
220600000728     C     CSRFLD        WHENEQ    'XJCUST'
220700000728     C                   MOVEL(P)  YNUMBR        XJCUST
220800990125      *
220900990125      * Post Code
221000000728     C     CSRFLD        WHENEQ    'XJPCDE'
221100000728     C                   MOVEL(P)  YNUMBR        XJPCDE
221200990119      *
221300990119      * Customer Type.
221400000728     C     CSRFLD        WHENEQ    'XJTYPE'
221500000728     C                   MOVEL(P)  YNUMBR        XJTYPE
221600990125      *
221700990125      * Company Code
221800000728     C     CSRFLD        WHENEQ    'XJCOMP'
221900000728     C                   MOVEL(P)  YNUMBR        XJCOMP
222000990125      *
222100990125      * Warehouse Code
222200000728     C     CSRFLD        WHENEQ    'XJWHSE'
222300000728     C                   MOVEL(P)  YNUMBR        XJWHSE
222400990126      *
222500990126      * Product Code
222600000728     C     CSRFLD        WHENEQ    'XKPROD'
222700000728     C                   MOVEL(P)  YNUMBR        XKPROD
222800990119      *
222900990119     C                   ENDSL
223000990121     C                   ENDIF                                                  ..EndIf2
223100990115      *
223200990119      * otherwise cursor is in wrong format/field so display message.
223300990121     C                   ELSE                                                   .ElseIf1
223400990115     C                   MOVEL     'HSV9009'     P0MSID
223500000728     C                   EXSR      YSNDMG
223600990119     C                   ENDIF                                                  .EndIf1
223700990115      *
223800990115     C                   ENDSR
223900990106      *
224000990127      *----------------------------------------------------------------
224100000728      * ‡SERSF - F4 prompt control for order detail subfile.
224200990127      *----------------------------------------------------------------
224300990127      *
224400000728     C     YSERSF        BEGSR
224500990127      * Clear row / column control.
224600990127     C                   Z-ADD     0             CSRROW
224700990127     C                   Z-ADD     0             CSRCOL
224800990127      *
224900990127      * Retrieve the current cursor position.
225000990127     C     CSRLOC        DIV       256           ROW               2 0
225100990127     C                   MVR                     COL               2 0
225200990127     C                   Z-ADD     ROW           CSRROW
225300990127     C                   Z-ADD     COL           CSRCOL
225400990127      *
225500990127      * Execute search.
225600000728     C                   EXSR      YSERCH
225700990127      *
225800990127      * Flag subfile record to return value.
225900990127     C     CSRRRN        CHAIN     HSS200C                            99
226000990127     C     *IN99         IFEQ      *OFF
226100990127     C                   MOVEL     *ON           *IN37
226200000728XXX  C     YNUMBR        IFNE      *BLANKS
226300000728     C                   MOVEL(P)  YNUMBR        XKPROD
226400990225XXX  C                   ENDIF
226500990127     C                   UPDATE    HSS200C
226600990127     C                   MOVEL     *OFF          *IN37
226700990127     C                   ENDIF
226800990127     C                   ENDSR
226900990127      *
227000990106      *----------------------------------------------------------------
227100000728      * ‡SNDMG - Send message to this program's MSGQ.
227200990106      *----------------------------------------------------------------
227300990106      *
227400000728     C     YSNDMG        BEGSR
227500990106      *
227600990107     C                   CALL      'SNDMSGC'
227700990106     C                   PARM                    P0PGMQ                         Program queue
227800990106     C                   PARM                    P0PGRL                         Rel queue
227900990106     C                   PARM                    P0MSID                         Message ID
228000990106     C                   PARM                    P0MSGF                         Message file
228100990106     C                   PARM                    P0MSDA                         Message data
228200990106     C                   PARM                    P0MSTP                         Message type
228300990106      *
228400990106     C                   ENDSR
228500990106      *
228600990106      *----------------------------------------------------------------
228700000728      * ‡CLRMG - Clear message(s) from this program's MSGQ.
228800990106      *----------------------------------------------------------------
228900990106      *
229000000728     C     YCLRMG        BEGSR
229100990106      *
229200990107     C                   CALL      'RMVMSGC'
229300990106      *
229400990106     C                   ENDSR
229500990106      *
229600990223      *----------------------------------------------------------------
229700990223      * *UPPVCT- Update the voucher control
229800990223      *----------------------------------------------------------------
229900990223      *
230000990223     C     UPPVCT        BEGSR
230100990225     C                   Z-ADD     1             RRNQ
230200990225     C     RRNQ          CHAIN     HSS200C                            97
230300990223     C     *IN97         DOWEQ     *OFF
230400990223      *
230500990225     C                   ADD       1             RRNQ
230600000728     C     XSELC         IFNE      *BLANK
230700000728     C     XKPROD        ORNE      *BLANK
230800000728     C     XNDESC        ORNE      *BLANK
230900000728     C     XKPRIC        ORNE      *ZEROS
231000000728     C     XKQTYN        ORNE      *ZEROS
231100000728     C     XKVALU        ORNE      *ZEROS
231200000728     C     XKVATA        ORNE      *ZEROS
231300000728     C     XKSERC        ORNE      *BLANKS
231400000728     C     XKSERF        ORNE      *ZEROS
231500000728     C     XKSERT        ORNE      *ZEROS
231600000728     C                   MOVE      XKSERC        WKSERC
231700000728     C                   MOVE      XKSERT        TEMP              9 0
231800990223     C                   ADD       1             TEMP
231900990223     C     VCHKEY        SETLL     HSLVCTLA
232000000728     C     XKPROD        READE     HSLVCTLA                               92
232100990223     C                   MOVE      TEMP          BSERNN
232200990223     C                   UPDATE    HSFVCTL
232300990223     C                   ENDIF
232400990223      *
232500990225     C     RRNQ          IFEQ      100
232600990223     C                   LEAVE
232700990223     C                   ENDIF
232800990225     C     RRNQ          CHAIN     HSS200C                            97
232900990223     C                   ENDDO
233000990223     C                   ENDSR
233100990223      *
233200990106      *----------------------------------------------------------------
233300990106      * *INZSR - Initialization.
233400990106      *----------------------------------------------------------------
233500990106      *
233600990106     C     *INZSR        BEGSR
233700990217BL    *
233800990217BL    * Dummy read to open file.
233900990217BL   C                   READ      HSPINVT                                99
234000990222BL   C                   UPDATE    HSFINVT
234100990302     C                   OPEN      HSLEXPAA
234200990302BL   C                   READ      HSLEXPAA                               99
234300990302     C                   CLOSE     HSLEXPAA
234400990127      *
234500990106      * Key lists.
234600990128      * Key List for Voucher Control.
234700990127     C     VCHKEY        KLIST
234800000728     C                   KFLD                    XKPROD
234900990128     C                   KFLD                    WKSERC
235000990223      *
235100990223      * Key List for Voucher Cross-Reference.
235200990223     C     KEYXRF        KLIST
235300990223     C                   KFLD                    CCYY
235400000728     C                   KFLD                    XJCUST
235500000728     C                   KFLD                    XKPROD
235600990205      *
235700990211      * Key List for Sales Order.
235800990211     C     KEYOLN        KLIST
235900000728     C                   KFLD                    XJSORD
236000000728     C                   KFLD                    XOLIN
236100990211      *
236200990205      * Key List for Inventory Masterfile.
236300990205     C     IVMKEY        KLIST
236400000728     C                   KFLD                    XKPROD
236500990205     C                   KFLD                    WKSUBC
236600990205     C                   MOVE      *BLANKS       WKSUBC
236700990115      *
236800990128      * Key List for Voucher Tracking.
236900990128     C     TRCKEY        KLIST
237000000728     C                   KFLD                    XKPROD
237100000728     C                   KFLD                    XKSERC
237200000728     C                   KFLD                    XKSERF
237300990215      *
237400990301      * Key List for Customer Discount File.
237500990215     C     DSCKEY        KLIST
237600990215     C                   KFLD                    ECTYP
237700990215     C                   KFLD                    PFAMT
237800990128      *
237900990115      * Parameter Lists.
238000990203      * Entry parameter list.
238100990203     C     *ENTRY        PLIST
238200000728     C                   PARM                    YMODE             1            Mode
238300000728     C                   PARM                    YORDNO            8            Sales Order No.
238400000728     C                   PARM                    YTYPE             3            Order Type
238500000728     C                   PARM                    YCUST             8            Customer No.
238600990203      *
238700990115      * List for Call to HSR341 - Search program.
238800990115     C     PL341         PLIST
238900000728     C                   PARM                    YRTNC             1
239000000728     C                   PARM                    YNUMBR           15
239100000728     C                   PARM                    FILEY            10
239200000728     C                   PARM                    NAMEY             6
239300000728     C                   PARM                    NUMBRY            6
239400000728     C                   PARM                    LINY              3 0
239500000728     C                   PARM                    COLY              3 0
239600990127      *
239700990127      * Field definitions
239800000728     C     *LIKE         DEFINE    XKVALU        WKTOTV
239900000728     C     *LIKE         DEFINE    XKSERC        WKSERC
240000990128     C     *LIKE         DEFINE    BSERNN        WKSERN
240100000728     C     *LIKE         DEFINE    XKQTYN        WKQTYN
240200990211     C     *LIKE         DEFINE    DSUBC         WKSUBC
240300000728     C     *LIKE         DEFINE    XNO           WKRANG
240400000728     C     *LIKE         DEFINE    XNO           WKVCTL
240500000728     C     *LIKE         DEFINE    XNO           UPDAT
240600990106      *
240700990106      * Variable declarations.
240800000728     C                   MOVE      '0'           XNO               1
240900000728     C                   MOVE      '1'           XYES              1
241000000728     C                   MOVE      XNO           XERROR            1
241100000728     C                   MOVE      XNO           XUPDAT            1
241200000728     C                   MOVE      XNO           XDELET            1
241300990217BL   C                   MOVEL     *BLANKS       ORDPRM            8
241400990126     C                   Z-ADD     1             RCDNBR
241500000728     C                   Z-ADD     0             RRNX              5 0
241600990211     C                   Z-ADD     0             RRNVAL            5 0
241700990225     C                   Z-ADD     0             RRNLIN            5 0
241800990225     C                   Z-ADD     0             RRNQ              5 0
241900990225     C                   Z-ADD     0             RRNP              5 0
242000990106      *
242100990106      * Prepare message subfile.
242200990106     C                   MOVE      *ON           *IN79
242300000728     C                   MOVE      XPGMID        P0PGMQ           10            Program queu
242400990111     C                   MOVEL     '*PRV'        P0PGRL            5            Rel queue
242500990106     C                   MOVE      *BLANKS       P0MSID            7            Message ID
242600990106     C                   MOVEL     'HSM001'      P0MSGF           10            Message file
242700990106     C                   MOVE      *BLANKS       P0MSDA          132            Message data
242800990106     C                   MOVEL     '*INFO'       P0MSTP            7            Message type
242900990204      *
243000000728     C                   EXSR      YDATEC
243100990106      *
243200990106     C                   ENDSR
243300990215** Days in Month array DIM.
243400990215312831303130313130313031
