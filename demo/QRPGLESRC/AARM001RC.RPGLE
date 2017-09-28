000001888888      *%CSTD===========================================================*
000002888888      ** Application. : ARC ARCAD                                      *
000003888888      ** Composant. . : AARM001RC                     Type: RPGLE      *
000004888888      **===============================================================*
000005888888      ** Sous-système : CCM CM - Change mngmt = Skipper - Gestion de   *
000006888888      ** Fonction . . : VER Gestion des versions                       *
000007888888      ** Sous-fonction: OPN Ouverture d'une version                    *
000008888888      **%S=============================================================*
000009888888      ** Description des fonctionnalités:                              *
000011888888      **                                                               *
000012888888      **                                                               *
000013888888      **                                                               *
000014888888      **%E=============================================================*
000015888888      ** AUTEUR:    GILBERT    19/11/2009 03:41  09.00.10              *
000016888888      ** MODIFS: 01 APASQUALIN 04/01/2011 14:31  09.00.20 FM N°10/0305 *
000017888888      **           Modification après transformation unicode           *
000018888888      **         ** GILBERT    19/11/2009 03:41  09.00.10 FM N°09/0422 *
000019888888      **           Application Renommage des zones et composants       *
000020888888      **           ======> ARM003RC   renommé en   AARM001RC   <====== *
000021888888      **         13 GILBERT    24/02/2009 07:10  09.00.03 FM N°08/1266 *
000022888888      **           Agrandissement de zones (projet UNICODE et projets  *
000023888888      **         12 HELENEG    23/01/2008 16:18  08.07.02 FM N°08/0063 *
000024888888      **           2008/00010 Mettre les commentaires des sources liv  *
000025888888      **         12 HELENEG    23/01/2008 16:18  08.07.02 FM N°08/0073 *
000026888888      **           FA 07/0705 CHKVER ne verifie pas la cloture d'une   *
000027888888      **         11 GILBERT    22/03/2007 15:46  08.06.02 FM N°07/0341 *
000028888888      **           FA 07/0247 OPNVER avec incrém. n° pas ok si forme   *
000029888888      **         10 YBOSSE     29/12/2006 17:55  08.06.00 FM N°03/0032 *
000030888888      **           DM 02/0169 Nb de bibliothèques illimité             *
000031888888      **         10 YBOSSE     29/12/2006 17:55  08.06.00 FM N°06/1043 *
000032888888      **           DM 06/0347 FUSION ARCAPPF1 / ARCPAPF1               *
000033888888      **         09 GILBERT    11/07/2006 12:34    8.05.A FM N°06/0354 *
000034888888      **           FA 06/0374 OPNVER plante si ouverture au préalable  *
000035888888      **         08 GILBERT    01/10/2003 17:10    7.05.A FM N°03/0344 *
000036888888      **           FA 03/0160 Incrémental : OPNVER *GEN sur dernière   *
000037888888      **         08 GILBERT    01/10/2003 17:10    7.05.A FM N°03/0489 *
000038888888      **           FA 03/0512 INCREMENTAL : plantage CRTVERLST         *
000039888888      **         07 GILBERT    04/08/2003 10:13    7.01.I FM N°03/0344 *
000040888888      **           FA 03/0160 Incrémental : OPNVER *GEN sur dernière   *
000041888888      **         06 MICHEL     04/08/2003 00:00    6.02.J    N°00/     *
000042888888      **         05 MICHEL     22/11/1999 15:09    6.01.B FM N°99/0763 *
000043888888      **           FA 99/0034 DLTVER ne suprimme pas les suppressions  *
000044888888      **         04 FREDERIC   11/05/1999 17:36    5.01.C FM N°99/0365 *
000045888888      **           Incrémentation des versions non standards ARCAD     *
000046888888      **         03 PIERRE     11/05/1999 00:00    5.00.A    N°00/     *
000047888888      **         02 GILBERT    01/09/1997 11:48    4.05.H FM N°97/0299 *
000048888888      **           OPNVER pb n° de version (item) devenait $ après Z   *
000049888888      **         01 MAGNE      01/09/1997 00:00    4.00.G    N°00/     *
000050888888      *%ECSTD==========================================================*
005000091119      *%LOC: SRC=*L02 OBJ=*L02
005100891114     H****************************************************************
005200891114     H*         APPLICATION: ARCAD                                   *
005300091119     H*           PROGRAMME: AARM001RC                               *
005400891114     H* FONCTION: PROGRAMME D'INCREMENTATION DU N° DE VERSION        *
005500891114     H*    AUTEUR: MAGNE       14/11/89   9:16:41   VERSION: V 1.01  *
005600891114     H****************************************************************
005700091119     FAARCAPPF1 IF   E           K DISK
005800091119     FAARMVERL7 IF   E           K DISK
005900091119     FAARMMSOLC IF   E           K DISK
006000091119     FAARMMOBL3 IF   E           K DISK
006100100627K8K CD a_CALPH1        S              1C   DIM(61) CTDATA PERRCD(1)
006200100627K8K CD a_CALPH2        S              1C   DIM(61) ALT(a_CALPH1)
006300891114     D*------------------------------+
006400891114     D*      D    D    S             !
006500891114     D*------------------------------+
006600091119     D  Ds_CVer        DS
006700070322     D*----
006800100627K8K CD  CPV                    1      2C
006900100627K8K CD  CVERPLU                3      4C
007000110104K8K CD* NVERPLD                3      4  0
007100110104K8K CD* NVER                   5      6  0
007200100627K8K CD  CVER                   5      6C
007300110104K8K CD* NREL                   9     12  0
007400100627K8K CD  CREL                   9     12C
007500100627K8K CD  CITEM                 15     16C
007600990511     D*
007700070322     D*----
007800110104K8K CD* NVer2                  1      2  0
007900100627K8K CD  CVER2                  1      2C
008000110104K8K CD* NREL2                  5      8  0
008100100627K8K CD  CREL2                  5      8C
008200100627K8K CD  CITEM2                11     12C
008300110104K8K CD* NITEM2p               13     16  0
008400100627K8K CD  CITEM2p               13     16C
008500070322     D*----
008600100627K8K CD  CVER31                 1      2C
008700100627K8K CD  CVER32                 3      4C
008800110104K8K CD* NVER3                  1      4  0
008900100627K8K CD  CVER3p                 5      6C
009000100627K8K CD  CREL3                  7     10C
009100110104K8K CD* NREL3                  7     10  0
009200100627K8K CD  CREL3p                11     12C
009300110104K8K CD* NITEM3                13     16  0
009400100627K8K CD  CITEM3                13     16C
009500110104     D
009600100627K8K ID CVer01                         8C   Overlay(Ds_CVer:1)
009700110104     D*
009800110104     D  Ds_WVer1       DS
009900110104K8K CD  AVERPLU                2      2
010000110104K8K CD  NVERPLD                2      2  0
010100110104K8K CD  AVER                   3      3
010200110104K8K CD  NVER                   3      3  0
010300110104K8K CD  AREL                   5      6
010400110104K8K CD  NREL                   5      6  0
010500110104     D*
010600110104     D  Ds_WVer2       DS
010700110104K8K CD  AVER2                  1      1
010800110104K8K CD  NVer2                  1      1  0
010900110104K8K CD  AREL2                  3      4
011000110104K8K CD  NREL2                  3      4  0
011100110104K8K CD  AITEM2p                7      8
011200110104K8K CD  NITEM2p                7      8  0
011300110104     D*
011400110104     D  Ds_WVer3       DS
011500110104K8K CD  AVER31                 1      1
011600110104K8K CD  AVER32                 2      2
011700110104K8K CD  NVER3                  1      2  0
011800110104K8K CD  AREL3                  4      5
011900110104K8K CD  NREL3                  4      5  0
012000110104K8K CD  AITEM3                 7      8
012100110104K8K CD  NITEM3                 7      8  0
012200030804     D*
012300080123     D* FRA: Derni}re version en r{f{rence
012400080123     D* ENG: Last Version in reference
012500091119     D  CLASTVER       S                     Like(CRtnVer)
012600070322     D*
012700091119     D NMethod         S              1  0
012800100627K8K CD e_CInc          s              5C
012900100627K8K CD e_CREFVER       s              8C
013000100627K8K CD e_CVer          s              8C
013100100627K8K CD e_CRtn          s              1C
013200100627K8K CD CRtnVer         s              8C
013300091119     D NIdxI           s              2P 0
013400080123     C*----------------------------------+
013500080123     C* FRA:         C L E S             !
013600080123     C* ENG:         K E Y S             !
013700080123     C*----------------------------------+
013800940119     C     KAPP          KLIST
013900091119µ92 VC                   KFLD                    APP_CApp
014000940119     C     KVER          KLIST
014100091119µ92 VC                   KFLD                    APP_CApp
014200100627K8K CC                   KFLD                    CVer01
014300080123     C*-----------------------------------+
014400080123     C* FRA:     P A R A M E T R E S      !
014500080123     C* ENG:     P A R A M E T E R S      !
014600080123     C*-----------------------------------+
014700891114     C     *ENTRY        PLIST
014800091119µ92 VC                   PARM                    APP_CApp
014900091119     C                   PARM                    e_CInc
015000091119     C                   PARM                    e_CREFVER
015100091119     C                   PARM                    e_CVer
015200091119     C                   PARM                    e_CRtn
015300080123     C*----------------------------------+
015400091119     C* FRA:    T R A NIdxI T E M E N T      !
015500091119     C* ENG:    P R O C E S S NIdxI N G      !
015600080123     C*----------------------------------+
015700091119X701 C                   Eval      e_CRtn = *Blanks
015800940818     C*
015900091119     C     KAPP          CHAIN     AARCAPPF1                          50
016000091119X241 C                   If        *IN50 = '1'
016100091119X241 C                   Goto      FIN
016200091119X241 C                   Endif
016300990511     C*
016400030804     C*
016500080123     C* FRA: RECUP DU NIVEAU ACTUEL DE LA REFERENCE: DERNIERE VERSION INTEGREE DANS
016600080123     C*      LA REFERENCE
016700080123     C*      (ou dernière en prod sur une branche incrémentale)
016800080123     C* ENG: RETRIEVE CURRENT REFERENCE LEVEL: THE LAST VERSION INTEGRATED INTO
016900080123     C*      THE REFERENCE
017000080123     C*      (or the last version in production on a branch in incremental node)
017100091119     C                   CALL      'AARC012R2'                          80
017200091119µ92 VC                   PARM                    APP_CApp
017300091119     C                   PARM                    e_CREFVER
017400091119     C                   PARM                    CRtnVer
017500030804     C*
017600091119     C                   Eval      CLASTVER = CRtnVer
017700030804     C*
017800091119X201 C                   If        CLASTVER = *BLANK
017900091119X751 C                   Eval      e_CVer = 'V 0.00.A'
018000030804     C                   GOTO      FIN
018100091119     C                   Endif
018200080123     C***********************************************************
018300080123     C* FRA: RÉCUPÉRATION DU PREMIER CARACTÈRE DU N° DE VERSION *
018400080123     C* ENG: RETRIEVE THE FIRST CHARACTER OF THE VERSION Nb.    *
018500080123     C***********************************************************
018600110103X751 C                   Eval      CVer01 = CLASTVER
018700990511     C*
018800080123     C*******************************************************
018900080123     C* FRA: CAS STANDARD 1 : VERSION EN 'V 8.99.A'         *
019000080123     C* ENG: STANDARD CASE £1: 'V 8.99.A' VERSION Nb TYPE   *
019100080123     C*******************************************************
019200070322     C                   Select
019300091119X231 C                   When      CPV = 'V'
019400091119     C                   Eval      NMethod = 1
019500110104     C                   Eval      AVerPlu = CVerPlu
019600110104     C                   Eval      AVer = CVer
019700110104     C                   Eval      ARel = CRel
019800080123     C* FRA: Vérif. N° de version cohérent (V 8.99.A)
019900080123     C* ENG: Verify the version number consistency (V 8.99.A)
020000091119     C                   If        CVER >= '0' and CVER <= '9'
020100091119     C                             and CREL >= '00' and CREL <= '99'
020200091119     C                             and CITEM >= 'A' and CITEM <= 'Z'
020300060711     C                   Else
020400091119X751 C                   Eval      e_CVer = CLASTVER
020500091119X751 C                   Eval      e_CRtn = '4'
020600060711     C                   GOTO      FIN
020700091119     C                   EndIf
020800920706     C*
020900940119     C     LOOP          TAG
021000091119X201 C                   If        e_CInc = '*VER'
021100110104     C*
021200940929     C     VERTAG        TAG
021300091119X201 C                   If        NVER = 9
021400091119X201 C                   If        CVERPLU = ' '
021500091119X751 C                   Eval      CVERPLU = '1'
021600940929     C                   ELSE
021700091119X201 C                   If        NVERPLD = 9
021800091119X751 C                   Eval      e_CRtn = '1'
021900940818     C                   GOTO      FIN
022000091119     C                   Endif
022100091119X007 C                   Eval      NVER = -1
022200091119X006 C                   Eval      NVERPLD += 1
022300091119     C*                    MOVE NVERPLD   CVERPLU
022400091119     C                   Endif
022500091119     C                   Endif
022600940929     C*
022700091119X006 C                   Eval      NVER += 1
022800091119X007 C                   Eval      NREL = 00
022900091119X751 C                   Eval      CITEM = 'A'
023000940929     C                   GOTO      CHK
023100091119     C                   Endif
023200940929     C*
023300091119X201 C                   If        e_CInc = '*REL'
023400110104     C*
023500940929     C     RELTAG        TAG
023600091119X201 C                   If        NREL = 99
023700091119X751 C                   Eval      e_CRtn = '2'
023800940929     C                   GOTO      VERTAG
023900091119     C                   Endif
024000091119X006 C                   Eval      NREL += 1
024100091119X751 C                   Eval      CITEM = 'A'
024200940929     C                   GOTO      CHK
024300091119     C                   Endif
024400940929     C*
024500091119X201 C                   If        e_CInc = '*ITEM'
024600091119X201 C                   If        CITEM = 'Z'
024700091119X751 C                   Eval      e_CRtn = '2'
024800940929     C                   GOTO      RELTAG
024900091119     C                   Endif
025000091119X007 C                   Eval      NIdxI = 1
025100091119X511 C                   Eval      NIdxI = %Lookup(CITEM : a_CALPH1 : NIdxI)
025200091119X511 C                   Eval      *IN88 = (NIdxI > 0)
025300091119X201 C                   If        *IN88 = *ON
025400091119     C*          a_CALPH2,NIdxI IFEQ '$'
025500091119     C*                    MOVE '2'       e_CRtn
025600991122     C*                    GOTO RELTAG
025700991122     C*                    ENDIF
025800091119X751 C                   Eval      CITEM = a_CALPH2(NIdxI)
025900990511     C                   GOTO      CHK
026000960226     C                   ELSE
026100110103X751 C                   Eval      e_CVer = CVer01
026200091119X751 C                   Eval      e_CRtn = '3'
026300960226     C                   GOTO      FIN
026400091119     C                   Endif
026500091119     C                   Endif
026600990511     C*
026700080123     C*****************************************************
026800080123     C* FRA: CAS STANDARD 3 : VERSION EN '77.88.99'       *
026900080123     C* ENG: STADARD CASE £ 3: '77.88.99' VERSION Nb TYPE *
027000080123     C*****************************************************
027100091119     C                   When      CVER31 >= '0' and CVER31 <= '9'
027200091119     C                               and CVER32 >= '0' and CVER32 <= '9'
027300091119     C                   Eval      NMethod = 3
027400110104     C                   Eval      AVer31 = CVer31
027500110104     C                   Eval      AVer32 = CVer32
027600110104     C                   Eval      ARel3 = CRel3
027700110104     C                   Eval      AItem3 = CItem3
027800080123     C* FRA: Vérif. N° de version cohérent (V 8.99.A)
027900080123     C* ENG: Verify version number consistency (V 8.99.A)
028000091119     C                   If        CVER3p = '.' and CREL3p = '.'
028100091119     C                             and CREL3 >= '00' and CREL3 <= '99'
028200091119     C                             and CITEM3 >= '00' and CITEM3 <= '99'
028300070322     C                   Else
028400091119X751 C                   Eval      e_CVer = CLASTVER
028500091119X751 C                   Eval      e_CRtn = '4'
028600070322     C                   GOTO      FIN
028700091119     C                   EndIf
028800070322     C*
028900070322     C     LOOP3         TAG
029000091119X201 C                   If        e_CInc = '*VER'
029100110104     C*
029200070322     C     VER3TAG       TAG
029300091119X201 C                   If        NVER3 = 99
029400091119X751 C                   Eval      e_CRtn = '1'
029500070322     C                   GOTO      FIN
029600091119     C                   Endif
029700091119X006 C                   Eval      NVER3 += 1
029800091119X007 C                   Eval      NREL3 = 00
029900091119X007 C                   Eval      NITEM3 = 00
030000070322     C                   GOTO      CHK
030100091119     C                   Endif
030200070322     C*
030300091119X201 C                   If        e_CInc = '*REL'
030400110104     C*
030500070322     C     REL3TAG       TAG
030600091119X201 C                   If        NREL3 = 99
030700091119X751 C                   Eval      e_CRtn = '2'
030800070322     C                   GOTO      VER3TAG
030900091119     C                   Endif
031000091119X006 C                   Eval      NREL3 += 1
031100091119X007 C                   Eval      NITEM3 = 00
031200070322     C                   GOTO      CHK
031300091119     C                   Endif
031400070322     C*
031500091119X201 C                   If        e_CInc = '*ITEM'
031600091119X201 C                   If        NITEM3 = 99
031700091119X751 C                   Eval      e_CRtn = '2'
031800070322     C                   GOTO      REL3TAG
031900091119     C                   Endif
032000091119X006 C                   Eval      NITEM3 += 1
032100070322     C                   GOTO      CHK
032200091119     C                   Endif
032300070322     C*
032400080123     C**********************************************************
032500080123     C* FRA: CAS NON STANDARD : VERSION EN '8.99.A88'          *
032600080123     C* ENG: NON STANDARD CASE: '8.99.A88' VERSION NUMBER TYPE *
032700080123     C**********************************************************
032800070322     C                   Other
032900091119     C                   Eval      NMethod = 2
033000110104     C                   Eval      AVer2 = CVer2
033100110104     C                   Eval      ARel2 = CRel2
033200110104     C                   Eval      AItem2p = CItem2p
033300080123     C* FRA: Vérif. N° de version cohérent (8.99.A88)
033400080123     C* ENG: Verify the version number sonsistency (8.99.A88)
033500091119     C                   If        CVER2 >= '0' and CVER2 <= '9'
033600091119     C                             and CREL2 >= '00' and CREL2 <= '99'
033700091119     C                             and CITEM2 >= 'A' and CITEM2 <= 'Z'
033800091119     C                             and CITEM2p >= '00' and CITEM2p <= '99'
033900060711     C                   Else
034000091119X751 C                   Eval      e_CVer = CLASTVER
034100091119X751 C                   Eval      e_CRtn = '4'
034200060711     C                   GOTO      FIN
034300091119     C                   EndIf
034400990511     C*
034500990511     C     LOOP2         TAG
034600091119X201 C                   If        e_CInc = '*VER'
034700110104     C*
034800990511     C     VERTA2        TAG
034900091119X201 C                   If        NVer2 = 9
035000091119X751 C                   Eval      e_CRtn = '1'
035100990511     C                   GOTO      FIN
035200091119     C                   Endif
035300990511     C*
035400091119X006 C                   Eval      NVer2 += 1
035500091119X007 C                   Eval      NREL2 = 00
035600091119X751 C                   Eval      CITEM2 = 'A'
035700091119X007 C                   Eval      NITEM2p = 00
035800990511     C                   GOTO      CHK
035900091119     C                   Endif
036000990511     C*
036100091119X201 C                   If        e_CInc = '*REL'
036200110104     C*
036300990511     C     RELTA2        TAG
036400091119X201 C                   If        NREL2 = 99
036500091119X751 C                   Eval      e_CRtn = '2'
036600990511     C                   GOTO      VERTA2
036700091119     C                   Endif
036800091119X006 C                   Eval      NREL2 += 1
036900091119X751 C                   Eval      CITEM2 = 'A'
037000091119X007 C                   Eval      NITEM2p = 00
037100990511     C                   GOTO      CHK
037200091119     C                   Endif
037300990511     C*
037400091119X201 C                   If        e_CInc = '*ITEM'
037500091119X201 C                   If        NITEM2p = 99
037600091119X201 C                   If        CITEM2 = 'Z'
037700091119X751 C                   Eval      e_CRtn = '2'
037800990511     C                   GOTO      RELTA2
037900091119     C                   Endif
038000091119X007 C                   Eval      NIdxI = 1
038100091119X511 C                   Eval      NIdxI = %Lookup(CITEM2 : a_CALPH1 : NIdxI)
038200091119X511 C                   Eval      *IN88 = (NIdxI > 0)
038300091119X201 C                   If        *IN88 = *ON
038400091119X751 C                   Eval      CITEM2 = a_CALPH2(NIdxI)
038500091119X007 C                   Eval      NITEM2p = 00
038600990511     C                   GOTO      CHK
038700091119     C                   Endif
038800990511     C                   ELSE
038900091119X006 C                   Eval      NITEM2p += 1
039000990511     C                   GOTO      CHK
039100091119     C                   Endif
039200091119     C                   Endif
039300070322     C*
039400091119     C                   EndSl
039500940929     C*
039600080123     C* FRA: TEST SI N'EXISTE PAS DEJA
039700080123     C* ENG: TEST IF ALREADY EXISTS
039800940929     C*
039900940929     C     CHK           TAG
040000110104     C                   Select
040100110104     C                   When      NMethod = 1
040200110104     C                   Eval      CVerPlu = AVerPlu
040300110104     C                   Eval      CVer = AVer
040400110104     C                   Eval      CRel = ARel
040500110104     C                   When      NMethod = 2
040600110104     C                   Eval      CVer2 = AVer2
040700110104     C                   Eval      CRel2 = ARel2
040800110104     C                   Eval      CItem2p = AItem2p
040900110104     C                   When      NMethod = 3
041000110104     C                   Eval      CVer31 = AVer31
041100110104     C                   Eval      CVer32 = AVer32
041200110104     C                   Eval      CRel3 = ARel3
041300110104     C                   Eval      CItem3 = AItem3
041400110104     C                   EndSl
041500091119     C     KVER          CHAIN     AARMVERL7                          50
041600091119     C   50KVER          CHAIN     AARMMSOLC                          50
041700091119     C   50KVER          CHAIN     AARMMOBL3                          50
041800091119X201 C                   If        *IN50 = *OFF
041900070322     C                   Select
042000091119     C                   When      NMethod = 1
042100940119     C                   GOTO      LOOP
042200091119     C                   When      NMethod = 2
042300070322     C                   GOTO      LOOP2
042400091119     C                   When      NMethod = 3
042500070322     C                   GOTO      LOOP3
042600091119     C                   EndSl
042700091119     C                   Endif
042800080123     C* Nb OK
042900110103X751 C                   Eval      e_CVer = CVer01
043000891114     C*
043100891114     C     FIN           TAG
043200110104X952 C                   Eval      *InLr = *On
043300960226**
043400891114AB
043500891114BC
043600891114CD
043700891114DE
043800891114EF
043900891114FG
044000891114GH
044100891114HI
044200891114IJ
044300891114JK
044400891114KL
044500891114LM
044600891114MN
044700891114NO
044800891114OP
044900891114PQ
045000891114QR
045100891114RS
045200891114ST
045300891114TU
045400891114UV
045500891114VW
045600891114WX
045700891114XY
045800891114YZ
045900911218aB
046000911218bC
046100911218cD
046200911218dE
046300911218eF
046400911218fG
046500911218gH
046600911218hI
046700911218iJ
046800911218jK
046900911218kL
047000911218lM
047100911218mN
047200911218nO
047300911218oP
047400911218pQ
047500911218qR
047600911218rS
047700911218sT
047800911218tU
047900911218uV
048000911218vW
048100911218wX
048200911218xY
048300911218yZ
04840096022601
04850096022612
04860096022623
04870096022634
04880096022645
04890096022656
04900096022667
04910096022678
04920096022689
0493009602269A
