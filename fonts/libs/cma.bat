@DEL SL4_NOR.LIB
@DEL SL4_NOR.QLB
@DEL SL4_IPX.LIB
@DEL SL4_IPX.QLB
@CALL CML SDK_001
@CALL CML SDK_002
@CALL CML DRAC_002
@CALL CML WIN_001
@CALL CML DMOUSE
@CALL CML IPX
@LIB SL4_NOR.LIB /NOE UIASM.OBJ+UTILS.OBJ+SDK_001.OBJ+SDK_002+DRAC_002.OBJ+WIN_001.OBJ+DMOUSE.OBJ;
@LIB SL4_IPX.LIB /NOE UIASM.OBJ+UTILS.OBJ+SDK_001.OBJ+SDK_002+DRAC_002.OBJ+WIN_001.OBJ+DMOUSE.OBJ+IPX.OBJ;
@LINK /QU /LI /NOE /PACKCODE SL4_NOR.LIB+QBX.LIB+DTFMTER.LIB,SL4_NOR.QLB,,QBXQLB.LIB;
@LINK /QU /LI /NOE /PACKCODE SL4_IPX.LIB+QBX.LIB+DTFMTER.LIB,SL4_IPX.QLB,,QBXQLB.LIB;
@ECHO El fitxer SL4_NOR.LIB no te suport IPX.
@ECHO El fitxer SL4_IPX.LIB el te.