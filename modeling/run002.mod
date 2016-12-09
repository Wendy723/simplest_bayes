$PROB one cmpt simple mu modeled
$SUB ADVAN1 TRANS2
$INPUT ID TIME DV CMT EVID AMT RATE
$DATA mdata/simple_nocovar_50id_6tp.csv IGNORE=@
$PK


MU_1 = THETA(1)
MU_2 = THETA(2)
 
nCL = ETA(1)
nV = ETA(2)

CL = EXP(MU_1 + nCL)
V = EXP(MU_2 + nV) 
S1 = V

$ERROR
IPRED=F
Y = IPRED*(1 + ERR(1))  

$THETA
(0.001, 0.7) ; LN_TVCL
(0.001, 3.4) ; LN_TVV

$OMEGA BLOCK(2)
0.1    ; nCL
0.1  0.1  ; nV

$SIGMA
0.03 ; PROP

$EST MAXEVAL=9999 PRINT=5 NOABORT METHOD=1 INTER 
$COV PRINT=E MATRIX=R
$TABLE ID TIME IPRED DV CWRES PRED ONEHEADER NOAPPEND NOPRINT FILE=sdtab002
$TABLE ID CL V MU_1 MU_2 nCL nV ONEHEADER FIRSTONLY NOPRINT NOAPPEND FILE=patab002