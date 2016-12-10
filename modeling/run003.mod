$PROB one cmpt simple mu modeled bayes
$SUB ADVAN1 TRANS2
$INPUT ID TIME DV CMT EVID AMT RATE
$DATA mdata/simple_nocovar_50id_6tp.csv IGNORE=@

$PRIOR NWPRI NTHETA=2 NETA=2, NTHP=2, NETP=2
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

; THETA PRIORS
$THETAP (0.7 FIX) (3.4 FIX) 

; THETA (uniformative) PRIORs
$THETAPV BLOCK(2)
10000 FIX
0.0 10000 

$OMEGAP BLOCK(2)
0.2 FIX
0 0.2

; degrees of freedom to prior omega matrix - low dof = highly uninformative
$OMEGAPD (2 FIX)

$EST METHOD=CHAIN FILE=run003chains.chn NSAMPLE=5 ISAMPLE=0 DF=20
