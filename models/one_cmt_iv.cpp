[PARAM] @annotated
CL  : 3   : Clearance (L/hr)
V   : 35  : Volume (L) 


[CMT] @annotated
CENT : Central compartment (mg)

[PKMODEL]
ncmt=1, trans=11

[MAIN]
double CLi = CL*exp(nCL);
double Vi = V*exp(nV);

  
[OMEGA] @annotated @correlation @block
nCL : 0.1     : Random effect on CL
nV  : 0.4 0.04 : Random effect on V
    
[SIGMA] @annotated
PROP : 0.04 : Proportional error
// so don't get into issues with estimating via multiplicative error only
ADD  : 0.1 : Additive residual error

[TABLE]
double IPRED = CENT/Vi;
double DV = CENT/Vi*(1+PROP) + ADD;

[CAPTURE] @annotated
DV    : plasma concentration (mg/L)
IPRED : Individual predicted plasma concentration (mg/L)
CLi   : Individual Clearance (L/hr)
Vi    : Individual Volume (L)
