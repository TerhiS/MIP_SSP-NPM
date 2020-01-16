* ==================
* A time-index-based mixed integer linear program for the
* job sequencing and tool switching problem with non-identical parallel machines
* ==================

***
* author: D. Calmels
***

* ==================
* Definition of indices, sets and parameters of a sample instance
* ==================

*** Include data set that contains
* machine set @index(m)
* job set @index(j)
* tool set @index(t)
* time set @index(k)
* tools required for job j @set sig(j,t)
* jobs that require tool t @set tau(t,j)
* tool magazine capacity @parameter c(m)
* procssing time of job j on machine m @parameter p(j,m)
* tool switching time per machine @parameter sw(m)
***

***
* Include for example
* sample instance: output1.txt
* replace file path
* $include "file path:\...\"output1.txt
* Note: ensure that the time horizon K must be large enough in order to allow for feasible solutions
$include C:\Users\Administrator\Dropbox\Math_Model\Gams_Input\output273.txt
;

*** Additional sets and parameters for precedence-based formulation
Parameter
*auxiliary parameter
h large constant (maximum possible setup time)
;
h = smax(m,(c(m)*sw(m)));

* ==================
* Definition of variables
* ==================

Variables
v(t,m,k)         if tool t is present in the magazine in period k
w(t,m,k)         if a tool t is inserted in period k that was not in the magazine in period k-1
x(j,m,k)         if if processing or tool switching of job j starts in period k on machine m
y(j,m,k)         if job j's processing or setup is initiated or ongoing on machine m in period k
z(j,m,k)         if processing starts without setup or if switching has been initiated in a previous period
d1(m,k)          if tool switch is initiated in  period k on machine m
d(j)             if tool switch(es) are preceding job j's processing

* continuous variable
st(j)            switching time between preceding job and job j
s(j)             starting period of processing
f(j)             completion time of job j

* objective function values
fmax             objective value makespan
tft              objective value total flowtime
;

*Binary Variables
Binary Variables

v,w,x,y,z,d,d1;

*Positive Variables
* st,s,f;

* ==================
* Definition of equations
* ==================

*** the numbers are identical to the numbers in the publication
Equations
e40(j)
e41(m,k)
e42(j,k)
e43(m,k)
e44(j,k)
e45(m,k)
e46(t,m,k)
e47(m,k)
e48(t,m,k)
e49(m,k)
e50(j,k)
e51(j,m)
e52(j,m)
e53(j,m,k)
e54(j,m,k)
e55(j)
e56(j)
e57(j)
e58(j,k)
e59(j)
e60(j,k)
e61(j)
e62(m,k)
e63(m,k)
e64(m,k)
e65(m)
e66(m)

* objective functions
e71
e73(j)
;

* ==================
* Equations
* ==================

e40(j)..
sum((m,k),x(j,m,k)) =e= 1;

e41(m,k)..
sum(j,x(j,m,k)) =l= 1;

e42(j,k)..
sum(m,x(j,m,k)) =l= sum(m,y(j,m,k));

e43(m,k)..
sum(j,y(j,m,k)) =l= 1;

e44(j,k)..
sum(m,y(j,m,k)) =l= 1;

e45(m,k)$(ord(k)>1)..
sum(j,y(j,m,k)) =l= sum(j,y(j,m,k-1));

e46(t,m,k)..
sum(j$tau(t,j),y(j,m,k)) =l= v(t,m,k);

e47(m,k)..
sum(t,v(t,m,k)) =l= c(m);

e48(t,m,k)$(ord(k)>1)..
v(t,m,k)-v(t,m,k-1) =l= w(t,m,k);

e49(m,k)..
sum(j,z(j,m,k)) =l= 1;

e50(j,k)..
sum(m,z(j,m,k)) =l= sum(m,y(j,m,k));

e51(j,m)..
sum(k,y(j,m,k)) =l= (card(k))*sum(k,x(j,m,k));

e52(j,m)..
sum(k,z(j,m,k)) =l= (card(k))*sum(k,x(j,m,k));

e53(j,m,k)..
st(j) =g= sw(m)*sum(t,w(t,m,k))-h*(1-x(j,m,k));

e54(j,m,k)..
st(j) =l= sw(m)*sum(t,w(t,m,k))+h*(1-x(j,m,k));

e55(j)..
sum((k,m),y(j,m,k)) =e= st(j)+sum((m,k),(p(j,m)*x(j,m,k)));

e56(j)..
sum((k,m),z(j,m,k)) =e= st(j)+sum((m,k),(p(j,m)*x(j,m,k)))-d(j);

e57(j)..
f(j)-s(j) + 1 =e= st(j)+sum((m,k),(p(j,m)*x(j,m,k)));

e58(j,k)..
f(j) =g= ord(k)*sum(m,y(j,m,k));

e59(j)..
s(j) =e= sum((m,k),(ord(k))*x(j,m,k));

e60(j,k)..
s(j) =l= (ord(k))*sum(m,z(j,m,k))+card(k)*(1-sum(m,z(j,m,k)));

e61(j)..
d(j) =l= st(j);

e62(m,k)..
d1(m,k) + sum(j,z(j,m,k)) =l= 1;

e63(m,k)..
d1(m,k) =l= sum(t,w(t,m,k));

e64(m,k)..
sum(t,w(t,m,k)) =l= c(m)*d1(m,k);

* ====== Initial loading ========
* Assumed that all machines will be used
* There must be a first job on each machine
*e31(m)..
*         sum(j,y(j,m,'1')) =e= 1;
* The processing of one job must start in period 1
*e32(m)..
*         sum(j,x(j,m,'1')) =e= 1;
* No tool switches in period 1
e65(m)..
         sum(t,w(t,m,'1')) =e= 0;
* No tool switching starts in period 1
e66(m)..
         d1(m,'1') =e= 0;

* objective functions
e71..
tft =g= sum(j,f(j));
e73(j)..
fmax =g= f(j);

* ==================
* Model
* ==================

* Create model 'tib' that uses all variables and constraints
Model tib /all/;

* ==================
* CPLEX Settings
* ==================
* time limit
tib.reslim=1800;
* use all cores
option threads=12;
* other settings
option optcr=0;
*$onecho > cplex.opt
*lpmethod 4
*$offecho

* ==================
* Solve first objective
* ==================

Solve tib using MIP minimizing tft;

* ==================
* Results
* ==================

Variable
time1     CPU used
gap1      relative gap to best possible bound for objective value
;
time1.l=tib.resusd;
gap1.l=(tib.objVal-tib.objEst)/tib.objVal;

* save the results in an excel-file 'file.xlsx' in the 'file_path'
* in the workbook 'workbook_tft'
execute_unload "file.gdx" tft.l time1.l gap1.l
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=tft.l rng='workbook_tft'!a1'
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=time1.l rng='workbook_tft'!b1'
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=gap1.l rng='workbook_tft'!c1'

* ==================
* Solve second objective
* ==================
Solve tib using MIP minimizing fmax;

* ==================
* Results
* ==================
Variable
time2     CPU used
gap2      relative gap to best possible bound for objective value
;
time2.l=tib.resusd;
gap2.l=(tib.objVal-tib.objEst)/tib.objVal;

* save the results in an excel-file 'file.xlsx' in the 'file_path'
* in the workbook 'workbook_fmax'
execute_unload "file.gdx" fmax.l time2.l gap2.l
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=fmax.l rng='workbook_fmax'!a1'
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=time2.l rng='workbook_fmax'!b1'
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=gap2.l rng='workbook_fmax'!c1'

* ==================
* End
* ==================


