* ==================
* A precedence-based mixed integer linear program for the
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
* job set @index(j), alias (i,j,l)
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
* $include "file path:\...\"input1.txt
;

*** Additional sets and parameters for precedence-based formulation
Sets
* dummy set required for set difference
ss(j,t)
* set difference (Tools \ Subset of tools required by job j)
diff(j,t)
* job aliases
alias(i,j)
alias(l,j)
;

* Calculation of the set difference
ss(j,t) = yes;
diff(j,t) = ss(j,t) - sig(j,t);

Parameter
g large constant;
g = sum(j,smax(m,p(j,m)))+(card(j)-1)*smax(m,(c(m)*sw(m)));

* ==================
* Definition of variables
* ==================

Variables
* binary variables
u(j,m)          if job j is assigned to machine m
v(j,t)          if tool t is present in the magazine while job j is processed
w(j,t)          if tool t needs to be inserted before starting to process job j
x(i,j,m)        if job i is immediately followed by job j on machine m
start(j,m)      if job j is the first job in the sequence on machine m (alpha)
end(j,m)        if job j is the last job in the sequence on machine m (omega)

* continuous variable
f(j)            completion time of job j

* objective function values
tft             objective value total flowtime
fmax            objective value makespan
;

Binary Variables
w,u,x,v,u,start,end;

* ==================
* Definition of equations
* ==================

*** the numbers are identical to the numbers in the publication
equations
e1(i)
e2(j)
e3(j,m)
e4(j,m)
e5(j)
e6(i,j,m,t)
e7(m)
e8(m)
e9(j)
e10(j)
e11(j)
e12(j,m)
e13(j,m)
e14(i,j,m)
e15(j,m)
e16(j,m)
e17(i,j,m)
e18(j,m)
e19(j,t)
e20(j,t)

* objective functions
e24
e26(j)
;

* ==================
* Equations
* ==================

e1(i)..
sum((m,j)$(ord(j)<>ord(i)),x(i,j,m)) =l= 1;

e2(j)..
sum((m,i)$(ord(i)<>ord(j)),x(i,j,m)) =l= 1;

e3(j,m)..
sum(l,x(j,l,m))-sum(i,x(i,j,m)) =l= start(j,m);

e4(j,m)..
sum(i,x(i,j,m))-sum(l,x(j,l,m)) =l= end(j,m);

e5(j)..
sum(t,v(j,t)) =l= sum(m, (c(m)*u(j,m)));

e6(i,j,m,t)$(ord(j)<>ord(i))..
x(i,j,m)+v(j,t)-v(i,t) =l= w(j,t)+1;

e7(m)..
sum(j,start(j,m)) =l= 1;

e8(m)..
sum(j,end(j,m)) =l= 1;

e9(j)..
sum(m,start(j,m)) =l= 1;

e10(j)..
sum(m,end(j,m)) =l= 1;

e11(j)..
sum(m,u(j,m)) =e= 1;

e12(j,m)..
start(j,m) =l= u(j,m);

e13(j,m)..
end(j,m) =l= u(j,m);

e14(i,j,m)$(ord(j)<>ord(i))..
2*x(i,j,m) =l= u(i,m) + u(j,m);

e15(j,m)..
sum(i$(ord(i)<>ord(j)),x(i,j,m))+start(j,m) =e= u(j,m);

e16(j,m)..
sum(i$(ord(i)<>ord(j)),x(j,i,m))+end(j,m) =e= u(j,m);

e17(i,j,m)$(ord(j)<>ord(i))..
f(j) =g= f(i) + p(j,m)*u(j,m) + sw(m)*sum(t,w(j,t))- g*(1-x(i,j,m));

e18(j,m)..
f(j) =g= (p(j,m)*u(j,m)) + sw(m)*sum(t,w(j,t));

e19(j,t)$sig(j,t)..
v(j,t) =e= 1;

e20(j,t)$diff(j,t)..
w(j,t) =e= 0;

* objective functions
e24..
tft =e= sum(j,f(j));

e26(j)..
fmax =g= f(j);


* ==================
* Model
* ==================

* Create model 'prb' that uses all variables and constraints
Model prb /all/;

* ==================
* CPLEX Settings
* ==================
* time limit
prb.reslim=1800;
* use all cores
option threads=12;
* other settings
option optcr=0;
$onecho > cplex.opt
lpmethod 4
$offecho

* ==================
* Solve first objective
* ==================
Solve prb using MIP minimizing tft;

* ==================
* Results
* ==================
Variable
time1     CPU used
gap1      relative gap to best possible
;
time1.l=prb.resusd;
gap1.l=(prb.objVal-prb.objEst)/prb.objVal;

* save the results in an excel-file 'file.xlsx' in the 'file_path'
* in the workbook 'workbook_tft'
execute_unload "file.gdx" tft.l time1.l gap1.l
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=tft.l rng='workbook_tft'!a1'
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=time1.l rng='workbook_tft'!b1'
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=gap1.l rng='workbook_tft'!c1'

* ==================
* Solve second objective
* ==================
Solve prb using MIP minimizing fmax;

* ==================
* Results
* ==================
Variable
time2     CPU used
gap2      relative gap to best possible bound for objective value
;
time2.l=prb.resusd;
gap2.l=(prb.objVal-prb.objEst)/prb.objVal;

* save the results in an excel-file 'file.xlsx' in the 'file_path'
* in the workbook 'workbook_fmax'
execute_unload "file.gdx" fmax.l time2.l gap2.l
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=fmax.l rng='workbook_fmax'!a1'
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=time2.l rng='workbook_fmax'!b1'
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=gap2.l rng='workbook_fmax'!c1'

* ==================
* End
* ==================


