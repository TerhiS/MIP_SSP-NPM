* ==================
* The job sequencing and tool switching problem with time-tabling
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
*$include C:\Users\Dorothea\Dropbox\Math_Model\output1.txt
*$include C:\Users\Administrator\Dropbox\EURO2\EURO\SSP-NPM-I\output1.txt
$include C:\Users\Administrator\Dropbox\Math_Model\output1.txt
$ontext
Sets
m        machines /1,2/
j        jobs /1*6/
t        tools /1*9/
k        time /1*20/
;
Sets
         sig(j,t)        set of tools required for job j
                 / 1.(1,4,8,9)
                   2.(1,3,5)
                   3.(2,6,7,8)
                   4.(1,5,7,9)
                   5.(3,5,8)
                   6.(1,2,4) /

         tau(t,j)        set of jobs requiring tool t
                 / 1.(1,2,4,6)
                   2.(3,6)
                   3.(2,5)
                   4.(1,6)
                   5.(2,4,5)
                   6.3
                   7.(3,4)
                   8.(1,3,5)
                   9.(1,4) /;

Parameter
c(m)        magazine capacity /
         1=4
         2=3
/

p(j,m)   processing time of job j on machine m
/
1.1 =1
1.2 =2
2.1 =3
2.2 =4
3.1 =4
3.2 =3
4.1 =5
4.2 =5
5.1 =6
5.2 =3
6.1 =1
6.2 =1
/

sw(m)       tool switching time on machine m
/
         1 = 1
         2 = 2
/
$offtext
;
Parameter
g
h
;

g = sum(j,smax(m,p(j,m)))+(card(j)-card(m))*smax(m,(c(m)*sw(m)));
h = smax(m,(c(m)*sw(m)));

* ==================
* Definition of variables
* ==================
Variables
         y(j,m,k)        if job j uses machine m in period k (processing or switching)
         z(j,m,k)       if processing starts without setup or if switching has been initiated in a previous period
         v(t,m,k)        if tool t is present in the magazine in period k
         w(t,m,k)        if a tool t is inserted in period k that was not in the magazine in period k-1
         x(j,m,k)        if processing or tool switching starts
         d1(m,k)         if tool switch taking place
         d(j)           if tool switch(es) are preceding job j's processing
         st(j)           switching time between preceding job and job j
         s(j)            starting period of processing
         f(j)            completion time of job j
         z1              objective value makespan
         z2              objective value total flowtime
         z3              objective value total number of tool switches
;

*Binary Variables

Binary Variables
* x,
v,y,x,d1,d,w,z;

*Positive Variables

* st,s,f;

* ==================
* Constraints and objective function
* ==================

*** declaration of equations
Equations
$ontext
b1(j,m,k)
b2(m,k)
b3(j)
b4(j,m,k)
b5(j,m,k)
b6(t,m,k)
b7(t,m,k)
$offtext
e1(j)
e2(m,k)
e3(j,k)
e4(m,k)
e5(j,k)
e6(m,k)
e7(t,m,k)
e8(m,k)
e9(t,m,k)
e10(m,k)
e11(j,k)
e12(j,m)
e13(j,m)
e14(j,m,k)
e15(j,m,k)
e16(j)
e17(j)
e18(j)
e19(j)
e20(j,k)
e21(j,k)
e22(j)
e23(j)
e24(m,k)
e25(m,k)
e26(m,k)

*e31(m)
*e32(m)
e33(m)
e34(m)

makespan(j)
*flowtime
*switches
ub(m,k)
u2
;
$ontext
b1(j,m,k)..
x(j,m,k) =l= 1;
b2(m,k)..
d1(m,k) =l= 1;
b3(j)..
d(j) =l= 1;
b4(j,m,k)..
y(j,m,k) =l= 1;
b5(j,m,k)..
z(j,m,k) =l= 1;
b6(t,m,k)..
v(t,m,k) =l= 1;
b7(t,m,k)..
w(t,m,k) =l= 1;
$offtext

e1(j)..
sum((m,k),x(j,m,k)) =e= 1;
e2(m,k)..
sum(j,x(j,m,k)) =l= 1;
e3(j,k)..
sum(m,x(j,m,k)) =l= sum(m,y(j,m,k));
e4(m,k)..
sum(j,y(j,m,k)) =l= 1;
e5(j,k)..
sum(m,y(j,m,k)) =l= 1;
e6(m,k)$(ord(k)>1)..
sum(j,y(j,m,k)) =l= sum(j,y(j,m,k-1));
e7(t,m,k)..
sum(j$tau(t,j),y(j,m,k)) =l= v(t,m,k);
e8(m,k)..
sum(t,v(t,m,k)) =l= c(m);
e9(t,m,k)$(ord(k)>1)..
v(t,m,k)-v(t,m,k-1) =l= w(t,m,k);
e10(m,k)..
sum(j,z(j,m,k)) =l= 1;
e11(j,k)..
sum(m,z(j,m,k)) =l= sum(m,y(j,m,k));
e12(j,m)..
sum(k,y(j,m,k)) =l= (card(k))*sum(k,x(j,m,k));
e13(j,m)..
sum(k,z(j,m,k)) =l= (card(k))*sum(k,x(j,m,k));
e14(j,m,k)..
st(j) =g= sw(m)*sum(t,w(t,m,k))-h*(1-x(j,m,k));
e15(j,m,k)..
st(j) =l= sw(m)*sum(t,w(t,m,k))+h*(1-x(j,m,k));
e16(j)..
sum((k,m),y(j,m,k)) =e= st(j)+sum((m,k),(p(j,m)*x(j,m,k)));
*sum((k,m),y(j,m,k)) =e= sum((m,k),(p(j,m)*x(j,m,k)));
e17(j)..
sum((k,m),z(j,m,k)) =e= st(j)+sum((m,k),(p(j,m)*x(j,m,k)))-d(j);
*sum((k,m),z(j,m,k)) =e= st(j)+sum((m,k),(p(j,m)*x(j,m,k)))-d(j);
e18(j)..
*f(j)-s(j) =e= st(j)+sum((m,k),(p(j,m)*x(j,m,k)))-d(j);
*f(j)-s(j) =e= sum((m,k),(p(j,m)*x(j,m,k)))-d(j);
f(j)-s(j) + 1 =e= st(j)+sum((m,k),(p(j,m)*x(j,m,k)));
e19(j)..
s(j) =e= sum((m,k),(ord(k))*x(j,m,k));
*s(j) =e= sum((m,k),(ord(k))*x(j,m,k))+d(j);
e20(j,k)..
f(j) =g= ord(k)*sum(m,y(j,m,k));

e21(j,k)..
*s(j) =l= (ord(k)-1)*sum(m,z(j,m,k))+card(k)*(1-sum(m,z(j,m,k)));
s(j) =l= (ord(k))*sum(m,z(j,m,k))+card(k)*(1-sum(m,z(j,m,k)));

e22(j)..
d(j) =l= st(j);
e23(j)..
st(j) =l= h*d(j);
e24(m,k)..
d1(m,k) + sum(j,z(j,m,k)) =l= 1;
*opt
e25(m,k)..
d1(m,k) =l= sum(t,w(t,m,k));

e26(m,k)..
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
e33(m)..
         sum(t,w(t,m,'1')) =e= 0;
* No tool switching starts in period 1
e34(m)..
         d1(m,'1') =e= 0;

makespan(j)..
z1 =g= f(j);
ub(m,k)..
sum(t,w(t,m,k)) =l= c(m)*sum(j,x(j,m,k));

u2..
z1 =g= 30;
*flowtime..
*z2 =g= sum(j,f(j));

*switches..
*z3 =g= sum((t,m,k),w(t,m,k));

Model new /all/;

* set time limit to 1800 seconds
new.reslim=3600;


* use all cores
option threads=4;
option optcr=0;
*$onecho > cplex.opt
*varsel 3
*$offecho

Solve new using MIP minimizing z1;
