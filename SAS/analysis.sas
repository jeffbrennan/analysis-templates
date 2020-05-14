/* ---------------- Setup: Data Handling ---------------- */

/* Manual import */
/* datalines is interchangeable with cards */
/* $ needed to indicate categorical var */
input Catvar $ Numvar Catvar2 $ Numvar2;
datalines;
Catvar Numvar Catvar2 Numvar2
;
 
/* Input from delimited .txt/.csv */
/* Use import wizard to avoid needing to manually name columns */
data df;
	infile '\\path.txt' firstobs=1 dlm=' ';
	input var1 var2;

/* Set library (wd) */
/* Save dataframes to this library and access via explorer */
libname lib '\\path';

/* Capturing results & closing output */
ods rtf file='\\path\results.rtf';
ods rt close;


/* Drop missing observations */
data df;
set df;
if var1=. or var2=. or var3=. then delete;
keep var1 var2 var3
run;


/* Split data into training and testing */


/* split data for validation */

/**split data**/
proc surveyselect data=gp.dataclean
out=gp.datatrain
seed=2102
method=srs
samprate=.5
rep=1;
run;

data gp.trainid;
set gp.datatrain;
train=1;
keep seqn train;
run;

data gp.dataval;
merge gp.trainid gp.dataclean;
by seqn;
if train=1 then delete;
drop train;
run;



/* ---------------- Tables ---------------- */

/* Conditionally output vars */
proc print data=df;
	var var1 var2;
	where var1 > 40; 
 	where var2 = 'condition';
run;


/* Frequency tables */
/* Given that var1 and var2 are categorical */
proc freq data=df;
	tables var1;
	tables var1*var2 / chisq;
run;


/* Statistical testing */

/* t testing */

/* manual critical value given: probability & degrees of freedom */
data tvalue;
    input probability df;
    critval=abs(tinv(probability,df));
    cards;
    0.025 23
    ;
run;
proc print data = tvalue;
run;


/* mean interval */
proc reg data = df;
    model var1 = var2 / clm alpha = 0.10;
run;     


/* individual prediction interval */
proc reg data = df;
    model  out = pred /cli alpha = 0.10;
run;


/* correlation - normal */
PROC CORR DATA = df pearson;
    VAR var1 var2;
run;

/* corrleation - pearsons */
PROC CORR DATA = df spearman;
    VAR var1 var2;
run;

/* f testing */

/* manual assess critical value */
data FValue;
    input prob ndf ddf;
    critval = (Finv(prob,ndf,ddf));
    cards;
    prob1 ndfd1 ddf1
    ;
run;

/* --------- Regression Diagnostics --------- */

/* make model, create resid */
PROC REG DATA = df;
    MODEL var1 = var2 var3 var4 / clb;
    plot out*pred p.*pred / overlay;
    output out=new P=Pred R=Resid rstudent = studdelresid student = studres;
run;
quit;

/* create semistudentized resid using MSE from model */
data new;
set new;
semistud=resid/48.8233;
run;

/* normality of semistudentized resid */
proc univariate plot data = new normal;
    var resid semistud;
    histogram;
    qqplot /normal(mu=est sigma=est color=red l=2) square;
run;

/* assess linearity */
symbol1 v=circle l=32 c = black;
symbol2 v=star l=32 c = red interpol = join;
title "scatter plot predicted vs. X overlay";
PROC GPLOT DATA=NEW;
    PLOT out*pred Pred*pred/ OVERLAY;
RUN;
quit;

/* nonparametric regression */
title "prediction diagnostic plots for data";
proc reg data = df plots = prediction(X=pred_var, smooth);
    model out_var=pred_var/clb clm;
    output out = regout1 p=pred r=resid
lclm=lowerbnd uclm=upperbnd;
run;


/* shape of semistudentized resid */
title "plot of semistudentized predicted values
versus residual";
proc gplot data = new;
    plot semistud*pred / vref=0 lvref=2;
run;


/* Tests */

/* Normality - shapiro */

/* Heteroschedasticity - bp */
proc model data =plutonium3;
	alpha=b0+b1*plutact; /*this is the linear model for proc model to match our model of interest*/
	fit alpha PARMS=(b0 b1)/ breusch=(plutact);  /*the PARMS statement lets sas know what parameters we are interested in estimating by least squares.*/
run;

/* TRANSFORMATIONS */

/* Box-Cox w/ untransformed x */
proc transreg data = df ss2 details;
	model boxcox(var1)=identity(var2);
run;

/* Box-Cox w/ transformed x */
/* square root: sqrt() */
proc transreg data = df ss2 details;
	model boxcox(var1)=log(var2);
run;



/* ------------- Multiple Regression -------------- */

/* Manually obtain interval estimate for one parameter (b) */
/* Parent df is matrix */
/* x_col is integer representing position of desired predictor in 
   df
 */
start sumofsquares;
    SSTO=t(y)*y-(1/n)*t(y)*j(n,n,1)*y;
    SSE=t(y)*y-t(b)*t(x)*y;
    MSE=SSE/(n-p);
    varb=MSE*inv(t(x)*x);
finish sumofsquares;
start inference;
    tstat=abs(b[x_col]/sqrt(varb[x_col, x_col])); 
    tpvalue=2*(1-probt(tstat,n-p));
    bintervalub=b[3]+tinv(0.975,n-p)*sqrt(varb[3,3]);
    bintervallb=b[3]-tinv(0.975,n-p)*sqrt(varb[3,3]);
    finish inference;


/* Multivar diagnostics */

start outtodata;
    studres=(1/sqrt(mse))*e; /*compute the semistudentized residuals*/
    sqresid=e##2; /*# is the element wise power operator, this line squares each element in e*/
    matout=y||x[,2:p]||yhat||e||studres||sqresid;
    create fattrimidreg from matout [colname =
    {"Response" "Pred1" "Pred2" "preds" "resids" "studres"
    "sqresid"}];
    append from matout;
    finish outtodata;

/* variance */
proc sgscatter data = df;
plot sqresids*var1 sqresids*var2 sqresids*var3 / loess=(smooth=0.05);
run;

/* Scatter plots of residuals with loess smooth */
proc sgscatter data = df;
plot resids*var1 resids*var2 resids*var3 / loess=(smooth=0.05);
run;


/* Model selection */

/* Mallow's CP */

Proc reg data=df_train outest=modelsel;
	Model yvar=var1 var2 var3 / selection = cp;
	Output out=df_mallows r=resids p=preds;

Proc sort data=df_mallows;
	By _CP_;
Run;

Title “Data sorted by Cp low to high with variables”;
Proc print data=df_mallowcs label (OBS=4085)
	Var _IN_ _P_ _ CP _ yvar var1 var2 var3;
Run;


/* Model validation */

/* MSPR */

data df_all;
    set df_train df_test;
run;

title "preparing for computing MSPR";
proc reg data = df_all;
    model yvar = var1 var2 var3;
    output out = df_all_out p=predicted r= resids;
run;

data df_test_1;
    set df_all_out;
Run; 


data df_test_1;
    set df_test_1;
    preddiffsq=(bp_avg-predicted)**2;
run;


title "model 1 MSPR";
proc means data =df_test_1;
    var preddiffsq;
run;

/* ------------ Matrices -------- */

/* Simple ops */
proc iml;
start matrixops; /*start a module in iml called Mytranspose*/
	A={2 5, 10 3, 9 4}; /*define matrix A, comma separates rows, spaces for columns*/
	B ={1 3, 4 6, 9 0};
    AT=t(A);
    A_INV  = inv(A);
    A_DET = det(A);
    A_RANK = round(trace(ginv(A)*A));

    ID_4 = I(4)
    C = A+B;
    D = A-B;
    E = A * 4 /* scalar times matrix*/
    F = A * B /* matrix times matrix*/

	print A, AT, C, D;/*print the matrices A, AT, C, and CT. NOTE for printing: Comma prints out the matrices separately, no comma puts them in one table.*/
finish matrixops;
run matrixops;
quit;

/* Regression w/ matrices */

/* read data into matrix */
proc iml;
    start datasetup;
        use df;
        read all var {'var1'} into y 
        read all var {'var2'} into xmat
        n = nrow(xmat)
        x=j(n,1,1)||xmat; /*add a column of 1's to the left side of x the command j(a,b,c) fills a matrix of a rows, b columns with the value c)*/
        p=ncol(x); /* set p = number of columns of design matrix, or total number of beta coefficients*/
    finish datasetup;
        
    start estimateb;
        xpx= t(x)*x; /*compute X'X*/
        xpy= t(x)*y; /*compute X'Y*/
        xpinv=inv(xpx); /*compute the inverse of X'x*/
        b=inv(xpx)*xpy; /*compute b vector*/
    finish estimateb;

    start predicted;
        yhat=x*b;
        e=y-yhat;
        hat=x*inv(xpx)*t(x);
    finish predicted;

    start sumofsquares; /*this module computes each of the sum of squares using the non-quadratic forms of the matrix equations (SSR is left for you to do on your own)*/
		SSTO=t(y)*y-(1/n)*t(y)*j(n,n,1)*y; /*computes total sum of squares eqn 5.83*/
		SSE=t(y)*y-t(b)*t(x)*y; /*computes Error sum of squares eqn 5.84a */
		MSE=SSE/(n-p);/*compute mean squared error recall p is the number of columns of the design matrix (including X0) */
		varb=MSE*inv(t(x)*x); /*compute the variance-covariance matrix of of b*/
		xh={1,65}; /* vector of the new value for prediciton of the mean*/
		meanpredvar=t(xh)*varb*xh; /*compute variance for predicting mean response at Xh*/
	finish sumofsquares;

	start inference;
		tstat =j(p,1,-1);/*start a matrix tstat and tpvalue to hold below computations*/
		tpvalue=j(p,1,-1);
		binterval=j(p,2,0);
		do i = 1 to p;
			tstat[i]=(b[i]/sqrt(varb[i,i]));/*compute the t-statistic*/
		                           /*b[3] is the coefficient for midarm circumference varb[3,3] is the variance term for midarm circumference*/
     		tpvalue[i]=2*(1-probt(abs(tstat[i]),n-p)); /*compute the pvalue for the tstatistic, with n-p df. we multiply by 2 for two tail test.  due to symmmetry we take the absolute value for simplicity of finding the pvalue*/
			binterval[i,1]=b[i]-tinv(0.975,n-p)*sqrt(varb[i,i]); /*lower bound of interval for the coefficients, put on left column of binterval*/
			binterval[i,2]=b[i]+tinv(0.975,n-p)*sqrt(varb[i,i]); /*upper bound of interval for the coeffficients, put in right column of binterval*/
		end;
	finish inference;


	start computeF;
		SSR=t(b)*t(x)*y-(1/n)*t(y)*j(n,n,1)*y; /*compute SSR*/
		MSR=SSR/(p-1);/*compute the regression mean squares. For SLR p=2, but holds for multivariate regression */
		Fstat=MSR/MSE; /*compute the ANOVA F statsitic for the model*/
		Fpvalue = 1-probF(Fstat,p-1,n-p); /*compue the pvalue for the anova F statistic*/
	finish computeF;

    start partialcorr;
		Rsq=SSR/SSTO;
		adjRsq=1-((n-1)/(n-p))*(SSE/SSTO);
	finish partialcorr;


	start outtodata;
		studres=(1/sqrt(mse))*e; /*compute the semi-studentized residuals*/
		sqresid=e##2; /*## is the element wise power operator, this line squares each element in e*/	
						/*not the same as t(e)*e or e*t(e). one is a scalar one is a matrix*/
		matout=y||x[,2:p]||yhat||e||studres||sqresid;
		create Dwainereg from matout [colname = {"var1" "var2" "var3" "preds" "resids" "studres" "sqresid"}]; /*create a dataset in sas with the varnames in {}*/ 
			append from matout;
	finish outtodata;
	

    run datasetup;
    run estimateb;
    run predicted;
    run sumofsquares;
    run outtodata;
    print b varb MSE rootMSE SSE;
    run computef;
    run inference;
    run partialcorr;
    print fstat fpvalue, b varb tstat tpvalue binterval, Rsq adjRsq, MSE rootMSE; 
run;
quit;

	print y X b yhat e,bintervallb bintervalub, SSTO, SSE MSE; /*using spaces prints in one table, commas give it a separate table*/
	run;/*this run statement executes the print above*/
quit; 

/* SURVIVAL ANALYSIS */

/* KAPLAN MEIER */

/* 
OPTIONS
- outs=out_surv: output statistics to var (can print using proc print)
- conftype=loglog; linear; ASINSQRT: transformation for CI 
- plots=(s, ls, lls): plot CI (surv, log surv, loglog surv) [base]
- survival(cl cb=hw): plot conf line and confband
- timelist=0.5 5 10: report specific S(t)
- timelim=27: set restricted mean tau
*/

ods graphics on;
proc lifetest data=survdat plots=survival(cl);
time time*censor(0);
run;
ods graphics off;


/* NELSON AALEN */

/* option a */
proc lifetest data=survdat plots=(s) nelson;
time time*censor(0);
run;

/* option b */
proc phreg data=survdat plots=(s);
model time*censor(0)=;
baseline out = survout cumhaz=cumhaz survival=sur_nelson;
run;

proc print data=survout noobs;
run;


/* LIFE TABLES */

/* can also use intervals=0,2,4,6,8; if intervals aren't even */
proc lifetest data=example method=lt intervals=(0 to 8 by 2) plots=(s);
time Years*Fail(0);
freq Freq;
run;


/* LOG-RANK TEST */
/* given trt 1 = 'a' 2='b' */
ods graphics on;
proc lifetest data=example_data plots=survival(cl);
time time*death(0);
strata trtgrp;
format trtgrp trt.;
run;
ods graphics off;

/* STRATIFIED TEST */
proc lifetest data=Exposed;
      time Days*Status(0);
      strata Sex / group=Treatment;
run;

/* TREND TEST */
data bmt; set sashelp.bmt;
score=.;
if group='ALL' then score=2;
if group='AML-Low Risk' then score=1;
if group='AML-High Risk' then score=3;
run;

ods graphics on;
proc lifetest data=bmt plots=(s);
time t*status(0);
strata score/trend;
run;
ods graphics off;

/* PH REGRESSION */

*fit a model;
*specify the option for handling ties;
proc phreg data=actg320;
model time*censor(0)=tx age sex cd4 priorzdv/ties=efron risklimits;
run;

*hypothesis testing: wald test;
proc phreg data=actg320;
model time*censor(0)=tx age sex cd4 priorzdv/ risklimits;
Test1: test age=0,sex=0;
Test2: test age,sex;
run;

*all three tests--LR, Wald and score;
proc phreg data=actg320;
model time*censor(0)=tx age sex cd4 priorzdv/ risklimits;
contrast 'Testing age and sex' age 1, sex 1/test(all);
run;

/* COX */
ods graphics on;
proc phreg data=larynx_recode plots(overlay)=(survival);
model time*death(0)=z1 z2 z3 age;
baseline covariates=spec out=out_surv survival=_all_ cumhaz=_all_/
         cltype=loglog rowid=label;
run;
ods graphics off;

/* ASSUMPTION CHECKS */
**************************
*checking PH assumption, using standardized score residuals
*Note that you also get p-values!
****************************;
ods graphics on;
proc phreg data=bmt;
class group(param=ref ref="1");
model t2*d3(0)=group page dage page*dage fab wtime mtx;
assess ph/resample=1000 npaths=0 seed=1;
run;
ods graphics off;

*also try this;
*larger resample size would provide a more accurate p-value;
*npath will add simulated realizations of the score process;
ods graphics on;
proc phreg data=bmt;
class group(param=ref ref="1");
model t2*d3(0)=group page dage page*dage fab wtime mtx;
assess ph/resample=5000 seed=1 npaths=20;
run;
ods graphics off;


*******************
*Checking PH assumption for mtx, by testing an interaction with log-time;
***************;
proc phreg data=bmt;
class group(param=ref ref="1");
model t2*d3(0)=group page dage page*dage fab wtime mtx mtx_time;
mtx_time=mtx*log(t2);
run;


***************************
*checking PH assumption, using the strata statement and baseline cumulative hazards;
**************************;
*first create a dataset to define "baseline";
data spec;
input group page dage fab wtime;
datalines;
1 0 0 0 0
;
run;

proc phreg data=bmt;
class group(param=ref ref="1");
model t2*d3(0)=group page dage page*dage fab wtime;
strata mtx;
baseline covariates=spec cumhaz=_all_ out=out_dat;
run;

*Open the "out_dat" dataset, you will see that H0(t) are estimated for the two mtx groups; 
*to make figures, we need to transform the out_dat dataset;
*transpose the out_dat from long to wide;
proc sort data=out_dat; by t2; run;

proc transpose data=out_dat out=plotdat0 prefix=cumhaz;
by t2; id mtx; var cumhaz;
run;

data plotdat; set plotdat0; 
retain cumhaz0_1 cumhaz1_1;
if cumhaz0>=0 then cumhaz0_1=cumhaz0;*logH in the first strata;
if cumhaz1>=0 then cumhaz1_1=cumhaz1;*logH in the second strata;
diff_log=log(cumhaz1_1)-log(cumhaz0_1);*difference in log H;
run;

proc sgplot data=plotdat;
step x=t2 y=diff_log;
run;

*Anderson plot;
proc sgplot data=plotdat;
step x=cumhaz0_1 y=cumhaz1_1;
xaxis values=(0 to 1 by 0.1);
run;


/* MULTIVARIATE ANALYSIS */



/* PREDICTIONS */




