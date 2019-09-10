/* Visualizations */

/* ---------------- Exploratory plotting ---------------- */


/* Normality assessments */
proc univariate data=df;
    var var1;
    histogram / normal;
    qqplot / normal;
run;


/* ---------------- Linear modeling ---------------- */
proc reg data=df;
    var out pred;
    model out = pred / clb;
  predictor  plot out*pred p.*pred / overlay;
    output out=NEW p=PRED R=Resid;
run;


/* scatter plot w/ confidence bands */
proc reg data = df plots = fitplot(nocli);
    model var1 = var2 / clb;
run;

