#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
DataFrame for_loop_idm(double resolution,
                       int n,
                       int time_length,
                       double s_0,
                       double Tg,
                       double a,
                       double b,
                       double v_0,
                       double small_delta,
                       double ln1,

                       NumericVector Time,
                       NumericVector vn,
                       NumericVector vn1,
                       NumericVector sn_star,
                       NumericVector sn,
                       NumericVector xn,
                       NumericVector xn1,
                       NumericVector deltav,
                       NumericVector v_dot
) {


  for(int t = 0; t < (time_length-1); t++) {

    // # desired spacing


    if (((vn[t] * Tg) + ((vn[t] * deltav[t]) / (2 * sqrt((a * b))))) < 0) {

      sn_star[t] = s_0;

    } else {

      sn_star[t] = s_0 + ((vn[t] * Tg) + ((vn[t] * deltav[t]) / (2 * sqrt((a * b)))));

    }

    // # acceleration rate
    if (NumericVector::is_na(sn_star[t])) {

      v_dot[t] = a * (1 - (pow((vn[t] / v_0), small_delta)));

    } else {

      v_dot[t] = a * (1 - (pow((vn[t] / v_0), small_delta)) - (pow((sn_star[t] / sn[t]), 2)));

    }



    if (v_dot[t] < -b){

      v_dot[t] = -b;

    } else {

      v_dot[t] = v_dot[t];

    }

    // # speed
    vn[t+1] = vn[t] + (v_dot[t] * resolution);

    // ### if the speed is negative, make it zero

    if (vn[t+1] < 0){

      vn[t+1] = 0;

    } else {

      vn[t+1] = vn[t+1];

    }


    // # position
    xn[t+1] = xn[t] + (vn[t] * resolution) + (0.5 * v_dot[t] * pow(resolution,2));

    // # spacing
    sn[t+1] = abs(xn1[t+1] - xn[t+1]) - ln1;

    // # speed difference
    deltav[t+1] = vn[t+1] - vn1[t+1];


  }

  DataFrame df = DataFrame::create(Named("fvn") = n,
                                   Named("Time") = Time,
                                   Named("xn1") = xn1,
                                   Named("vn1") = vn1,
                                   Named("ln1") = ln1,
                                   Named("sn_star") = sn_star,
                                   Named("v_dot") = v_dot,
                                   Named("xn") = xn,
                                   Named("vn") = vn,
                                   Named("sn") = sn,
                                   Named("deltav") = deltav);

  return df;

}
