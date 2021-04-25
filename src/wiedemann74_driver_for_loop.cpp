#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
DataFrame for_loop_wiedemann(double D_MAX,
                             int time_length,
                             double BXadd,
                             double AX,
                             double CX,
                             double EX,
                             double OPDVadd,
                             double BMAXmult,
                             double V_MAX,
                             double FaktorV,
                             double BMIN,
                             double BNULL,
                             double resolution,

                             NumericVector vn,
                             NumericVector vn1,
                             NumericVector sn,
                             NumericVector xn,
                             NumericVector xn1,
                             NumericVector deltav,
                             NumericVector bn1) {

  // Empty vectors
  NumericVector BX (time_length, NumericVector::get_na());
  NumericVector ABX (time_length, NumericVector::get_na());
  NumericVector SDV (time_length, NumericVector::get_na());
  NumericVector SDX (time_length, NumericVector::get_na());
  NumericVector CLDV (time_length, NumericVector::get_na());
  NumericVector OPDV (time_length, NumericVector::get_na());

  NumericVector BMAX (time_length, NumericVector::get_na());
  NumericVector bn (time_length, NumericVector::get_na());
  StringVector cf_state_sim (time_length);
  NumericVector B_Emg (time_length, NumericVector::get_na());
  NumericVector B_App (time_length, NumericVector::get_na());


  for(int t = 0; t < (time_length-1); t++) {

    if(vn[t] < vn1[t] || NumericVector::is_na(vn1[t])) {

      BX[t] = BXadd * sqrt(vn[t]);

    } else if (vn[t] > vn1[t]) {

      BX[t] = BXadd * sqrt(vn1[t]);

    }


    ABX[t] = AX + BX[t];

    SDV[t] = pow(((sn[t] - AX)/CX), 2);

    SDX[t] = AX + (EX * BX[t]);

    CLDV[t] = SDV[t] * pow(EX, 2);

    OPDV[t] = CLDV[t] * ((-1) * OPDVadd);





    if (NumericVector::is_na(sn[t]) || NumericVector::is_na(deltav[t])) {

      BMAX[t] = BMAXmult * (V_MAX - (vn[t] * FaktorV));

      bn[t] = BMAX[t];

      cf_state_sim[t] = "free_driving";

    } else if (sn[t] <= ABX[t]) {

      B_Emg[t] = 0.5 * (pow(deltav[t], 2) / (AX - sn[t])) + bn1[t] +
        (BMIN * ((ABX[t] - sn[t]) / (ABX[t] - AX)));

      cf_state_sim[t] = "emergency_braking";

      if (B_Emg[t] < BMIN || B_Emg[t] > 0) {

        bn[t] = BMIN;

      } else {

        bn[t] = B_Emg[t];

      }




    } else if (sn[t] < SDX[t]) {

      if ( deltav[t] > CLDV[t]) {

        B_App[t] = 0.5 * (pow(deltav[t], 2) / (ABX[t] - sn[t])) + bn1[t];

        if (B_App[t] < BMIN) {

          bn[t] = BMIN;

        } else {

          bn[t] = B_App[t];

        }

        cf_state_sim[t] = "approaching";

      } else if (deltav[t] > OPDV[t]) {

        if (deltav[t] < 0) {

          bn[t] = BNULL;

          cf_state_sim[t] = "following";

        } else {

          bn[t] = -BNULL;

          cf_state_sim[t] = "following";

        }

      } else {

        BMAX[t] = BMAXmult * (V_MAX - (vn[t] * FaktorV));

        bn[t] = BMAX[t];

        cf_state_sim[t] = "free_driving";

      }

    } else {

      if (deltav[t] > SDV[t] && sn[t] < D_MAX) {

        B_App[t] = 0.5 * (pow(deltav[t], 2) / (ABX[t] - sn[t])) + bn1[t];

        if (B_App[t] < BMIN) {

          bn[t] = BMIN;

        } else {

          bn[t] = B_App[t];

        }

        cf_state_sim[t] = "approaching";

      } else {

        BMAX[t] = BMAXmult * (V_MAX - (vn[t] * FaktorV));

        bn[t] = BMAX[t];

        cf_state_sim[t] = "free_driving";

      }
    }



    vn[t+1] = vn[t] + (bn[t] * resolution);

    if (vn[t+1] < 0) {

      vn[t+1] = 0;

    } else {

      vn[t+1] = vn[t+1];

    }


    xn[t+1] = xn[t] + (vn[t] * resolution) + (0.5 * bn[t] * pow(resolution, 2));

    sn[t+1] = abs(xn1[t+1] - xn[t+1]);

    deltav[t+1] = vn[t+1] - vn1[t+1];

  }


  DataFrame df = DataFrame::create(Named("xn1") = xn1,
                                   Named("vn1") = vn1,
                                   Named("bn") = bn,
                                   Named("xn") = xn,
                                   Named("vn") = vn,
                                   Named("sn") = sn,
                                   Named("deltav") = deltav,
                                   Named("AX") = AX,
                                   Named("BX") = BX,
                                   Named("ABX") = ABX,
                                   Named("CX") = CX,
                                   Named("SDX") = SDX,
                                   Named("SDV") = SDV,
                                   Named("CLDV") = CLDV,
                                   Named("OPDV") = OPDV,
                                   Named("BMAX") = BMAX,
                                   Named("B_App") = B_App,
                                   Named("B_Emg") = B_Emg,
                                   Named("BNULL") = BNULL,
                                   Named("cf_state_sim") = cf_state_sim);

  return df;


}

