#' Estimate the Sum of Squared Errors (SSE) from the Predicted and Observed Car-following Data
#'
#' @description Estimate the SSE using formulae provided in Chapter 16 (pg. 319) of the Traffic Flow Dynamics book (first edition) by Martin Treiber and Arne Kesting
#'
#' @param y_pred Predicted bumper-to-bumper spacing, speed or acceleration. A vector of doubles.
#' @param y_data Observed bumper-to-bumper spacing, speed or acceleration. A vector of doubles. It should have the same length as y_pred.
#' @param type Provide the type of SSE you want to calculate. Three options: "abs", "rel" and "mix" (default).
#'
#'
#' @return Sum of Squared Errors. Double. Either of absolute, relative or mixed.
#' @export
#' @examples
#'# Time
#'last_time <- 3000 ## s
#'time_frame <- 0.1 ## s
#'Time <- seq(from = 0, to = last_time, by = time_frame)
#'time_length <- length(Time)
#'
#'
#'
#'## Lead vehicle
#'vn1_first <- 13.9 ## first speed m/s
#'xn1_first <- 100 ## position of lead vehicle front center m
#'bn1_complete <- c(rep(0, 29500),
#'                  rep(-5, time_length - 29500))
#'
#'
#'
#'#############################################
#'### Complete speed trajectory of Lead vehicle
#'#############################################
#'
#'vn1_complete <- rep(NA_real_, time_length) ### an empty vector
#'xn1_complete <- rep(NA_real_, time_length) ### an empty vector
#'
#'vn1_complete[1] <- vn1_first
#'xn1_complete[1] <- xn1_first
#'
#'for (t in 2:time_length) {
#'
#'  ### Lead vehicle calculations
#'  vn1_complete[t] <- vn1_complete[t-1] + (bn1_complete[t-1] * time_frame)
#'
#'  vn1_complete[t] <- ifelse(vn1_complete[t] < 0, 0, vn1_complete[t])
#'
#'
#'  xn1_complete[t] <- xn1_complete[t-1] + (vn1_complete[t-1] * time_frame) +
#'    (0.5 * bn1_complete[t-1] * (time_frame)^2)
#'
#'}
#'
#'
#'
#'ldf <- data.frame(Time, bn1_complete, xn1_complete, vn1_complete)
#'
#'## Assuming that the observed following vehicle trajectory is approximated by IDM:
#'obs_data <- simulate_idm(
#'
#'  resolution=0.1,
#'  N=1,
#'
#'  dfn1=ldf,
#'  xn1="xn1_complete",
#'  vn1="vn1_complete",
#'
#'  xn_first=list(85),
#'  vn_first=list(12),
#'  ln=list(5),
#'
#'  a=2,
#'  v_0=14.4,
#'  small_delta=1,
#'  s_0=4,
#'  Tg=1,
#'  b=1.5
#')
#'
#' # Remove the zero speed part in observed data:
#' obs_data <- obs_data[obs_data$vn > 0,]
#'
#'# Predicting with Wiedemann model
#'pred_data <- simulate_wiedemann74_driver(
#'  resolution=0.1,
#'  N=1,
#'  dfn1=ldf,
#'  xn1="xn1_complete",
#'  vn1="vn1_complete",
#'  bn1="bn1_complete",
#'  xn_first=list(85),
#'  vn_first=list(12),
#'  ln=list(5),
#'  D_MAX=150,
#'  V_MAX=44,
#'  V_DESIRED=14.4,
#'  FAKTORVmult=0.001,
#'  BMAXmult=0.08,
#'  BNULLmult=0.25,
#'  BMIN=-5,
#'  CX=50,
#'  AXadd=2,
#'  BXadd=2,
#'  EXadd=2,
#'  OPDVadd=1.5
#')
#'
#'pred_data <- pred_data[1:nrow(obs_data), ]
#'
#'
#'
#'# Now we can estimate the sum of squared errors:
#'sse(pred_data$sn, obs_data$sn, type = "mix")
#'sse(pred_data$vn, obs_data$vn, type = "abs")
sse <- function(y_pred, y_data, type = "mix"){

  if (type == "mix"){

    sse <- sum(((y_pred - y_data)^2) / abs(y_data), na.rm = TRUE)  / sum(abs(y_data), na.rm = TRUE)

  } else if (type == "abs") {

    sse <- sum(((y_pred - y_data)^2), na.rm = TRUE) / sum(((y_data)^2), na.rm = TRUE)

  } else if (type == "rel") {

    sse <- sum((((y_pred - y_data) / y_data)^2), na.rm = TRUE) / length(na.omit(y_data))

  } else {

    sse <- sum(((y_pred - y_data)^2) / abs(y_data), na.rm = TRUE)  / sum(abs(y_data), na.rm = TRUE)

  }

  return(sse)

}


