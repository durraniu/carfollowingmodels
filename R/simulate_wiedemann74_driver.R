#' Simulate Wiedemann74 Model with single driver data
#'
#' @description This function takes in the lead vehicle trajectory and calculates speed, spacing and acceleration of the following vehicle using the Wiedemann74 Model.It also estimates the car-following state in each time frame.
#'
#' @param resolution Duration of a time frame. Typical values are 0.1, 0.5, 1.0 s. Double. Must match with the resolution of the observed lead vehicle data dfn1 defined below
#' @param N Number of Following Vehicles in the same lane. Integer.
#' @param dfn1 Unquoted name of the dataframe that contains lead vehicle data.
#' @param xn1 Name of the column in dfn1 that contains lead vehicle position. Character.
#' @param vn1 Name of the column in dfn1 that contains lead vehicle speed. Character.
#' @param bn1 Name of the column in dfn1 that contains lead vehicle acceleration. Character.
#' @param xn_first First value of vehicle position of each of the following vehicles. A list of doubles with size equal to N.
#' @param vn_first First value of vehicle speed of each of the following vehicles. A list of doubles with size equal to N.
#' @param ln Length of each of the lead vehicles. A list of doubles with size equal to N.
#' @param D_MAX Upper limit of reaction spacing. Double. Typical value is 150 m.
#' @param V_MAX Maximum speed of the following vehicle model. Double. Typical values are 40 m/s, 60 m/s.
#' @param V_DESIRED Desired speed of the following driver. Double.
#' @param FAKTORVmult Controls acceleration in Free-driving state. Double. Higher values will result in large acceleration.
#' @param BMAXmult Controls acceleration in Free-driving state. Double. It can be calculated as the maximum acceleration of the following vehicle model divided by V_MAX.
#' @param BNULLmult Controls oscillation in speed in Following state. Double. Typical value is 0.25 m/s2.
#' @param BMIN Controls maximum deceleration in Approaching and Emergency-braking states. Double.
#' @param CX Calibration parameter. Double. It is a function of width of the lead vehicle and the angular velocity threshold at which driver slows down during Approaching state. Typical values are 20 - 75.
#' @param AXadd Calibration parameter. Double. It is the bumper-to-bumper distance when both vehicles are stopped. Typical values are 2 - 4 m.
#' @param BXadd Calibration parameter. Double. It controls the speed dependent spacing to the lead vehicle. Typical value is 2.
#' @param EXadd Calibration parameter. Double. It controls the maximum spacing to the lead vehicle, as well as the speed difference in perception of closing in. Typical value is 2.
#' @param OPDVadd Calibration parameter. Double. It controls the speed difference in perception of the opening process. Typical value is 1.5.
#'
#' @return A dataframe with lead and following vehicle(s) trajectories. It also returns all the pereception thresholds.
#' @export
#'
#' @examples
#' # Time
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
#'   (0.5 * bn1_complete[t-1] * (time_frame)^2)
#'
#'}
#'
#'
#'
#'ldf <- data.frame(Time, bn1_complete, xn1_complete, vn1_complete)
#'
#' # Run the Wiedemann function:
#'simulate_wiedemann74_driver(
#'  resolution=0.1,
#'  N=5,
#'  dfn1=ldf,
#'  xn1="xn1_complete",
#'  vn1="vn1_complete",
#'  bn1="bn1_complete",
#'  xn_first=list(85, 70, 55, 40, 25),
#'  vn_first=list(12, 12, 12, 12, 12),
#'  ln=list(5, 5, 5, 5, 5),
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
simulate_wiedemann74_driver <- function(
  resolution,
  N,
  dfn1,
  xn1,
  vn1,
  bn1,
  xn_first,
  vn_first,
  ln,
  D_MAX,
  V_MAX,
  V_DESIRED,
  FAKTORVmult,
  BMAXmult,
  BNULLmult,
  BMIN,
  CX,
  AXadd,
  BXadd,
  EXadd,
  OPDVadd
  ) {


  ####### Time #############################################

  # Last time frame of the simulation
  last_time <- (nrow(dfn1) - 1) * resolution

  # Time vector:
  Time <- seq(from = 0, to = last_time, by = resolution)

  # Length of the Time vector
  time_length <- length(Time)

  # Initialize list of N following vehicles
  list_of_N_veh <- vector(mode = "list", length = N)





  # Run the model for N following vehicles
  for (n in seq_along(list_of_N_veh)) {


    ####### Assign names to Lead Vehicle Parameters ##########

    if (n == 1L) {

      # Lead vehicle position
      xn1 <- dfn1[[xn1]]

      # Lead vehicle speed
      vn1 <- dfn1[[vn1]]

      # Lead vehicle acceleration
      bn1 <- dfn1[[bn1]]


    }

    # Length of lead vehicle
    ln1 <- ln[[n]]

    # # Width of lead vehicle
    # wn1 <- wn[[n]]



    # Standstill Spacing
    AX = ln1 + AXadd

    # Calibration parameter for CLDV and SDX
    EX = EXadd

    # Acceleration/Deceleration during Following state
    BNULL = BNULLmult

    # Calibration parameter for BMAX
    FaktorV = V_MAX / (V_DESIRED + FAKTORVmult * (V_MAX - V_DESIRED))



    ####### Allocate Vectors ##################################

    # Speed of following vehicle
    vn <- rep(NA_real_, times = length(Time))

    # Position of following vehicle
    xn <- rep(NA_real_, times = length(Time))

    # Speed difference
    deltav <- rep(NA_real_, times = length(Time))

    # Spacing
    sn <- rep(NA_real_, times = length(Time))



    ######## Initial values for Following vehicle ##################################

    # speed
    vn[1] <- vn_first[[n]]

    # position
    xn[1] <- xn_first[[n]]

    # spacing
    sn[1] <- abs(xn1[1] - xn_first[[n]])

    # speed difference
    deltav[1] <- vn_first[[n]] - vn1[1]


    ###### Wiedemann Calculations ############################


    result_dfn <- for_loop_wiedemann(D_MAX,
                                     time_length,
                                     BXadd,
                                     AX,
                                     CX,
                                     EX,
                                     OPDVadd,
                                     BMAXmult,
                                     V_MAX,
                                     FaktorV,
                                     BMIN,
                                     BNULL,
                                     resolution,
                                     vn,
                                     vn1,
                                     sn,
                                     xn,
                                     xn1,
                                     deltav,
                                     bn1)

    ################## Result in a dataframe ###################################
    fvn <- n
    result_dfn <- cbind(fvn, Time, result_dfn, ln1)

    list_of_N_veh[[n]] <- result_dfn

    xn1 <- result_dfn$xn
    vn1 <- result_dfn$vn
    bn1 <- result_dfn$bn

  }


  result <- do.call("rbind", list_of_N_veh)

  # return the result dataframe
  return(result)


}
