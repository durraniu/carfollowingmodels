#' Simulate Intelligent Driver Model
#'
#' @description This function takes in the lead vehicle trajectory and calculates speed, spacing and acceleration of the following vehicle using the Intelligent Driver Model.
#'
#' @param resolution Duration of a time frame. Typical values are 0.1, 0.5, 1.0 s. Double. Must match with the resolution of the observed lead vehicle data dfn1 defined below
#' @param N Number of Following Vehicles in the same lane. Integer.
#' @param dfn1 Unquoted name of the dataframe that contains lead vehicle data.
#' @param xn1 Name of the column in dfn1 that contains lead vehicle position. Character.
#' @param vn1 Name of the column in dfn1 that contains lead vehicle speed. Character.
#' @param xn_first First value of vehicle position of each of the following vehicles. A list of doubles with size equal to N.
#' @param vn_first First value of vehicle speed of each of the following vehicles. A list of doubles with size equal to N.
#' @param ln Length of each of the lead vehicles. A list of doubles with size equal to N.
#' @param a Acceleration rate starting from zero speed m/s2. Double.
#' @param v_0 Desired speed m/s. Double.
#' @param small_delta Acceleration exponent. Double.
#' @param s_0 standstill bumper-to-bumper spacing m. Double.
#' @param Tg Bumper-to-bumper time gap. Double.
#' @param b Comfortable maximum deceleration rate m/s2. Double and Positive.
#'
#' @return A dataframe with lead and following vehicle(s) trajectories
#' @useDynLib carfollowingmodels, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @export
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
#' ## Run the IDM function:
#' simulate_idm(
#'
#'resolution=0.1,
#'N=5,
#'
#'dfn1=ldf,
#'xn1="xn1_complete",
#'vn1="vn1_complete",
#'
#'xn_first=list(85, 70, 55, 40, 25),
#'vn_first=list(12, 12, 12, 12, 12),
#'ln=list(5, 5, 5, 5, 5),
#'
#'a=2,
#'v_0=14.4,
#'small_delta=1,
#'s_0=4,
#'Tg=1,
#'b=1.5
#')
simulate_idm <- function(

  ############## Simulation Parameters #######################
  resolution, # Duration of a time frame. Typical values are 0.1, 0.5, 1.0 s. Double. Must match with the resolution of the observed lead vehicle data dfn1
  N, # Number of Following Vehicles in the same lane (platoon). Integer.


  ############### Lead Vehicle Data #########################
  dfn1, # Name (unquoted) of the dataframe that contains lead vehicle data.
  xn1, # Name of the column in dfn1 that contains lead vehicle position. Character.
  vn1, # Name of the column in dfn1 that contains lead vehicle speed. Character.



  ############### Following Vehicle Data ####################
  xn_first, # First value of vehicle position of each of the following vehicles. A list of doubles with size equal to N.
  vn_first, # First value of vehicle speed of each of the following vehicles. A list of doubles with size equal to N.
  ln, # Length of each of the lead vehicles. A list of doubles with size equal to N.



  ############### Model Parameters ##########################
  a, # Acceleration rate starting from zero speed m/s2. Double.
  v_0, # Desired speed m/s. Double.
  small_delta, # Acceleration exponent. Double.
  s_0, # standstill bumper-to-bumper spacing m. Double.
  Tg, # Time gap (bumper-to-bumper). Double.
  b # Comfortable maximum deceleration rate m/s2. Double.



) {




  ####### Time #############################################

  # Last time frame of the simulation:
  last_time <- (nrow(dfn1) - 1) * resolution

  # Time vector:
  Time <- seq(from = 0, to = last_time, by = resolution)

  # Length of the Time vector
  time_length <- length(Time)






  list_of_N_veh <- vector(mode = "list", length = N)


  for (n in seq_along(list_of_N_veh)) {


    ####### Assign names to Lead Vehicle Parameters ##########

    if (n == 1L) {

      # Lead vehicle position
      xn1 <- dfn1[[xn1]]

      # Lead vehicle speed
      vn1 <- dfn1[[vn1]]


    }


    ln1 <- ln[[n]]



    ####### Allocate Vectors ##################################

    # acceleration rate
    bn <- rep(NA_real_, time_length)

    # speed
    vn <- rep(NA_real_, time_length)

    # position
    xn <- rep(NA_real_, time_length)

    # spacing
    sn <- rep(NA_real_, time_length)

    # bumper-to-bumper spacing
    frsn <- rep(NA_real_, time_length)

    # speed difference
    deltav <- rep(NA_real_, time_length)

    # desired spacing
    sn_star <- rep(NA_real_, time_length)



    ######## Initial values for Following vehicle ##################################

    # speed
    vn[1] <- vn_first[[n]]

    # position
    xn[1] <- xn_first[[n]]

    # spacing
    # sn[1] <- ifelse (
    #
    #   n == 1L,
    #
    #   abs(xn1[1] - xn_first[[n]]) - ln1,
    #
    #   abs(xn_first[[n-1]] - xn_first[[n]]) - ln[[n-1]]
    #
    # )


    sn[1] <- xn1[1] - xn_first[[n]]
    frsn[1] <- sn[1] - ln1



    # speed difference
    # deltav[1] <- ifelse (
    #
    #   n == 1L,
    #
    #   vn_first[[n]] - vn1[1],
    #
    #   vn_first[[n]] - vn_first[[n-1]]
    #
    # )


    deltav[1] <- vn_first[[n]] - vn1[1]



    # maximum deceleration
    # BMIN <- -8


    ###### IDM Calculations ############################

    result_dfn <- for_loop_idm(resolution,
                               n,
                               time_length,
                               s_0,
                               Tg,
                               a,
                               b,
                               v_0,
                               small_delta,
                               ln1,

                               Time,
                               vn,
                               vn1,
                               sn_star,
                               sn,
                               frsn,
                               xn,
                               xn1,
                               deltav,
                               bn
    )

    ################## Result in a dataframe ###################################

    # result_dfn <- data.frame(fvn=n, Time, xn1, vn1, ln1, sn_star, bn, xn, vn, sn, deltav)


    list_of_N_veh[[n]] <- result_dfn

    xn1 <- result_dfn$xn
    vn1 <- result_dfn$vn


  }

  result <- do.call("rbind", list_of_N_veh)

  # return the result dataframe
  return(result)
}
