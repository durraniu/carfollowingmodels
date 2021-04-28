#' Simulate Gipps Model
#'
#' @description This function takes in the lead vehicle trajectory and calculates speed, spacing and acceleration of the following vehicle using the Gipps Model.
#'
#' @param resolution Duration of a time frame. Typical values are 0.1, 0.5, 1.0 s. Double. Must match with the resolution of the observed lead vehicle data dfn1 defined below
#' @param N Number of Following Vehicles in the same lane (platoon). Integer.
#' @param dfn1 Unquoted name of the dataframe that contains lead vehicle data.
#' @param xn1 Name of the column in dfn1 that contains lead vehicle position. Character.
#' @param vn1 Name of the column in dfn1 that contains lead vehicle speed. Character.
#' @param xn_first First value of vehicle position of each of the following vehicles. A list of doubles with size equal to N.
#' @param vn_first First value of vehicle speed of each of the following vehicles. A list of doubles with size equal to N.
#' @param ln Length of each of the lead vehicles. A list of doubles with size equal to N. Note that it is called as sn in Gipps Model
#' @param an Maximum acceleration which the driver wishes to undertake m/s2. Double.
#' @param Vn Desired speed/speed at which driver  wishes to travel m/s. Double.
#' @param tau Reaction Time s. Double.
#' @param bn_const Most severe braking that the driver wishes to undertake m/s2. Double and Negative.
#' @param bcap An estimate of lead vehicle deceleration m/s2. Double and Negative.
#'
#' @return A dataframe with lead and following vehicle(s) trajectories
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
#' ## Run the Gipps function:
#' simulate_gipps(
#'
#'############## Simulation Parameters #######################
#'resolution=0.1,
#'N=5,
#'
#'
#'############### Lead Vehicle Data #########################
#'dfn1=ldf,
#'xn1="xn1_complete",
#'vn1="vn1_complete",
#'
#'
#'############### Following Vehicle Data ####################
#'xn_first=list(85, 70, 55, 40, 25),
#'vn_first=list(12, 12, 12, 12, 12),
#'ln=list(6.5, 6.5, 6.5, 6.5, 6.5),
#'
#'
#'############### Model Parameters ##########################
#'an=2,
#'Vn=14.4,
#'tau=0.1,
#'bn_const=-1.5,
#'bcap=-2
#')
simulate_gipps <- function(

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
  ln, # Effective size of each of the lead vehicles i.e. vehicle length plus margin of safety. A list of doubles with size equal to N.


  ############### Model Parameters ##########################
  an, # Maximum acceleration which the driver wishes to undertake m/s2. Double.
  Vn, # Desired speed/speed at which driver  wishes to travel m/s. Double.
  tau, # Reaction Time s. Double.
  bn_const, # Most severe braking that the driver wishes to undertake m/s2. Double and Negative.
  bcap # An estimate of lead vehicle deceleration m/s2. Double and Negative.



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

    # free-flow speed
    vn_ff <- rep(NA_real_, time_length)


    # car-following speed
    vn_cf <- rep(NA_real_, time_length)

    # speed
    vn <- rep(NA_real_, time_length)

    # position
    xn <- rep(NA_real_, time_length)

    # spacing
    sn <- rep(NA_real_, time_length)

    # speed difference
    deltav <- rep(NA_real_, time_length)

    # acceleration rate
    bn <- rep(NA_real_, time_length)



    ######## Initial values for Following vehicle ##################################

    # speed
    vn_ff[1] <- vn_first[[n]]
    vn_ff[1] <- vn_first[[n]]
    vn[1] <- vn_first[[n]]

    # position
    xn[1] <- xn_first[[n]]

    # spacing
    sn[1] <- xn1[1] - xn_first[[n]]

    # speed difference
    deltav[1] <- vn_first[[n]] - vn1[1]



    ###### Gipps Calculations ############################

    result_dfn <- for_loop_gipps(resolution,
                                 n,
                                 time_length,
                                 tau,
                                 an,
                                 bn_const,
                                 Vn,
                                 bcap,
                                 ln1,

                                 Time,
                                 vn_ff,
                                 vn_cf,
                                 vn,
                                 vn1,
                                 sn,
                                 xn,
                                 xn1,
                                 deltav,
                                 bn
    )

    ################## Result in a dataframe ###################################




    list_of_N_veh[[n]] <- result_dfn

    xn1 <- result_dfn$xn
    vn1 <- result_dfn$vn


  }

  result <- do.call("rbind", list_of_N_veh)

  # return the result dataframe
  return(result)

}
