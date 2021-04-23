simulate_idm2 <- function(

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
    v_dot <- rep(NA_real_, time_length)

    # speed
    vn <- rep(NA_real_, time_length)

    # position
    xn <- rep(NA_real_, time_length)

    # spacing
    sn <- rep(NA_real_, time_length)

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


    sn[1] <- abs(xn1[1] - xn_first[[n]]) - ln1



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

    for (t in 1:(time_length-1)) {

      # desired spacing
      sn_star[t] <- s_0 + max(0, (vn[t] * Tg) + ((vn[t] * deltav[t]) / (2 * sqrt((a * b)))))


      # acceleration rate
      v_dot[t] <- ifelse (

        is.na(sn_star[t]),

        a * (1 - ((vn[t] / v_0)^small_delta)),

        a * (1 - ((vn[t] / v_0)^small_delta) - ((sn_star[t] / sn[t])^2))

      )


      # v_dot[t] <- ifelse (v_dot[t] < BMIN, BMIN, v_dot[t])

      v_dot[t] <- ifelse (v_dot[t] < -b, -b, v_dot[t])


      # speed
      vn[t+1] <- vn[t] + (v_dot[t] * resolution)

      ### if the speed is negative, make it zero
      vn[t+1] <- ifelse(vn[t+1] < 0, 0, vn[t+1])



      # position
      xn[t+1] <- xn[t] + (vn[t] * resolution) + (0.5 * v_dot[t] * (resolution)^2)

      # spacing
      sn[t+1] <- abs(xn1[t+1] - xn[t+1]) - ln1

      # speed difference
      deltav[t+1] <- vn[t+1] - vn1[t+1]


    }

    ################## Result in a dataframe ###################################

    result_dfn <- data.frame(fvn=n, Time, xn1, vn1, ln1, sn_star, v_dot, xn, vn, sn, deltav)


    list_of_N_veh[[n]] <- result_dfn

    xn1 <- xn
    vn1 <- vn


  }

  result <- do.call("rbind", list_of_N_veh)

  # return the result dataframe
  return(result)
}
