
source("/Users/ynz23/Desktop/BRBF_data.R")

##################################### Inputs ####################################
# Building design parameters
n_story = 12           # Number of stories
l1 = 120               # Length in one direction (ft)
l2 = 120               # Length in another direction (ft)
area = l1 * l2         # Floor area (ft2)
H = 13 * n_story       # Building height (ft)
l_bm = 2 * (l1 + l2)   # length of perimeter beams per floor (ft)
depth = 3.5            # Depth of concrete above deck (in)
purlin = "W18X35"      # Size of purlin
l_purlin = 1440        # Total length of purlins per floor (ft)

# Gravity columns
gcol <- data.frame("story"=1:n_story,
                  "size"=c("W12X210","W12X210","W12X190","W12X190","W12X170","W12X170",
                           "W12X136","W12X136","W12X120","W12X120","W12X87","W12X87"),
                  "l"=c(rep(c(221),times=c(n_story)))  # total length of gravity columns per floor (ft)
                  )

# Frame columns
fcol <- data.frame("story"=1:n_story,
                  "size"=c("W14X283","W14X283","W14X257","W14X257","W14X211","W14X211",
                           "W14X211","W14X211","W14X145","W14X145","W14X145","W14X145"),
                  "l"=c(rep(c(208),times=c(n_story)))  # total length of frame columns per floor (ft)
                  )

# Gravity beams
gbm <- data.frame("story"=1:n_story,
                 "size"=c("W18X76","W18X76","W18X76","W18X76","W18X76","W18X76",
                          "W18X76","W18X76","W18X76","W18X76","W18X76","W18X76"),
                 "l"=c(rep(c(1080),times=c(n_story)))  # total length of gravity beams per floor (ft)
                 )

# Frame beams
fbm <- data.frame("story"=1:n_story,
                 "size"=c("W21X73","W21X73","W21X73","W21X62","W21X62","W21X62",
                          "W21X62","W21X62","W21X62","W21X62","W21X50","W21X50"),
                 "l"=c(rep(c(120),times=c(n_story)))  # total length of frame beams per floor (ft)
                 )

# Braces
brb <- data.frame("story"=1:n_story,
                "core"=c(9,9,8,8,7,7,6,6,6,5,4,3),  # sectional area of steel core (in2)
                "casing"=c(rep(c("12x12"),times=c(n_story))),  # type of casing, options = {"6x6","8x8","10x10","12x12","14x14"}  
                "l"=c(rep(c(158.8),times=c(n_story)))  # total length of braces per floor (ft)
                )


# Assumptions
n_windows = 24               # Number of exterior windows per floor
n_doors = 1                  # Number of exterior doors
n_stairs = (n_story -1) * 2  # Number of stairs
n_elevators = 1              # Number of elevators

################################## Calculation ##################################
## compute unit cost by interpolation or extrapolation
interpolate <- function(x){
  a <- grep(x, W$type, ignore.case =T)
  if (length(a) > 0) {
    W$cost[a]
  } else {
      b <- grep(x, W_search$type, ignore.case =T)
      area <- W_search$area[b]
      temp <- W$area - area
      lower <- max(W$area[temp <= 0])
      W$cost[match(lower,W$area)] + (area - lower) * W_delta
      }
}

# Gravity columns
gcol_total = sum(sapply(gcol$size, interpolate) * gcol$l)

# Frame columns
fcol_total = sum(sapply(fcol$size, interpolate) * fcol$l)

# Gravity beams
gbm_total = sum(sapply(gbm$size, interpolate) * gbm$l)

# Frame beams
fbm_total = sum(sapply(fbm$size, interpolate) * fbm$l)


## select W beam for steel core based on sectional area
select <- function(x){
  temp <- W_search$area - x
  upper <- min(W_search$area[temp >= 0])
  W_search$type[match(upper,W_search$area)]
}

## select brace and return unit cost
cost_tube <- function(x){
  a <- grep(x, brace$type, ignore.case =T)
  brace$tube_cost[a]
}

cost_grout <- function(x){
  a <- grep(x, brace$type, ignore.case =T)
  brace$grout_cost[a]
}

# Braces
core_type = sapply(brb$core, select)
core_total = sum(sapply(core_type, interpolate) * brb$l)
casing_total = sum(sapply(brb$casing, cost_tube) * brb$l)
grout_total =  sum(sapply(brb$casing, cost_grout) * brb$l)
brb_total = (core_total + casing_total + grout_total) * 1.5

# Slabs
deck_total = deck_price * area * n_story
concrete_total = concrete_price * area * depth /12 * n_story
purlin_total = unname(sapply(purlin, interpolate)) * l_purlin * n_story
slab_total = deck_total + concrete_total + purlin_total

# Exterior walls
wall_total = (H * l_bm - n_story * n_windows * window_area) * wall_price  

# Exterior windows
window_total = n_story * n_windows * (window_glass_price + window_frame_price)

# Exterior doors
door_total = n_doors * door_price

# Stairs
stair_total = n_stairs * stair_price

# Elevators
elevator_total = n_elevators * elevator_price

################################# Outputs #######################################
total <- c(gcol_total, fcol_total, gbm_total, fbm_total, slab_total, wall_total, 
           window_total, door_total, stair_total, elevator_total)

total_cost = sum(total)

p = total / total_cost * 100  # convert to percentage
