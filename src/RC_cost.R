
source("/Users/ynz23/Desktop/RC_data.R")

################################# Inputs ########################################
# Building design parameters
n_story = 12           # Number of stories
l1 = 120               # Length in one direction (ft)
l2 = 120               # Length in another direction (ft)
H = sum(rep(c(15,13),times=c(1,n_story-1)))           # Building height (ft)
area = l1 * l2         # Floor area (ft2)
n_gcol = 9             # Number of gravity columns per floor
n_fcol = 24            # Number of perimeter columns per floor
l_bm = 2 * (l1 + l2)   # length of perimeter beams per floor (ft)
l_slab = 30            # span of slab (ft)
d_slab = 8             # thickness of slab (in)

# Gravity column design parameters
gravity <- data.frame("story"=1:n_story,
                      "d"=(rep(c(24),times=c(n_story))),
                      "b"=(rep(c(24),times=c(n_story))),
                      "h"=(rep(c(15,13),times=c(1,n_story-1))),
                      "ratio"=(rep(c(0.025),times=c(n_story))),
                      "sh_ratio"=(rep(c(0.0093),times=c(n_story))),
                      "sh_spacing"=(rep(c(3.5),times=c(n_story)))
)

# Perimeter column design parameters
column <- data.frame("story"=1:n_story,
                    ## TODO: generalize with ifelse
                    "d"=(rep(c(36,30),times=c(8,4))),
                    "b"=(rep(c(24),times=c(n_story))),
                    "h"=(rep(c(15,13),times=c(1,n_story-1))),
                    ## TODO: generalize with ifelse
                    "in_ratio"=(rep(c(0.0127,0.0131,0.0109),times=c(8,2,2))),
                    "ex_ratio"=(rep(c(0.0182,0.0109),times=c(1,n_story))),
                    "insh_ratio"=(rep(c(0.0093),times=c(n_story))),
                    "insh_spacing"=(rep(c(3.5),times=c(n_story))),
                    "exsh_ratio"=(rep(c(0.0093,0.0070),times=c(1,n_story_1))),
                    "exsh_spacing"=(rep(c(3.5),times=c(n_story)))
)

# Perimeter beam design parameters
beam <- data.frame("story"=1:n_story,
                   ## TODO: generalize with ifelse
                   "d"=(rep(c(36,30),times=c(8,4))),
                   "b"=(rep(c(24),times=c(n_story))),
                   "l"=(rep(c(l_bm),times=c(n_story))),
                   ## TODO: generalize with ifelse
                   "top_ratio"=(rep(c(0.0061,0.0069,0.0076,0.0069,0.0061,0.0075,0.0066),times=c(1,1,2,2,2,1,3))),
                   ## TODO: generalize with ifelse
                   "btm_ratio"=(rep(c(0.0045,0.0052,0.0060,0.0052,0.0045,0.0036),times=c(1,1,2,3,2,3))),
                   ## TODO: generalize with ifelse
                   "sh_ratio"=(rep(c(0.0036,0.0041,0.0047,0.0041,0.0036,0.0030,0.0027),times=c(1,1,2,2,3,2,1))),
                   ## TODO: generalize with ifelse
                   "sh_spacing"=(rep(c(4.5,4,3.5,4,4.5,4,5.5,6),times=c(1,1,2,2,3,1,1,1)))
)

# Slab design parameters
slab <- data.frame("story"=1:n_story,
                   "d"=(rep(c(d_slab),times=c(n_story))),
                   "b"=(rep(c(l_slab),times=c(n_story))),
                   "l"=(rep(c(l_slab),times=c(n_story)))
)

# Assumptions
n_col = 12  # Number of steel bars in the column
n_col_tie = 6  # Number of shear reinforcements in the column
n_bm_top = 4  # Number of steel bars at top
n_bm_btm = 4  # Number of steel bars at bottom
n_bm_stirrup = 6 # Number of shear reinforcements in the beam
n_windows = 24  # Number of exterior windows per floor
n_doors = 1  # Number of exterior doors
n_stairs = (n_story -1) * 2  # Number of stairs
n_elevators = 1  # Number of elevators

############################### Calculation #####################################
# Gravity columns
## TODO: is this specific to 12 story?
col_s <- c(col_s3, col_s4, col_s5, col_s6, col_s7, col_s8, col_s9, col_s10, col_s11, col_s14, col_s18)

selection <- function(x){
  ## TODO: is this specific to 12 story?
  bar_size <- c(0.11, 0.2, 0.31, 0.44, 0.6, 0.79, 1, 1.27, 1.56, 2.25, 4)
  temp <- bar_size - x
  upper <- min(bar_size[temp > 0])
  lower <- max(bar_size[temp < 0])
  col_s[match(upper,bar_size)]
}

## TODO: is 144 specific to 12 story?
gra_concrete = (gravity$d * gravity$b / 144) * gravity$h * c1   # 8000 psi concrete

gra_steel = (gravity$d * gravity$b * gravity$ratio)/8  # assuming 8 steel bars per column
gra_steel <- sapply(gra_steel, selection) * 8 * gravity$h * n_gcol  # assuming 8 steel bars per column
  
gra_tie = (gravity$d * gravity$b * gravity$sh_ratio)/n_col_tie
## TODO: are these parameters specific to 12 story?
gra_tie <- sapply(gra_tie, selection) * (gravity$d *6 + gravity$b * 2) /12 * gravity$h * 12 / gravity$sh_spacing * n_gcol
  
gra_total = sum(gra_concrete) + sum(gra_steel) + sum(gra_tie)


# Perimeter columns
col_concrete = (column$d * column$b / 144) * column$h * c1 * n_fcol   # 8000 psi concrete
  
col_in_steel = (column$d * column$b * column$in_ratio)/n_col
col_in_steel <- sapply(col_in_steel, selection) * n_col * column$h * (n_fcol - 4)

col_ex_steel = (column$d * column$b * column$ex_ratio)/n_col
col_ex_steel <- sapply(col_ex_steel, selection) * n_col * column$h * 4

col_in_tie = (column$d * column$b * column$insh_ratio)/n_col_tie
## TODO: are these parameters specific to 12 story?
col_in_tie <- sapply(col_in_tie, selection) * (4 + 2/3) * (column$d + column$b) /12 * column$h * 12 / column$insh_spacing * n_col

col_ex_tie = (column$d * column$b * column$exsh_ratio)/n_col_tie
## TODO: are these parameters specific to 12 story?
col_ex_tie <- sapply(col_ex_tie, selection) * (4 + 2/3) * (column$d + column$b) /12 * column$h * 12 / column$exsh_spacing * n_col

col_total = sum(col_concrete) + sum(col_in_steel) + sum(col_ex_steel) + sum(col_in_tie) + sum(col_ex_tie)


# Beams
## TODO: is this specific to 12 story?
bm_s <- c(bm_s3, bm_s4, bm_s5, bm_s6, bm_s7, bm_s8, bm_s9, bm_s10, bm_s11, bm_s14, bm_s18)

selection2 <- function(x){
  ## TODO: is this specific to 12 story?
  bar_size <- c(0.11, 0.2, 0.31, 0.44, 0.6, 0.79, 1, 1.27, 1.56, 2.25, 4)
  temp <- bar_size - x
  upper <- min(bar_size[temp > 0])
  lower <- max(bar_size[temp < 0])
  bm_s[match(upper,bar_size)]
}

## TODO: is 144 specific to 12 story?
bm_concrete = (beam$d * beam$b / 144) * beam$l * c2   # 5000 psi concrete

bm_top_steel = (beam$d * beam$b * beam$top_ratio) / n_bm_top
bm_top_steel <- sapply(bm_top_steel, selection2) * n_bm_top * beam$l 
  
bm_btm_steel = (beam$d * beam$b * beam$btm_ratio) / n_bm_btm
bm_btm_steel <- sapply(bm_btm_steel, selection2) * n_bm_btm * beam$l 
  
bm_stirrup = (beam$d * beam$b * beam$sh_ratio) / n_bm_stirrup
## TODO: are these parameters specific to 12 story?
bm_stirrup <- sapply(bm_stirrup, selection2) * (beam$d * 6 + beam$b * 2) /12 * beam$l * 12 / beam$sh_spacing

bm_total = sum(bm_concrete) + sum(bm_top_steel) + sum(bm_btm_steel) + sum(bm_stirrup)


# Slabs
## TODO: is this specific to 12 story?
slab = (slab$d / 12) * area * slab_price
slab_total = sum(slab)

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

################################### Outputs #####################################
total <- c(gra_total, col_total, bm_total, slab_total, wall_total, window_total,
           door_total, stair_total, elevator_total)

total_cost = sum(total)

p = total / total_cost * 100  # convert to percentage

## get cost per sq ft
total_area = area * n_story
unit_cost = total_cost / total_area
