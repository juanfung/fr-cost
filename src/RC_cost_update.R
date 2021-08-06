
source("/Users/ynz23/Desktop/RC_data.R")

raw <- read.csv(file = '/Users/ynz23/Desktop/test.csv')

################################# Inputs ########################################
# Building design parameters
n_story = 20           # Number of stories
l1 = 120               # Dimension in one direction (ft)
l2 = 120               # Dimension in another direction (ft)
H = sum(rep(c(15,13),times=c(1,n_story-1)))           # Building height (ft)
area = l1 * l2         # Floor area (ft2)
n_gcol = 25            # Number of gravity columns per floor
n_fcol = 24            # Number of perimeter columns per floor
l_bm = 2 * (l1 + l2)   # length of perimeter beams per floor (ft)
l_slab = 20            # span of slab (ft)
d_slab = 8             # thickness of slab (in)
b_gravity = 24         # shorter side of gravity column (in)
d_gravity = 24         # longer side of gravity column (in)


# Assumptions
n_col = 12                   # Number of steel bars in the column
n_col_tie = 6                # Number of shear reinforcements in the column
n_bm_top = 4                 # Number of steel bars at top
n_bm_btm = 4                 # Number of steel bars at bottom
n_bm_stirrup = 6             # Number of shear reinforcements in the beam
n_windows = 24               # Number of exterior windows per floor
n_doors = 1                  # Number of exterior doors
n_stairs = (n_story -1) * 2  # Number of stairs
n_elevators = 2              # Number of elevators

############################### Data processing #####################################
raw1 <- raw[seq(1, n_story*2-1, by=2),]
raw2 <- raw[seq(2, n_story*2, by=2), ]

Col_ex_d = sub("x.*", "", raw1$Cex)
Col_ex_b = gsub(" .*$", "", sub(".*x", "", raw1$Cex))
Col_ex_ratio = sub(",.*", "", gsub("\\(|\\)", "", raw2$Cex))
Col_ex_sh_ratio = sub(".*, ", "", gsub("\\(|\\)", "", raw2$Cex))
Col_ex_sh_spacing = gsub("\\[|\\]", "", gsub(".* ", "", sub(".*x", "", raw1$Cex)))

Col_in_d = sub("x.*", "", raw1$Cin)
Col_in_b = gsub(" .*$", "", sub(".*x", "", raw1$Cin))
Col_in_ratio = sub(",.*", "", gsub("\\(|\\)", "", raw2$Cin)) 
Col_in_sh_ratio = sub(".*, ", "", gsub("\\(|\\)", "", raw2$Cin))
Col_in_sh_spacing = gsub("\\[|\\]", "", gsub(".* ", "", sub(".*x", "", raw1$Cin)))
  
Bm_d = sub("x.*", "", raw1$Bex)
Bm_b = gsub(" .*$", "", sub(".*x", "", raw1$Bex))
Bm_top_ratio = sub(",.*", "", gsub("\\(|\\)", "", raw2$Bex))
s = gsub("\\(|\\)", "", raw2$Bex)
Bm_btm_ratio = regmatches(s, regexec(",\\s*([^,]*)", s))[[1]][2]
Bm_sh_ratio = sub(".*, ", "", gsub("\\(|\\)", "", raw2$Bex))
Bm_sh_spacing = gsub("\\[|\\]", "", gsub(".* ", "", sub(".*x", "", raw1$Bex)))


# Gravity column design parameters
gravity <- data.frame("story"=1:n_story,
                      "d"=(rep(c(d_gravity),times=c(n_story))),
                      "b"=(rep(c(b_gravity),times=c(n_story))),
                      "h"=(rep(c(15,13),times=c(1,n_story-1))),
                      "ratio"=(rep(c(0.025),times=c(n_story))),
                      "sh_ratio"=as.numeric(Col_in_sh_ratio),
                      "sh_spacing"=as.numeric(Col_in_sh_spacing)
)

# Perimeter column design parameters
column <- data.frame("story"=1:n_story,
                     "in_d"=as.numeric(Col_in_d),
                     "in_b"=as.numeric(Col_in_b),
                     "ex_d"=as.numeric(Col_ex_d),
                     "ex_b"=as.numeric(Col_ex_b),
                     "h"=(rep(c(15,13),times=c(1,n_story-1))),
                     "in_ratio"=as.numeric(Col_in_ratio),
                     "ex_ratio"=as.numeric(Col_ex_ratio),
                     "insh_ratio"=as.numeric(Col_in_sh_ratio),
                     "insh_spacing"=as.numeric(Col_in_sh_spacing),
                     "exsh_ratio"=as.numeric(Col_ex_sh_ratio),
                     "exsh_spacing"=as.numeric(Col_ex_sh_spacing)
)

# Perimeter beam design parameters
beam <- data.frame("story"=1:n_story,
                   "d"=as.numeric(Bm_d),
                   "b"=as.numeric(Bm_b),
                   "l"=(rep(c(l_bm),times=c(n_story))),
                   "top_ratio"=as.numeric(Bm_top_ratio),
                   "btm_ratio"=as.numeric(Bm_btm_ratio),
                   "sh_ratio"=as.numeric(Bm_sh_ratio),
                   "sh_spacing"=as.numeric(Bm_sh_spacing)
)

# Slab design parameters
slab <- data.frame("story"=1:n_story,
                   "d"=(rep(c(d_slab),times=c(n_story))),
                   "b"=(rep(c(l_slab),times=c(n_story))),
                   "l"=(rep(c(l_slab),times=c(n_story)))
)

################################ Calculation #####################################
# Gravity columns
col_s <- c(col_s3, col_s4, col_s5, col_s6, col_s7, col_s8, col_s9, col_s10, col_s11, col_s14, col_s18)

selection <- function(x){
  bar_size <- c(0.11, 0.2, 0.31, 0.44, 0.6, 0.79, 1, 1.27, 1.56, 2.25, 4)
  temp <- bar_size - x
  upper <- min(bar_size[temp >= 0])
  lower <- max(bar_size[temp < 0])
  col_s[match(upper,bar_size)]
}

gra_concrete = (gravity$d * gravity$b / 144) * gravity$h * c1   # 8000 psi concrete

gra_steel = (gravity$d * gravity$b * gravity$ratio)/8  # assuming 8 steel bars per column
gra_steel <- sapply(gra_steel, selection) * 8 * gravity$h * n_gcol  # assuming 8 steel bars per column
  
gra_tie = (gravity$d * gravity$b * gravity$sh_ratio)/n_col_tie
gra_tie <- sapply(gra_tie, selection) * (gravity$d * 6 + gravity$b * 2) /12 * gravity$h * 12 / gravity$sh_spacing * n_gcol
  
gra_total = sum(gra_concrete) + sum(gra_steel) + sum(gra_tie)


# Perimeter columns
col_concrete = (column$in_d * column$in_b / 144) * column$h * c1 * (n_fcol - 4) +  # 8000 psi concrete
  (column$ex_d * column$ex_b / 144) * column$h * c1 * 4
  
col_in_steel = (column$in_d * column$in_b * column$in_ratio)/n_col
col_in_steel <- sapply(col_in_steel, selection) * n_col * column$h * (n_fcol - 4)

col_ex_steel = (column$ex_d * column$ex_b * column$ex_ratio)/n_col
col_ex_steel <- sapply(col_ex_steel, selection) * n_col * column$h * 4

col_in_tie = (column$in_d * column$in_b * column$insh_ratio)/n_col_tie
col_in_tie <- sapply(col_in_tie, selection) * (4 + 2/3) * (column$in_d + column$in_b) /12 * column$h * 12 / column$insh_spacing * (n_fcol - 4)

col_ex_tie = (column$ex_d * column$ex_b * column$exsh_ratio)/n_col_tie
col_ex_tie <- sapply(col_ex_tie, selection) * (4 + 2/3) * (column$ex_d + column$ex_b) /12 * column$h * 12 / column$exsh_spacing * 4

col_total = sum(col_concrete) + sum(col_in_steel) + sum(col_ex_steel) + sum(col_in_tie) + sum(col_ex_tie)


# Beams
bm_s <- c(bm_s3, bm_s4, bm_s5, bm_s6, bm_s7, bm_s8, bm_s9, bm_s10, bm_s11, bm_s14, bm_s18)

selection2 <- function(x){
  bar_size <- c(0.11, 0.2, 0.31, 0.44, 0.6, 0.79, 1, 1.27, 1.56, 2.25, 4)
  temp <- bar_size - x
  upper <- min(bar_size[temp >= 0])
  lower <- max(bar_size[temp < 0])
  bm_s[match(upper,bar_size)]
}

bm_concrete = (beam$d * beam$b / 144) * beam$l * c2   # 5000 psi concrete

bm_top_steel = (beam$d * beam$b * beam$top_ratio) / n_bm_top
bm_top_steel <- sapply(bm_top_steel, selection2) * n_bm_top * beam$l 
  
bm_btm_steel = (beam$d * beam$b * beam$btm_ratio) / n_bm_btm
bm_btm_steel <- sapply(bm_btm_steel, selection2) * n_bm_btm * beam$l 
  
bm_stirrup = (beam$d * beam$b * beam$sh_ratio) / n_bm_stirrup
bm_stirrup <- sapply(bm_stirrup, selection2) * (beam$d * 6 + beam$b * 2) /12 * beam$l * 12 / beam$sh_spacing

bm_total = sum(bm_concrete) + sum(bm_top_steel) + sum(bm_btm_steel) + sum(bm_stirrup)


# Slabs
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

p = total / total_cost  # percentage

unit_cost = total_cost / (area * n_story)
