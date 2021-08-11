
# Repair costs include:
# Demolition
## Floor finishes
## Partitions obstructing works (full height)
# 
# Remove, store and reinstall	
## Ceilings 
## Mechanical and electrical systems
## Office furniture and equipment
# 
# Temporary	
## Floor protection (access)
## Dust curtains
## Scaffolding or work platforms (underside access)
## Shoring
# 
# Column/beam repairs	
## Prepare work area
## Remove loose concrete, clean rebars
## Epoxy injection
## Formwork (including removal)
## Concrete (replacement costs for concrete)
## Remove damaged reinf and splice in new (replacement costs for steel)
# 
# Replace	
## Partitions removed - patch in
## Floor finishes
# 
# Mechanical and electrical modifications or relocation	
# as required for repair work

################################# Inputs ########################################

source("/Users/ynz23/Desktop/RC_repair_data.R")
raw <- read.csv(file = '/Users/ynz23/Desktop/test.csv')

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
span = 20              # bay spacing (ft)

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
gravity <- data.frame("story"=n_story:1,
                      "d"=(rep(c(d_gravity),times=c(n_story))),
                      "b"=(rep(c(b_gravity),times=c(n_story))),
                      "h"=(rep(c(13,15),times=c(n_story-1,1))),
                      "ratio"=(rep(c(0.025),times=c(n_story))),
                      "sh_ratio"=as.numeric(Col_in_sh_ratio),
                      "sh_spacing"=as.numeric(Col_in_sh_spacing)
)

# Perimeter column design parameters
column <- data.frame("story"=n_story:1,
                     "in_d"=as.numeric(Col_in_d),
                     "in_b"=as.numeric(Col_in_b),
                     "ex_d"=as.numeric(Col_ex_d),
                     "ex_b"=as.numeric(Col_ex_b),
                     "h"=(rep(c(13,15),times=c(n_story-1,1))),
                     "in_ratio"=as.numeric(Col_in_ratio),
                     "ex_ratio"=as.numeric(Col_ex_ratio),
                     "insh_ratio"=as.numeric(Col_in_sh_ratio),
                     "insh_spacing"=as.numeric(Col_in_sh_spacing),
                     "exsh_ratio"=as.numeric(Col_ex_sh_ratio),
                     "exsh_spacing"=as.numeric(Col_ex_sh_spacing)
)

# Perimeter beam design parameters
beam <- data.frame("story"=n_story:1,
                   "d"=as.numeric(Bm_d),
                   "b"=as.numeric(Bm_b),
                   "l"=(rep(c(l_bm),times=c(n_story))),
                   "top_ratio"=as.numeric(Bm_top_ratio),
                   "btm_ratio"=as.numeric(Bm_btm_ratio),
                   "sh_ratio"=as.numeric(Bm_sh_ratio),
                   "sh_spacing"=as.numeric(Bm_sh_spacing)
)

# Slab design parameters
slab <- data.frame("story"=n_story:1,
                   "d"=(rep(c(d_slab),times=c(n_story))),
                   "b"=(rep(c(l_slab),times=c(n_story))),
                   "l"=(rep(c(l_slab),times=c(n_story)))
)

################################ Calculation #####################################
# Gravity columns
col_s <- c(col_s3, col_s4, col_s5, col_s6, col_s7, col_s8, col_s9, col_s10, col_s11, col_s14, col_s18)

## select steel bars for columns
selection <- function(x){
  bar_size <- c(0.11, 0.2, 0.31, 0.44, 0.6, 0.79, 1, 1.27, 1.56, 2.25, 4)
  temp <- bar_size - x
  upper <- min(bar_size[temp >= 0])
  lower <- max(bar_size[temp < 0])
  col_s[match(upper,bar_size)]
}

gra_concrete = (gravity$d * gravity$b / 144) * gravity$h * c1   # 8000 psi concrete

gra_steel = (gravity$d * gravity$b * gravity$ratio)/8  # assuming 8 steel bars per column
gra_steel <- sapply(gra_steel, selection) * 8 * gravity$h  # assuming 8 steel bars per column

gra_tie = (gravity$d * gravity$b * gravity$sh_ratio)/n_col_tie
gra_tie <- sapply(gra_tie, selection) * (gravity$d * 6 + gravity$b * 2) /12 * gravity$h * 12 / gravity$sh_spacing

gra_total = gra_concrete + gra_steel + gra_tie  # ($/each)

gra_repair = list()

for (i in 1:3) {
  gra_repair[[i]] = gra_total + DS_adj[i]
  gra_repair[[i+3]] = gra_concrete + DS_adj[i+3]
  gra_repair[[i+6]] = DS_adj[i+6]
}

gra_repair_cost = do.call(rbind, gra_repair) # Rows represent damage states DS3:DS1; columns represent story number n_story:1

# Perimeter columns
col_in_concrete = (column$in_d * column$in_b / 144) * column$h * c1  # 8000 psi concrete
col_ex_concrete = (column$ex_d * column$ex_b / 144) * column$h * c1  # 8000 psi concrete

col_in_steel = (column$in_d * column$in_b * column$in_ratio)/n_col
col_in_steel <- sapply(col_in_steel, selection) * n_col * column$h

col_ex_steel = (column$ex_d * column$ex_b * column$ex_ratio)/n_col
col_ex_steel <- sapply(col_ex_steel, selection) * n_col * column$h

col_in_tie = (column$in_d * column$in_b * column$insh_ratio)/n_col_tie
col_in_tie <- sapply(col_in_tie, selection) * (4 + 2/3) * (column$in_d + column$in_b) /12 * column$h * 12 / column$insh_spacing

col_ex_tie = (column$ex_d * column$ex_b * column$exsh_ratio)/n_col_tie
col_ex_tie <- sapply(col_ex_tie, selection) * (4 + 2/3) * (column$ex_d + column$ex_b) /12 * column$h * 12 / column$exsh_spacing

col_in_total = col_in_concrete + col_in_steel + col_in_tie  # ($/each)
col_ex_total = col_ex_concrete + col_ex_steel + col_ex_tie  # ($/each)

col_repair = list()

for (i in 1:3) {
  col_repair[[i]] = col_ex_total + DS_adj[i]
  col_repair[[i+9]] = col_in_total + DS_adj[i]
  col_repair[[i+3]] = col_ex_concrete + DS_adj[i+3]
  col_repair[[i+12]] = col_in_concrete + DS_adj[i+3]
  col_repair[[i+6]] = DS_adj[i+6]
  col_repair[[i+15]] = DS_adj[i+6]
}

col_repair_cost = do.call(rbind, col_repair) # Rows 1-9 are for exterior columns; rows 10-18 are for interior columns

# Beams
bm_s <- c(bm_s3, bm_s4, bm_s5, bm_s6, bm_s7, bm_s8, bm_s9, bm_s10, bm_s11, bm_s14, bm_s18)

## select steel bars for beams
selection2 <- function(x){
  bar_size <- c(0.11, 0.2, 0.31, 0.44, 0.6, 0.79, 1, 1.27, 1.56, 2.25, 4)
  temp <- bar_size - x
  upper <- min(bar_size[temp >= 0])
  lower <- max(bar_size[temp < 0])
  bm_s[match(upper,bar_size)]
}

bm_concrete = (beam$d * beam$b / 144) * span * c2   # 5000 psi concrete

bm_top_steel = (beam$d * beam$b * beam$top_ratio) / n_bm_top
bm_top_steel <- sapply(bm_top_steel, selection2) * n_bm_top * span

bm_btm_steel = (beam$d * beam$b * beam$btm_ratio) / n_bm_btm
bm_btm_steel <- sapply(bm_btm_steel, selection2) * n_bm_btm * span 

bm_stirrup = (beam$d * beam$b * beam$sh_ratio) / n_bm_stirrup
bm_stirrup <- sapply(bm_stirrup, selection2) * (beam$d * 6 + beam$b * 2) /12 * span * 12 / beam$sh_spacing

bm_total = bm_concrete + bm_top_steel + bm_btm_steel + bm_stirrup  # ($/each)

bm_repair = list()

for (i in 1:3) {
  bm_repair[[i]] = bm_total + DS_adj[i]
  bm_repair[[i+3]] = bm_concrete + DS_adj[i+3]
  bm_repair[[i+6]] = DS_adj[i+6]
}

bm_repair_cost = do.call(rbind, bm_repair)  # Rows represent damage states DS3:DS1; columns represent story number n_story:1

################################### Outputs #####################################
gra_repair_cost
col_repair_cost
bm_repair_cost
