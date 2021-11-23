# Last update 11/22/2021

setwd("/Users/ynz23/Documents/R\ scripts")
source("Nonstructural_data.R")

#################################### Inputs #####################################
n_story = 12           # Number of stories
l1 = 120               # Length in one direction (ft)
l2 = 120               # Length in another direction (ft)

# file = "building_component_data_B1-4"
# file = "building_component_data_B14-4"
# file = "building_component_data_B15-4"
# file = "building_component_data_B16-4"

# file = "building_component_data_B1-8"
# file = "building_component_data_B14-8"
# file = "building_component_data_B15-8"
# file = "building_component_data_B16-8"

# file = "building_component_data_B1-12"
# file = "building_component_data_B14-12"
# file = "building_component_data_B15-12"
# file = "building_component_data_B16-12"

# file = "BRBF_comp-4"
file = "BRBF_comp-12"

## NS designs
#1 Baseline
#2 Stairs, C2011.001b
delta_2 = 1 + 0.135
#3 Ceiling, C3032.003a, C3032.003b, C3032.003c, C3032.003d
delta_3 = 1
#4 Guide-rail, D1014.043

#5 Elev-machine, D1014.041
delta_5 = 1 + 0.15
#6 Ducting, D3041.011c
delta_6 = 1 + 0.15
#7 Pipe-bracing, D2021.013b, D2021.023b
delta_7 = 1 + 0.175
#8 VAV-box, D3041.041b
delta_8 = 1 + 0.15
#9 HVAC-fan, D3041.103c
delta_9 = 1 + 0.15
#10 AHU, D3052.013l
delta_10 = 1 + 0.15
#11 E-dist-panel, D5012.033c
delta_11 = 1 + 0.15
#12 E-control, D5012.013d
delta_12 = 1 + 0.15
#13 Switch-gear, D5012.023c
delta_13 = 1 + 0.15
#14 Transformer, D5011.013c
delta_14 = 1 + 0.15

################################### Calculation #################################
raw <- read.csv(file=paste("/Users/ynz23/Documents/Design/BRBF/",
                                 file,".csv", sep = ""))
component <- raw[complete.cases(raw),] # Remove rows with NAs

ID <- function(x){
  a <- grep(x, ns$comp, ignore.case = T)
  if (length(a) > 0) {
    ns$cost[a]
  } else {
   print(paste0("Not found. Component id: ", x)) 
  }
}

assembly <- sapply(component$comp_id,ID)


#2 Stairs, C2011.011b, C2011.001b
if ('C2011.011b' %in% names(assembly)){
  i = grep("C2011.011b", names(assembly), ignore.case = T)
  assembly[i] <- assembly[i] * delta_2
}
if ('C2011.001b' %in% names(assembly)){
  i = grep("C2011.001b", names(assembly), ignore.case = T)
  assembly[i] <- assembly[i] * delta_2
}

#3 Ceiling, C3032.003a, C3032.003b, C3032.003c, C3032.003d
if ('C3032.003a' %in% names(assembly)){
  i = grep("C3032.003a", names(assembly), ignore.case = T)
  assembly[i] <- assembly[i] * delta_3
}
if ('C3032.003b' %in% names(assembly)){
  j = grep("C3032.003b", names(assembly), ignore.case = T)
  assembly[j] <- assembly[j] * delta_3
}
if ('C3032.003c' %in% names(assembly)){
  k = grep("C3032.003c", names(assembly), ignore.case = T)
  assembly[k] <- assembly[k] * delta_3
}
if ('C3032.003d' %in% names(assembly)){
  l = grep("C3032.003d", names(assembly), ignore.case = T)
  assembly[l] <- assembly[l] * delta_3
}

#5 Elev-machine, D1014.041
if ('D1014.041' %in% names(assembly)){
  i = grep("D1014.041", names(assembly), ignore.case = T)
  assembly[i] <- assembly[i] * delta_5
}

#6 Ducting, D3041.011c
if ('D3041.011c' %in% names(assembly)){
  i = grep("D3041.011c", names(assembly), ignore.case = T)
  assembly[i] <- assembly[i] * delta_6
}

#7 Pipe-bracing, D2021.013b, D2021.023b
if ('D2021.013b' %in% names(assembly)){
  i = grep("D2021.013b", names(assembly), ignore.case = T)
  assembly[i] <- assembly[i] * delta_7
}
if ('D2021.023b' %in% names(assembly)){
j = grep("D2021.023b", names(assembly), ignore.case = T)
assembly[j] <- assembly[j] * delta_7
}

#8 VAV-box, D3041.041b
if ('D3041.041b' %in% names(assembly)){
  i = grep("D3041.041b", names(assembly), ignore.case = T)
  assembly[i] <- assembly[i] * delta_8
}

#9 HVAC-fan, D3041.103c
if ('D3041.103c' %in% names(assembly)){
  i = grep("D3041.103c", names(assembly), ignore.case = T)
  assembly[i] <- assembly[i] * delta_9
}

#10 AHU, D3052.013l
if ('D3052.013l' %in% names(assembly)){
  i = grep("D3052.013l", names(assembly), ignore.case = T)
  assembly[i] <- assembly[i] * delta_10
}

#11 E-dist-panel, D5012.033c
if ('D5012.033c' %in% names(assembly)){
  i = grep("D5012.033c", names(assembly), ignore.case = T)
  assembly[i] <- assembly[i] * delta_11
}

#12 E-control, D5012.013d
if ('D5012.013d' %in% names(assembly)){
  i = grep("D5012.013d", names(assembly), ignore.case = T)
  assembly[i] <- assembly[i] * delta_12
}

#13 Switch-gear, D5012.023c
if ('D5012.023c' %in% names(assembly)){
  i = grep("D5012.023c", names(assembly), ignore.case = T)
  assembly[i] <- assembly[i] * delta_13
}

#14 Transformer, D5011.013c
if ('D5011.013c' %in% names(assembly)){
  i = grep("D5011.013c", names(assembly), ignore.case = T)
  assembly[i] <- assembly[i] * delta_14
}

mat = data.matrix(component)

####################### RCMF ###################################################
# cost = assembly * rowSums(mat[,6:(6+n_story-1)])
# data <- data.frame(keyName=names(assembly), value=assembly, row.names=NULL)
# data$quantity <- rowSums(mat[,6:(6+n_story-1)])
# data$baseline <- unname(cost)

####################### BRBF ###################################################
cost = assembly * mat[,2]
data <- data.frame(keyName=names(assembly), value=assembly, row.names=NULL)
data$quantity <- mat[,2]
data$baseline <- unname(cost)

## FEMA P-58 fragility groups
group = data.frame(
  "code" = c("A10","A20","B10","B20","B30","B40","C10","C20","C30","D10","D20",
             "D30","D40","D50","E10","E20","F10"),
  "name" = c("Foundations","Basement","Super structure","Exterior enclosure",
             "Roof elements","Exterior finishes","Interior construction","Stairs",
             "Interior finishes","Conveying","Plumbing","HVAC","Fire protection",
             "Electrical","Equipment","Furnishings","Special construction")
)

classify <- function(x){
  a <- substr(x, start = 1, stop = 2)
  b <- grep(a, group$code, ignore.case =T)
  group$name[b]
}

category <- sapply(component$comp_id,classify)


################################# Outputs ######################################
df <- data.frame(category,cost)
aggregate(cost~category,df,sum)

non <- df[!(df$category == "Super structure"),]

total_cost = sum(non$cost)
unit_cost = total_cost / n_story / (l1 * l2)



