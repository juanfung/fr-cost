## Repair costs for nonstructural components: Interior construction, Interior finishes,
## Conveying, Plumbing, HVAC, Fire protection

#################################### Inputs #####################################
n_story = 4           # Number of stories
l1 = 120               # Length in one direction (ft)
l2 = 120               # Length in another direction (ft)

file = "baseline_4story"
# file = "4story_ie125"
# file = "4story_ie150"
# file = "4story_drift10"
# file = "4story_drift15"
# file = "4story_rc4"

# file = "baseline_12story"
# file = "12story_ie125"
# file = "12story_ie150"
# file = "12story_drift10"
# file = "12story_drift15"
# file = "12story_rc4"

# file = "baseline_20story"
# file = "20story_ie125"
# file = "20story_ie150"
# file = "20story_drift10"
# file = "20story_drift15"
# file = "20story_rc4"


################################### Calculation #################################
component <- read.csv(file=paste("/Users/ynz23/Downloads/Archetype\ Model\ Outputs/",
                                 file,"/building_component_data.csv", sep = ""))
mat = data.matrix(component)
cost = mat[,4] * rowSums(mat[,5:(5+n_story-1)])


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

nonstr <- df[!(df$category == "Exterior enclosure" | df$category == "Stairs" |
            df$category == "Super structure"),]

total_cost = sum(nonstr$cost)
unit_cost = total_cost / n_story / (l1 * l2)
