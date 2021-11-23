## Replacement cost data (FEMA P-58 median)
## Assembly costs !!!

ns <- data.frame(
  "comp" = c("B1031.001","B1031.011a","B1031.011b","B1031.021a","B1031.021b", # Shear tab connections
             "B1033.213a","B1033.213b", # BRB
             "B1035.031", # Welded steel moment connections
             "B1041.001a","B1041.001b","B1041.002a","B1041.002b","B1041.003a","B1041.003b","B1049.011","B1049.012",
             "B2011.201a","B2011.201b","B2022.002", # Exterior enclosure
             "C1011.001b", # Wall partition
             "C2011.001b","C2011.011b", # Stairs
             "C3011.001b","C3027.002", # Interior finishes
             "C3032.003a","C3032.003b","C3032.003c","C3032.003d", # Ceiling
             "C3034.002", # Light fixtures
             "D1014.041","D1014.042","D1014.043","D1014.044", # Elevator
             "D2021.013a","D2021.013b","D2021.023a","D2021.023b","D2031.013b", # Water piping + bracing
             "D3031.013c","D3031.013f","D3031.013i", # Chiller
             "D3031.023f","D3031.023i", # Cooling tower + anchor
             "D3041.002c","D3041.011c","D3041.012c","D3041.032c", # HVAC ducting, diffuser + anchor
             "D3041.041b", # Variable air volume (VAV)
             "D3041.103c", # HVAC fan + anchor
             "D3052.013I","D3052.013l", # Air handling unit (AHU)
             "D3067.012c", # Control panel + anchor
             "D4011.023a", "D4011.024a",  # Fire sprinkler piping
             "D4011.053a", "D4011.054a",  # Fire sprinkler head
             "D5011.013c", # Transformer
             "D5012.013d", # Motor control center + anchor 
             "D5012.023c", # Low voltage switchgear + anchor
             "D5012.033c"), # Distribution panel + anchor
  "cost" = c(0,0,0,0,0, # Shear tab connections
             0,0, # BRB
             0, # Welded steel moment connections
             0,0,0,0,0,0,0,0,
             50700,50700, # Exterior enclosure, wall 30' long x13' high, clading panel + window + gypsum board ($/EA)
             750, # Exterior enclosure, curtain walls, 30 sf ($/EA)
             13000, # Wall partition, 100' long x 13' high, metal stud + gypsum board ($/EA)
             15200,18200, # Stairs ($/EA)
             2700, # Interior wall finish, 100' long x 9' high, gypsum + wallpaper ($/EA)
             650, # Interior floor finish, 1000 sf ($/EA)
             2687.5, 6256.5, 18382.5, 24187.5, # Ceiling, 250 sf, 600 sf, 1800 sf, 2500 sf ($/EA)
             250, # Light fixtures ($/EA)
             4400,18500,11400,7300, # Elevator machine room, cabin and counterweight, guide rail, guide rail brackets ($/EA)
             1800,180, # Water piping, diameter < 2.5", 20' section, 1000' long ($/EA)
             2500,300, # Water piping, diameter > 2.5", 20' section, 1000' long ($/EA)
             200, # Sanitary waste piping support ($/EA)
             45500,150800,251200, # Chiller, <100 ton, 100-350 ton, 350-750 ton ($/EA)
             75800,126200, # Cooling tower, 100-350, 350-750 ton ($/EA)
             2500,6150,3000,200, # HVAC ducting, diffuser ($/EA)
             1200, # Variable air volume ($/EA)
             3000, # HVAC fan ($/EA)
             127400,183200, # Air handling unit, 30000 UFM ($/EA)
             4300, # HVAC control panel ($/EA)
             1800,1800, # Fire sprinkler piping, 20' section, 1000' long ($/EA)
             400,400, # Fire sprinkler head ($/EA)
             6375, # Transformer, 75 kVA ($/EA)
             4300, # Motor control center, contorl panel ($/EA)
             8175, # Low voltage switchgear, 225 Amp ($/EA)
             8175) # Distribution panel, 225 Amp ($/EA)
    )



