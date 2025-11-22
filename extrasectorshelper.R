# Create the data frame
battery_df <- data.frame(
  sector = c(
    "Raw Materials", "Raw Materials", "Active Materials", "Active Materials",
    "Components", "Components", "Cell Manufacturing", "Systems & Integration",
    "Applications", "End-of-Life"
  ),
  technology = c(
    "Lithium Extraction & Processing", "Graphite & Carbon Materials",
    "Cathode Materials", "Anode Materials",
    "Electrolytes & Additives", "Separators",
    "Battery Cell Design & Assembly", "Battery Management Systems (BMS)",
    "Electric Vehicles & Mobility", "Battery Recycling & Recovery"
  ),
  brief_description = c(
    "Extraction, purification, and processing of lithium and other battery metals (Ni, Co, Mn).",
    "Production of graphite and carbon materials used for anodes.",
    "Production and improvement of cathode active materials such as LiCoO2, LiFePO4, NMC, etc.",
    "Development of anode materials including graphite, silicon, and lithium metal.",
    "Formulation and improvement of electrolytes (liquid, solid-state, polymer) and their additives.",
    "Development of membranes or separators between electrodes in batteries.",
    "Methods for assembling electrodes, cell structure, and sealing of batteries.",
    "Monitoring and control systems for voltage, temperature, and state-of-charge.",
    "Integration of batteries into electric vehicles, hybrid systems, and charging infrastructure.",
    "Processes for recovering metals and components from spent batteries."
  ),
  CPC = c(
    "C22B 3/00; C22B 7/00; C22B 26/00",
    "C01B 31/02; C01B 32/00",
    "H01M 4/505; H01M 4/5055",
    "H01M 4/131; H01M 4/139",
    "H01M 10/056; H01M 10/058",
    "H01M 10/12; H01M 10/14",
    "H01M 2/00; H01M 6/00",
    "G01R 31/36; H02J 7/00",
    "B60L 11/18; Y02T 10/70",
    "C22B 7/00; H01M 10/54"
  ),
  stringsAsFactors = FALSE
)

# View the data frame
