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


# Defence Technology classification.
# CPC scope follows the F41/F42 weapons-and-ammunition branch plus the
# cross-cutting subclasses most commonly used in the patent-economics
# literature on military / defence technology — see Acosta, Coronado, Marin
# & Prats (2013) "Factors affecting the diffusion of patented military
# technology in the field of weapons and ammunition", Scientometrics 92(3),
# and Acosta, Coronado & Marin (2017) on dual-use military patents.
# https://link.springer.com/article/10.1007/s11192-012-0857-8
# https://link.springer.com/article/10.1007/s11192-017-2443-6
defence_df <- data.frame(
  sector = c(
    "Weapons", "Weapons", "Weapons", "Weapons", "Weapons",
    "Armour", "Training",
    "Ammunition & Explosives", "Ammunition & Explosives", "Ammunition & Explosives",
    "Naval Warfare", "Military Aviation", "Defence Sensing"
  ),
  technology = c(
    "Small Arms & Ordnance",
    "Non-firearm Weapons",
    "Projectile & Missile Launching",
    "Weapon Sights & Aiming",
    "Missile Propulsion",
    "Armour & Armoured Vehicles",
    "Targets & Training",
    "Ammunition",
    "Fuzes & Ammunition Safety",
    "Blasting & Demolition",
    "Naval Warfare",
    "Military Aviation Equipment",
    "Defence Radar & Sonar"
  ),
  brief_description = c(
    "Small arms common features and ordnance: pistols, rifles, breech mechanisms, mountings (CPC F41A, F41C).",
    "Non-firearm weapons: bows, missile throwers, spring guns, hand-to-hand weapons (CPC F41B).",
    "Apparatus for launching projectiles or missiles: rocket launchers, torpedo and missile-launching systems (CPC F41F).",
    "Weapon sights, fire-control systems, range-finding and aiming arrangements (CPC F41G).",
    "Jet propulsion plants for missiles, rocket motors and related propulsion technology (CPC F02K).",
    "Armour, armoured personnel/military vehicles, tanks, military shielding (CPC F41H).",
    "Targets, target ranges and other military training apparatus (CPC F41J).",
    "Explosive charges, ammunition, mines, grenades and warheads (CPC F42B).",
    "Ammunition fuzes, safety arrangements, igniting devices and detonators (CPC F42C).",
    "Blasting agents, demolition charges and related military-engineering uses (CPC F42D).",
    "Warfare at sea: offensive/defensive arrangements on water-borne vessels, mine-laying, mine sweeping, submarines (CPC B63G).",
    "Arrangement of armaments, armament accessories or military shielding in aircraft (CPC B64D 7/00).",
    "Radar, sonar and lidar systems with explicit military or defence framing (CPC G01S 7/41, G01S 13/52, G01S 15/52)."
  ),
  # Trailing "/" on B64D7/ ensures the prefix match captures the whole main
  # group 7 of B64D (military equipment in aircraft) — i.e. B64D7/00,
  # B64D7/02, B64D7/04, ... — not just the bare B64D7/00 main-group code.
  CPC = c(
    "F41A; F41C",
    "F41B",
    "F41F",
    "F41G",
    "F02K",
    "F41H",
    "F41J",
    "F42B",
    "F42C",
    "F42D",
    "B63G",
    "B64D7/",
    "G01S7/41; G01S13/52; G01S15/52"
  ),
  stringsAsFactors = FALSE
)

