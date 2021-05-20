# BlueCarbonErosionAndDecomp

Refining Estimates of Greenhouse Gas Emissions From Salt Marsh “Blue Carbon” Erosion and Decomposition
https://doi.org/10.3389/fmars.2021.661442

Run order for the scripts
1.	DecompExpt.R – This script deals with the data from our incubation decomposition experiment. The raw gas concentration data from the Greenhouse Gas Analyzer (GGA) is read in, wrangled, converted from concentration to moles, and production rates are calculated. 
2.	Q10.R – This script calculates the temperature sensitivity of the cored sediment. The summarized data created in DecompExpt.R is used to calculate both ‘traditional’ Q10 and Q10-q. 
3.	Local Water Temperature.R -- this script reads in raw temperature data from a YSI near the coring location and calculates the decomposition that would occur if sediment were incubated at those temperatures for a year
4.	CalculateNationWideDecomposition.R – reads in the CONUS temp data from NOAA CO-OPS temperature stations from file and calculates the decomposition at each station. Takes estimates for shoreline length and annual erosion and combines them with our estimates for annual decomposition to estimate annual decomposition from erosion in CONUS

Plotting Scripts – these scripts plot the data. Not necessary to generate the results for the manuscript
1.	CONUS plot.R – (FIGURE 5) This script creates a map plot of the results from "Calculate Nationwide Decomposition.R" 
2.	Cumulative C Respired Plot.R – (FIGURE 2) This script plots the data from the incubation experiments

Abstract: 
Coastal wetlands have sediments that contain organic matter preserved against decomposition for timespans that can range up to millennia. This “blue carbon” in wetland sediments has been proposed as a sink for atmospheric carbon dioxide and a potential source of greenhouse gases if coastal habitats are lost. A missing gap in the role of coastal habitats in the global carbon cycle is elucidating the fate of wetland sediment carbon following disturbance events, such as erosion, that can liberate organic matter to an oxygenated environment where decomposition can more readily occur. Here, we track the fate of previously stored salt marsh sediment by measuring the production of carbon dioxide (CO2) and methane (CH4) during an oxygenated incubation. Sediments from two depth horizons (5–10 cm and 20–25 cm) were incubated at two temperatures (20 and 30°C) for 161 days. Q10 of the decomposition process over the entire course of the experiment was 2.0 ± 0.1 and 2.2 ± 0.2 for shallow and deep horizons, respectively. Activation energy for the decomposition reaction (49.7 kJ ⋅ mol–1 and 58.8 kJ ⋅ mol–1 for shallow and deep sediment horizons, respectively) was used to calculate temperature-specific decomposition rates that could be applied to environmental data. Using high-frequency water temperature data, this strategy was applied to coastal states in the conterminous United States (CONUS) where we estimated annual in situ decomposition of eroded salt marsh organic matter as 7–24% loss per year. We estimate 62.90 ± 2.81 Gg C ⋅ yr–1 is emitted from eroded salt marsh sediment decomposition in the CONUS.
