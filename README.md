# Licor_serie_Calibration
Calibration for Licor Li-7820 and Li-7810 connected in series to measure discrete samples

# What you need

1. Two files (one from the Li-7820 and another from the Li-7810) containing injections performed for calibration. Typically, these injections involve different concentrations and injection volumes ranging from 0.2 to 1 mL.
2. Download the code from this repository: [Measuring_discrete_GHG_samples_with_Licor
](https://github.com/JorgeMonPe/Measuring_discrete_GHG_samples_with_Licor.git)

# How to proceed
1. Run the scripts from the repository to obtain integration peaks (more details available in [the repository
](https://github.com/JorgeMonPe/Measuring_discrete_GHG_samples_with_Licor.git)). Specifically, use `Map_injections.R` and `Raw_to_peaks_Licor.R`.

  . Remember to modify the "raw" mapped injections and save them as "corrected".
  
2. Next, run the script `Calibration-enserie.R`
3. The script will generate:
  . A file containing the calibration curve, which can be used directly in the repository
  . Two files with calibration curve plots for N<sup>2</sup>2 and CO<sup>2</sup> and CH<sup>4</sup>.