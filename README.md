# Licor_serie_Calibration
Calibration for Licor Li-7820 and Li-7810 connected in series to measure discrete samples

# What you need

1. Two files (one from Li-7820 and another one from Li-7810) with injections done for calibration. Usually, injections with different concentration and injecting distinct volume, from 0.2 to 1 mL.
2. Download the code in the repo: 

# How to proceed
1. Run the code from repo in order to obtain integration peaks (more info in the repo). Just Map_injections.R and Raw_to_peaks_Licor.R. Remember to modified "raw"" maps injections and saved as "corrected""
2. Then, you can run this script: `Calibration-enserie.R`
3. The script will return you: one file with calibration curve, which can be use directly in repo and two files with N<sup>2</sup>2 and CO<sup>2</sup> and CH<sup>4</sup> calibration curve plots.