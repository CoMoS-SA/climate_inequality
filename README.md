Code and supplementary documentation for Palagi et al (submitted to PNAS, 2022).

This folder contains data and replication codes for figures and tables contained both in the main paper and in the Supplementary Information.

All codes are included. However, lines referring to saving of files are commented, as, for ease of replication, final data files are also provided and used in figures.R and figures_SI.R. 

- The folder "final_charts" contains the code to replicate all figures in the main paper, as well as SI Appendix figures for projections. 

- The code figures_SI.R replicates all figures in SI Appendix that are related to the empirical part of the paper. 

- The code clean_generate_data.R generates our main data set by combining economic and climate variables. 

- The code main_reg_and_margins.do replicates main regressions for Table 1 and SI Appendix, Tables S3-S5. It also employs Stata margins command to generate estimated response functions used for plotting parabolas. 

- The code robustness_SI.R contains several robustness checks presented in SI Appendix, Robustness. 

- The code proj_baseline.R replicates baseline projections used for Figures 2, 3 and 4. The code ssp_impacts_generator.R replicates projections for scenarios with different RCP-SSP combinations (SI Appendix). 

- The code econometric_uncertainty_with_boundaries.R replicates econometric uncertainty tests used for Figure 3. 

These files are free software: you can redistribute them and/or modify them under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

A copy of the GNU General Public License is available at http://www.gnu.org/licenses/.
