# Precision Policy Optimisation and Resource Allocation
This repository is for a new framework for optimising policy interventions using Generalised Random Forest (GRF) and Policy Tree methodologies. As we navigate current budgetary constraints and to address the need for more efficient resource allocation. These tools can be utilised to transition from reactive implementation to predictive "Active-Lift" strategies.
## Why This Matters
Traditional policy evaluation often relies on the "Average Treatment Effect" (ATE), which assumes a one-size-fits-all outcome. This approach frequently leads to inefficient spending by providing support to non-responsive applicants whilst underserving those with the highest potential for impact. By contrast, these new models explicitly account for latent propensity scores and selection bias, allowing us to:
* Maximise Social Return on Investment (ROI): Target interventions specifically towards individuals or units where the predicted "uplift" (the net benefit of treatment) is highest.
* Optimise Within Constraints: Incorporate strict budget capacities (e.g., limiting treatment to a specific percentage of the population) directly into the optimisation target.
*Ensure Fiscal Discipline: Quantify and minimise "fiscal risk exposure" by identifying non-responsive populations.
## Workshop Context
This work draws from training materials developed for a workshop for [NUST Institute of Policy Studies (NIPS), Pakistan](https://nips.nust.edu.pk/) which I delivered on 12/06/2026. The framework is designed to provide decision-level guidance for resource-constrained environments, including (for example):
* **NSS Score Improvement**: You can use these methods to optimise National Student Survey scores by effectively targeting a fraction of students for specific interventions.
* **Internal Budget Allocation**: As departmental heads or leads, you can apply these techniques to provide financial incentives to a specific portion of your research group, ensuring the most impactful allocation of limited funds.
## Policy Domains Tested
I have validated this pipeline across ten distinct, simulated policy scenarios to ensure architectural robustness. These scenarios are detailed in the "causal_forest_simulations.pdf" file and include:
* **After-School Mathematics Tutoring**: Targeting students based on socioeconomic status and prior test scores.
* **Remote Patient Monitoring (RPM)**: Managing chronic disease readmissions by prioritising high-comorbidity patients.
* **Residential Energy Efficiency Retrofit**: Prioritising older residential housing stock for grants.
* **Small Business Digital Transformation**: Subsidising consulting and software adoption to spur local economic growth.
* **Smart Irrigation Subsidy**: Optimising water usage for large farms in drought-prone areas.
* **Academic Staff Incentives**: Evaluating the causal return on seed capital based on career stage and discipline.
* **General Training Incentives**: A synthetic template used for core system validation.
* **Urban Public Transit Subsidy**: Reducing carbon footprints by subsidising annual passes for commuters.
* **Urban Tree Canopy Maintenance**: Combating urban heat island effects via targeted zone maintenance.
* **Wastewater Infrastructure and Smart Grids**: Prioritising high-risk sewer catchments to prevent toxic spills.

All the accompanying R codes test these simulations to see which covariates have the most profound effect on policy implementation. You can open the R scripts in RStudio and click on "source" button to see the results for all these scenarios. These methods provide a scalable roadmap for evidence-based governance, ensuring that we maximise social welfare whilst maintaining rigorous fiscal discipline. 
 
