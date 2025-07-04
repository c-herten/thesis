## **Causal Inference Approach of Educational Policy Effects**

This repository implements causal inference methods for analyzing education policy impacts, focusing on early school leaving rates, completion rates and unemployment rates. The analysis employs Difference-in-Differences (DiD), Synthetic Control Methods (SCM) and Synthetic Difference-in-Differences (SDiD) to evaluate the policy Portugal introduced in 2009, which made upper secondary schooling mandatory.

### **Installation**

The analysis requires R version 4.3.2 or higher. The main packages used in the thesis are `fixest`, `Synth`, `synthdid` and `xsynthdid`.


### **File Structure**

- **`install-packages.R`**: Managing package dependencies 
- **`data-processing.R`**: Cleaning, preparing and merging data  
- **`methodology.R`**: Application of DiD, SCM and SDiD methods
- **`create-plots.R`**: Generating visualizations and tables
- **`data/`**: Datasets, including a ready-to-use dataset, called `final_data.csv`
- **`plots/`**: Generated figures and tables


### **Replication**

The analysis can be executed by running the scripts in the following sequence:

```r

# 1. Process data 
source("data-processing.R")

# This step can theoretically be skipped and the dataset, called `final_data.csv`, stored in the `data/` folder can be used right away for analysis.

# 2. Run analysis
source("methodology.R")

# 3. Generate visualizations and tables
source("create-plots.R")
```
### **References**

[1] Arkhangelsky, D. (2019). synthdid: Synthetic Difference-in-Difference Estimation. https://github.com/synth-inference/synthdid

[2] Bergé, L. (2018). Efficient estimation of maximum likelihood models with multiple fixed-effects: The R package FENmlm. *CREA Discussion Papers*, (13).

[3] Hainmueller, J., Diamond, A., & Abadie, A. (2011). Synth: An R Package for Synthetic Control Methods in Comparative Case Studies. *Journal of Statistical Software, 42*(13), 1-17. https://www.jstatsoft.org/v42/i13/

[4] Kranz, S. (2021). xsynthdid: Simple function to adjust for covariates in synthdid. https://github.com/skranz/xsynthdid


