## **Causal Inference Approach of Educational Policy Effects**

This repository implements causal inference methods for analyzing education policy impacts, focusing on early school leaving rates, completion rates and unemployment rates. The analysis employs Difference-in-Differences (DiD), Synthetic Control Methods (SCM) and Synthetic Difference-in-Differences (SDiD) to evaluate the policy Portugal introduced in 2009, which made upper secondary schooling mandatory. Specifically, it applies DiD and SDiD to the aforementioned educational indicators and SCM to unemployment rates. 

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

# 2. Run analysis
source("methodology.R")

# 3. Generate visualizations and tables
source("create-plots.R")
```

The first step can theoretically be skipped and the dataset, called `final_data.csv`, stored in the `data/` folder can be used right away for analysis.



