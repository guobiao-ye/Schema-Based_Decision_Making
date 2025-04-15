# Schema-Based Decision Making Simulation

<div align="center">
  
![Version](https://img.shields.io/badge/version-1.0.0-blue.svg)
![R](https://img.shields.io/badge/R-%3E%3D%204.0.0-brightgreen.svg)
![License](https://img.shields.io/badge/license-MIT-green.svg)

</div>

## 📋 Overview

This project simulates schema-based decision-making processes through computational modeling, exploring different adaptive strategies including Win-Stay, Lose-Shift (WSLS), emotion-driven feedback mechanisms, and high-reward selection patterns. The simulation is implemented in R with accompanying analysis and visualization scripts to interpret the results.

## 🎯 Purpose

The primary goal of this project is to investigate the effects of different adaptive strategies on performance in schema-based decision tasks. By simulating these strategies, we can gain deeper insights into how individuals optimize their decision-making processes in complex environments.

## 🧠 Simulation Model

The core of the project is the `simulation` function defined in `model_extend_adaptive_strategies.R`. This function simulates schema-based decision processes over multiple rounds, incorporating various adaptive strategies.

### Key Features

- **Schema Learning & Confidence Updates**: The model updates schema-specific and general confidence based on exploration and feedback.
- **Decision Process**: Uses sequential evidence accumulation mechanisms to select items, with attention shifting influenced by confidence and potential rewards.
- **Adaptive Strategies**:
  - **Win-Stay, Lose-Shift (WSLS)**: Biases attention based on previous outcomes.
  - **Emotion-Driven Feedback**: Adjusts evidence accumulation rates based on prior performance.
  - **High-Reward Selection Pattern**: Activates during low performance, focusing on high-reward items and bypassing schema selection.

## ⚙️ Parameters

The simulation is controlled by parameters in a dataframe (`Param.df`), including learning rates, confidence parameters, decision thresholds, and strategy-specific parameters. For detailed descriptions of each parameter, refer to the code comments in `model_extend_adaptive_strategies.R`.

## 🚀 Running the Simulation

The script `1_simulation_running.R` is used to set up parameters and run simulations for different conditions.

### Prerequisites

- R environment with necessary packages installed (tidyverse, ggpubr, dplyr)
- Input data file: `data/painting_schemainfo2.csv` (generated by the script)

### Steps

1. **Generate input data**: The script creates a CSV file containing schema information.
2. **Define parameter sets**: Create different parameter dataframes for different conditions (e.g., baseline, feedback, depression, excitement, high-reward, WSLS, integrated).
3. **Run simulations**: Call the `simulation` function for each condition and save results to specified directories.

### Example Code:

```r
res <- simulation(Param.df_baseline_1500s, "L", "painting", save = TRUE, 
                  savepath = "res_baseline_1500s/", sim.mode = "before", 
                  save.confi = TRUE, scale.confi.init = TRUE)
```

This will run a baseline simulation for 1500 seconds and save the results.

## 📊 Analysis & Visualization

The script `2_analysis_visualization.R` loads simulation results and generates figures used in the article.

### Prerequisites

- Simulation results saved as CSV files (e.g., `baseline_1500s_allresult_processed.csv`)
- Required R packages installed

### Generated Figures

- **Figure 2a**: Histogram of consecutive schema selections for WSLS vs. baseline.
- **Supplementary Figure 1**: Violin plots of behavioral metrics (reaction time, observation count, attention shifts) under WSLS condition.
- **Figure 2b**: Bubble plot of schema matching accuracy for WSLS vs. baseline.
- **Figure 2d**: Performance under different emotional states.
- **Figure 2e**: Violin plot of reaction times across different emotional states.
- **Supplementary Figure 2**: Distribution of accuracy for high-reward strategy vs. baseline.
- **Figure 2c**: Performance of different adaptive strategies over 1500 seconds.
- **Figure 2f**: Performance of different adaptive strategies over 2500 seconds.

### Statistical Tests

- **Kolmogorov-Smirnov test**: To compare distributions of consecutive schema selections.
- **Kruskal-Wallis test**: To compare behavioral metrics across different conditions.

Each figure is saved as a PNG file with descriptive filenames.

## 📝 Usage

1. **Set up environment**: Install R.
2. **Install required packages**: `install.packages(c("tidyverse", "ggpubr", "dplyr"))`.
3. **Prepare data**: Run the data generation part of `1_simulation_running.R` to create `data/painting_schemainfo2.csv`.
4. **Run simulations**: Execute `1_simulation_running.R` to run simulations for all conditions. This may take time depending on the number of subjects and rounds.
5. **Analysis & visualization**: After simulations are complete, run `2_analysis_visualization.R` to generate figures.

Make sure the CSV files generated by the simulation are in the correct paths specified in the scripts.

## ⚙️ Configuration

- **Simulation time**: Adjust the `before_time` parameter in `Param.df` to set simulation duration (e.g., 1500 or 2500 seconds).
- **Strategy parameters**: Modify parameters like `gamma_incentive`, `WSLS_boost_factor`, etc., to explore different strategy effects.
- **File paths**: Ensure save paths in `simulation` calls match your directory structure.

## 📁 Project Structure

```
.
├── src/
│   └── model_extend_adaptive_strategies.R  # Main simulation function
├── data/
│   └── painting_schemainfo2.csv            # Input data file
├── res_*/                                  # Simulation results directories for each condition
├── 1_simulation_running.R                  # Script to run simulations
└── 2_analysis_visualization.R              # Script for analysis and visualization
```


## 👥 Contributors

- [Guobiao Ye](https://github.com/goubiao-ye)

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.
