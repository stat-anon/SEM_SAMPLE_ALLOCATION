」 # Sample Allocation Optimizer for Two-Group SEM

This repository contains the R implementation accompanying the manuscript *[title omitted for blind review]*.  
The toolbox provides functions and examples for optimizing sample allocation across two groups in multiple-group structural equation modeling (MGSEM).

---

## Contents

- **Core functions (`99_all_in_1.R`)**  
  Complete function suite, organized into:
  - Power engine  
  - Constraint solvers  
  - Optimization strategies (grid, fixed-step, adaptive step)  
  - Heuristics and tie-breakers  
  - Visualization routines  

- **Quick Demos (Recommended)**  
  Lightweight examples that run with adaptive search only.  
  These scripts reproduce the main results quickly and are the best entry point for new users and reviewers.

- **Full Demos**
   More detailed scripts reproducing the analyses from the manuscript:  
   - Examples 1–3 (factor mean differences, path coefficient differences, and metric invariance).  
   - Validation with Classical Results (single-indicator two-group mean comparisons against the closed-form allocation formula).
     
   These scripts compare exhaustive grid search, staged fixed-step refinements, and adaptive search.  
  They provide finer resolution but may require substantial runtime (minutes to hours depending on grid size and model complexity).

## Getting Started
- The repository includes an empty `.here` file so that the root directory is consistently recognized when using the **here** package for relative paths.  
- To run the analyses, first download all R scripts and the demo script(s) you wish to execute.  
- We recommend starting with the **Quick Demos**, which illustrate the workflow with adaptive search and run quickly.  
- The **Full Demos** reproduce all results from the manuscript but involve exhaustive searches, which can be computationally intensive (minutes to hours depending on grid size and model complexity).


---

## License

This repository is released under the MIT License.  
See the [LICENSE](LICENSE) file for details.

