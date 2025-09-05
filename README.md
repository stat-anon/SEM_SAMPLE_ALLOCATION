# Sample Allocation Optimizer for Two-Group SEM

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
  More detailed scripts (Examples 1–3) comparing exhaustive grid search, staged fixed-step refinements, and adaptive search.  
  These provide finer resolution but may require substantial runtime (minutes to hours depending on grid size).

- **Sanity checks**  
  Validation against classical two-sample *t*-test allocation formulae.  

## Getting Started
The repository contains an empty .here file to ensure that the root directory is consistently recognized when using the here package for relative paths.

---

## License

This repository is released under the MIT License.  
See the [LICENSE](LICENSE) file for details.

