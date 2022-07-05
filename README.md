# EWS
The Early Warning System is designed to predict student dropout from administrative data. This repository documents the process of data pre-processing and hence makes this step reproducible.

## Getting started
1. Install Git, if not yet installed: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
2.  Register on GitHub: For students (more functions): https://education.github.com/students

3. Setup RStudio for Git and GitHub
  Online-Tutorials, such as:
  - https://github.com/llendway/github_for_collaboration/blob/master/github_f or_collaboration.md
  - https://www.youtube.com/watch?v=QLFc9gw_Hfs

4. Clone repository (via RStudio) by copying the link of this page, then In RStudio: File -> New project -> Version control 


# Create data set

## Setup

- You need the CSV file containing the student background data and the CSV file with the term data for the students. 

- Create a copy of `config.R.example` called `config.R` and change the configuration variables according to your local machine.

## Pipeline
You can run the entire pipeline by executing `pipeline.R`. This file runs the following scripts that also can be executed separately but depend on the previous scripts in the given order.

1. Variable calculation: `calculate_student_vars.R` derives additional variables and outputs them to `student_vars.csv` in the data directory.

2. Subsetting: `subset.R` filters the students used for the model fitting and validation.