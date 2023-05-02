# EWS
The Early Warning System is designed to predict student dropout from administrative data. This repository documents the process of data pre-processing, descriptive analyses, and predictive models.

## Getting started
1. Install Git, if not yet installed: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
2. Register on GitHub: For students (more functions): https://education.github.com/students
3. Setup RStudio for Git and GitHub
  Online-Tutorials, such as:
  - https://github.com/llendway/github_for_collaboration/blob/master/github_f or_collaboration.md
  - https://www.youtube.com/watch?v=QLFc9gw_Hfs
4. Clone repository (via RStudio) by copying the link of this page, then In RStudio: File -> New project -> Version control 

# Project structure

## Data preprocessing
For the preprocessing you need:
- the CSV files containing the student background, term and course data for the students. 
- a copy of `config.R.example` called `config.R` and change the variables to match your local directory structure

You can execute the entire preprocessing by executing `pipeline.R`. The file calls the scripts in the directory `preprocessing`, which may also be executed separately. They usually depend on intermediate results from previous scripts.

## Descriptive analyses
The directory `analyses` contains scripts to create various descriptive analyses

## Models
The directory `models` contains scripts that try to model the statistical structure of student dropout.
