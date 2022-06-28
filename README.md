# EWS
Here we can share and develop R code for the EWS 

# Install Git
If not yet installed: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git

# Register on GitHub
For students (more functions): https://education.github.com/students

# Setup RStudio for Git and GitHub
Online-Tutorials, such as:
- https://github.com/llendway/github_for_collaboration/blob/master/github_for_collaboration.md
- https://www.youtube.com/watch?v=QLFc9gw_Hfs

# Clone repository (via RStudio)
1. Copy link of this page
2. In RStudio: File -> New porject -> Version control 


# Create data set

## Setup

- You need the CSV file containing the student background data and the CSV file with the term data for the students. 

- Create a copy of `config.R.example` called `config.R` and change the configuration variables according to your local machine.

## Variable calculation

- Run `calculate_student_vars.R`, which will create `student_vars.csv` in the data directory.
