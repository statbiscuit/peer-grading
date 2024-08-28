## Overview

### Supplementary material for *A comparison of tutor- and peer-grading of `R` code* C. M. Jones-Todd and A. Renelle.

This study was reviewed and received ethics clearance through the University of Auckland Human Participants Ethics Committee (UAHPEC25385)

## Files

 + `01-analysis.r` contains all the `R` code for the summaries, plots, correlation and multivariate analysis of the data discussed in the manuscript. Sections of this script will output the plots found in the `plots/` directory.
 + `02-discrete_beta.r` contains the `RTMB` code for the discrete-Beta model discussed in the manuscript (including extra examples). Created plots are also output to the `plots/` directory.
 + `student_and_tutor-grades.RData` contains three `R` objects
    + `peer_grades` is a data frame with 741 observations and 10 variables
       + `file_name`, character submission ID
       + `assessor_id`, anonymised character peer-assessor ID
       + `o.completed`, logical, did the assessor leave an open-ended comment
       + `r.completed`, logical, did the assessor leave a rubric-specific comment
       + `score`, the total score awarded by each assessor
       + `item1`:`item5`, scores awarded by each assessor for each of the individual rubric sections
    + `tutor_grades` is a data frame with 741 observations and 8 variables
       + `file_name`, character submission ID
       + `grader`, anonymised character tutor ID
       + `criteria_1`:`criteria_5`, scores awarded by each grader for each of the individual rubric sections
       + `total`, total score awarded by each grader
    + `final_grades` is a data frame with 741 observations and 2 variables
       + `file_name`, character submission ID
       + `given_grade`, the student received grade post moderation after peer grading
 
