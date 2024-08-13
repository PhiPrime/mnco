
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mnco

<!-- badges: start -->
<!-- badges: end -->

The goal of mnco is to …

## Installation

You can install the development version of mnco from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("PhiPrime/centerOverview")
```

## Required Data Collection

mnco relies on exported files from Radius, Mathnasium’s internal
operations website. You can use the below links (and applicable
instuctions) to retrive all the data you’ll need.

### Daily:

[Students](https://radius.mathnasium.com/Student)  
[Accounts](https://radius.mathnasium.com/CustomerAccount)  
[Progress
Report](https://radius.mathnasium.com/ProgressReportManager/CurrentBatchDetail)  
[Assessments](https://radius.mathnasium.com/AssessmentReport)  
\* Select **All** in *Pre/Post* dropdown box  
[Enrollments](https://radius.mathnasium.com/Enrollment/EnrollmentReport)

### Other (used for updates)

[Assessment History](https://radius.mathnasium.com/AssessmentReport)  
\* Select **All** in *Pre/Post* dropdown box  
\* Select a date with the year **from 1970 to 2002** in *From:* dropdown
box  
\* Export and rename file to “Assessments_Prior_to_M_D_YYYY.xlsx”  
\* Move file to data directory manually or with provided code  
[Payments](https://radius.mathnasium.com/Payment)  
\* Not in regular routine  
[Curriculum](https://radius.mathnasium.com/CurriculumManager/CurriculumSearch)  
\* Used to update curriculum database  
[Email Templates](https://radius.mathnasium.com/EmailTemplate)  
[Attendance](https://radius.mathnasium.com/StudentAttendanceMonthlyReport)  
\* Not in regular routine
