# German_Credit_Analysis
Project features a model finding process for determining the creditworthiness of loan applicants. Final model improves over a blind proportional model by an estimated 13% per unit profit.

Completed by Connor Lenio. Email: cojamalo@gmail.com

Projec Report
Completion Date: Oct 28, 2017

Please view https://cojamalo.github.io/German_Credit_Analysis/report.html to properly view the report.

# Analysis Scenario

A loan manager is requesting a statistical model to help her department determine which loan applicants are creditable, i.e. most likely to repay their loans. An applicant’s demographic and socio-economic profiles are considered by loan managers before a decision is made regarding his/her loan application. The manager's goal is to minimize risk and maximize profits for the bank's loan portfolio. The manager shares the information that for the type of loans the model would predict, if the borrower pays back the loan, the bank makes a profit of 35% the value of the loan. On the other hand, if the borrower defaults, the bank's loss is 100%. The bank does not lose money for applicants who are rejected and the manager claims the model does that have to take into account opportunity cost for applicants who would have repaid the loan but were rejected.

Upon receiving this request, I decided to develop a model for the manager that maximizes a profit-cost function given the provided data. The priority of the model fitting task will be prediction in this case as the manager has not specifically requested an interpretable model, but has requested a model with the best profit characteristics.

# Data Source

The loan manager gives you access to a sample of her department's loan data for 1000 applicants with the outcome of their loans included. She claims the dataset was prepared by another analyst with her input to be representative of the bank's actual customers. 

The data used in this project was originally provided by Dr. Hans Hofmann of the University of Hamburg and hosted by the UCI Machine Learning Repository. The specific version of the data used here (`credit`) was sourced from Penn State's graduate-level Applied Data Mining and Statistical Learning 897D course.
