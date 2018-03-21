# Credit-Card-Fault-Analysis
Statistical Analysis and Prediction for Credit Card defaulters based on Taiwan's Credit banking data

Introduction:
In recent years, the credit card issuers in Taiwan faced the cash and credit card debt crisis. 
To increase the market share, card-issuing banks in Taiwan over-issued cash and credit cards to unqualified applicants. 
At the same time, most cardholders, irrespective of their repayment ability, overused credit card for consumption and 
accumulated heavy credit and cash– card debts. The crisis caused a major blow to consumer finance confidence and 
it is a big challenge for both banks and cardholders. In a well-developed financial system, crisis management is on 
the downstream and risk prediction is on the upstream. The major purpose of risk prediction is to use financial information, 
such as business financial statement, customer transaction and repayment records, etc., to predict business performance or individual 
customers’ credit risk and to reduce the damage and uncertainty.

Data Gathering:
This dataset contains information on default payments, demographic factors, credit data, history of payment, 
and bill statements of credit card clients in Taiwan from April 2005 to September 2005. The data has been collected from Kaggle.com. 
The number of instances is 30,000 and the number of variables is 25. 

****DATASET LINK****: https://www.kaggle.com/uciml/default-of-credit-card-clients-dataset

Tools Used:
1) RStudio: It is a free and open-source integrated development environment(IDE) for R, 
a programming language for statistical computing and graphics.
2) Tableau: It is used for creating data visualizations, publishing data sources as well as workbooks to Tableau Server.

Instructions to run code:
Open thr R file through R or R studio and change the file path (link and link2) as instructed.
Link: local path to the downloaded dataset
Link2: local path to store output files

Look at the statistical outputs of various models (Model_LimitPay_Int seems most accurate) in the console output.
Link2 is used to save the correlation plots and matrix.

****Dataset Information (as mentioned on the Kaggle link)**** 
https://www.kaggle.com/uciml/default-of-credit-card-clients-dataset
This dataset contains information on default payments, demographic factors, credit data, history of payment, and bill statements of credit card clients in Taiwan from April 2005 to September 2005.

Content

There are 25 variables:

ID: ID of each client
LIMIT_BAL: Amount of given credit in NT dollars (includes individual and family/supplementary credit
SEX: Gender (1=male, 2=female)
EDUCATION: (1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown)
MARRIAGE: Marital status (1=married, 2=single, 3=others)
AGE: Age in years
PAY_0: Repayment status in September, 2005 (-1=pay duly, 1=payment delay for one month, 2=payment delay for two months, ... 8=payment delay for eight months, 9=payment delay for nine months and above)
PAY_2: Repayment status in August, 2005 (scale same as above)
PAY_3: Repayment status in July, 2005 (scale same as above)
PAY_4: Repayment status in June, 2005 (scale same as above)
PAY_5: Repayment status in May, 2005 (scale same as above)
PAY_6: Repayment status in April, 2005 (scale same as above)
BILL_AMT1: Amount of bill statement in September, 2005 (NT dollar)
BILL_AMT2: Amount of bill statement in August, 2005 (NT dollar)
BILL_AMT3: Amount of bill statement in July, 2005 (NT dollar)
BILL_AMT4: Amount of bill statement in June, 2005 (NT dollar)
BILL_AMT5: Amount of bill statement in May, 2005 (NT dollar)
BILL_AMT6: Amount of bill statement in April, 2005 (NT dollar)
PAY_AMT1: Amount of previous payment in September, 2005 (NT dollar)
PAY_AMT2: Amount of previous payment in August, 2005 (NT dollar)
PAY_AMT3: Amount of previous payment in July, 2005 (NT dollar)
PAY_AMT4: Amount of previous payment in June, 2005 (NT dollar)
PAY_AMT5: Amount of previous payment in May, 2005 (NT dollar)
PAY_AMT6: Amount of previous payment in April, 2005 (NT dollar)
default.payment.next.month: Default payment (1=yes, 0=no)
Inspiration

Some ideas for exploration:

How does the probability of default payment vary by categories of different demographic variables?
Which variables are the strongest predictors of default payment?
Acknowledgements

Any publications based on this dataset should acknowledge the following:

Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.

The original dataset can be found here at the UCI Machine Learning Repository.

**** Acknowlledgements****:
1)Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
2) https://www.kaggle.com/uciml/default-of-credit-card-clients-dataset
