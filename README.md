# BTOSFinalProject
 The Business Trends and Outlook Survey (BTOS) ,is a survey that provides insight into the state of the economy by providing timely data for key economic measures and business expectations about future conditions

BTOS Data Documentation 

The BTOS sample consists of approximately 1.2 million businesses split into 6 panels
(approximately 200,000 cases per panel). Businesses in each panel will be asked to report once every 12 weeks for a year. Data collection will occur every two weeks. 

Variables

State : State where the business is located
Question ID : The Question number in the survey i.e 3,6,7
Question : The question asked in the BTOS
Answer ID :Numeric ID of the survey response i.e 1,2
Answer : Text of the response i.e Excellent
XYYYYMM: Bi-weekly survey responses . The value is reported as the percentage of respondents.

N/B

S - Estimate does not meet publication standards because of high sampling variability, poor response quality, or other concerns about the estimated quality. Unpublished estimates derived by subtraction are subject to these same limitations and should not be attributed to the U.S. Census Bureau
 
Engineered Features

State : State where the business is located
Survey.Year : Year the survey was taken
Survey.WeekNum : Week the survey was taken
Performance.Score : Net change in performance score
Net.Revenue.Q4.Index : Net change in revenue 
Net.Employee.Q5.Index : Net change in number of employees
Net.Hours.Q6.Index : Net change in number of hours worked
Net.AI.Q7.Index : Net change in the current  use of AI
Net.Demand.Q10.Index : Net change in the demand of goods and services
Net.Prices.Q11.Index : Net change in the prices the business’s charges for its own goods & services
Net.SuppliesPrices.Q12.Index : Net change in the cost of supplies
Net.AI.Q24.Index : Net change in the business's outlook on future use of AI

