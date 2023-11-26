# Categorical Naive Bayes Application
As part of the R programming course in the SISE Master's program, we had the to implement a naive bayes classifier. This Shiny application implements the categorical naive bayes package.
The Shiny application is available at this link: [Shiny application](https://categoricalnaivebayes.shinyapps.io/shinynaivebayes/).  
The code of the categorical naive bayes is availabe at this link: [Repo of the package](https://github.com/arthurparmentier99/categorical-naive-bayes-package).

# Shiny application
As mentioned above, the Shiny application is available at the following address: [Shiny Application](https://categoricalnaivebayes.shinyapps.io/shinynaivebayes/).  
This interactive application is designed to facilitate the exploration, analysis, training, and prediction using the Naive Bayes model. Below is an overview of the main functionalities available in this application.

## Shiny application structure

The pages are avalaible on the menu bar on the left: 

1. Home - Upon launching the application, you will land on the Home page. Here, you will find a warm welcome message and an introduction to the application's purpose. A central image representing the application and a brief description will guide you through the objectives.

2. Naive Bayes Info - Navigate to the "Info Naive Bayes" page to access detailed information about Naive Bayes. This section includes references to external documentation, tutorials, and resources that can enhance your understanding of the Naive Bayes algorithm.

3. Import Data - On this page, you can load your dataset in either .csv or .xlsx format, you have also to choose the separator for your dataset. The loaded data table will be displayed for your review.
![dataset](https://github.com/Ndeyefatou8/NaivebayesCategorial/assets/105281151/9e1ad148-3d48-4eb8-b02a-97dd1e5ab855)  

4. Explore Data - Move to the "Explore Data" page to perform statistical analysis on the loaded dataset. The page includes summaries and visualizations to help you understand the characteristics of your data.
For the quantitative variable:     
![explore1](https://github.com/Ndeyefatou8/NaivebayesCategorial/assets/105281151/c3007718-bec7-4e45-871a-43382ca9c6ae)  
![PLOT](https://github.com/Ndeyefatou8/NaivebayesCategorial/assets/105281151/0a2d8b28-4cb8-4cdb-8a11-64cd05f4ebce)  
![CORR](https://github.com/Ndeyefatou8/NaivebayesCategorial/assets/105281151/2edae5ec-8358-4c77-9d06-4eaf8142757a)  

5. Fit Model - Navigate to the "Fit Model" page to choose your target and explanatory variables, train the Naive Bayes model, and visualize some statistics on your dataset. For this examples, i choose "Name" for the target and for the explanatory variables : "Medical Condition" and "Admission Type".
![fit](https://github.com/Ndeyefatou8/NaivebayesCategorial/assets/105281151/b6d3ca67-e036-421f-961f-e5796646a9be)

6. Predict - The "Predict" page allows you to predict the classes of new data, which must perfectly match the dataset used for training. You can also export the results in `.csv` and `.xlsx`. Additionally, calculate the probability of belonging to each class for every observation.
![prdict1](https://github.com/Ndeyefatou8/NaivebayesCategorial/assets/105281151/b9cc07c5-65d6-407d-b41c-4660cdd5aba4)
![PREDICT2](https://github.com/Ndeyefatou8/NaivebayesCategorial/assets/105281151/cf6268ea-41a1-4483-8ed5-5760dae43d31)

# Use in R
## Prerequisites
All the packages you need are in the `app.R` and `server.R` files, so you can install them as required.

## Application
If you want to use this package in R download the files and the folder `www`.  
Put the folder as working directory and then, if you have installed all the packages as described above, you can run the `runApp()` command and your Shiny application should run in local.

# Authors
Arthur Parmentier - [GitHub](https://github.com/arthurparmentier99) - [Mail](mailto:arthurparmentier99@proton.me)
Ndeye Fatou Dieng - [GitHub](https://github.com/Ndeyefatou8) - [Mail](mailto:ndeye-fatou.dieng@univ-lyon2.fr)
