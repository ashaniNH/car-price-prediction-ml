# car-price-prediction-ml
Advanced machine learning analysis for predicting used car prices using feature engineering, regression models, Random Forest, and XGBoost.
🚗 Car Price Prediction — Advanced Machine Learning Analysis
📌 Project Overview

This project develops and evaluates advanced machine learning models to predict used car prices using the Kaggle Car Price Prediction Challenge dataset.

The dataset contains 19,237 vehicles with 17 attributes describing vehicle characteristics such as manufacturer, model, production year, mileage, engine volume, fuel type, gearbox type, drive wheels, color, airbags, and interior features.

The objective is to estimate a car’s market value based on its attributes, enabling data-driven decision-making for buyers and sellers in the used car market.

📊 Dataset
Source: https://www.kaggle.com/datasets/deepcontractor/car-price-prediction-challenge
Observations: 19,237
Features: 17 vehicle attributes
Target Variable: LogPrice

Key variables include:

Manufacturer & Model
Production Year
Mileage
Engine Volume
Fuel Type
Gear Box Type
Drive Wheels
Color
Airbags
Interior Features
🔎 Workflow
1️⃣ Exploratory Data Analysis (EDA)
Distribution analysis
Missing value inspection
Correlation analysis
Outlier detection
2️⃣ Data Preprocessing & Feature Engineering
Isolation Forest for consistent outlier removal
Rare category grouping
Creation of new features:
Car Age
Mileage per Year
Feature binning:
Cylinders → performance categories
Airbags → safety levels
Standardization of numerical variables
One-Hot Encoding for categorical variables
🤖 Machine Learning Models

The following models were implemented and compared:

Multiple Linear Regression
Ridge Regression
Lasso Regression
Elastic Net
Random Forest
XGBoost
📈 Model Evaluation

Performance metrics:

Mean Squared Error (MSE)
R² Score
Model	Train R²	Test R²	Test MSE
Linear Models	Lower	Lower	Higher
Random Forest	0.803	0.732	902.3
XGBoost	0.842	0.762	799.8
⭐ Key Findings
Linear models struggle to capture nonlinear relationships.
Tree-based ensemble methods significantly improve prediction accuracy.
XGBoost achieved the best performance, while Random Forest showed strong stability.
Gradient boosting is highly effective for structured tabular pricing problems.
🛠️ Technologies Used
R
tidyverse
caret
randomForest
xgboost
ggplot2
Isolation Forest
