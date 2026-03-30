# car-price-prediction-ml
Advanced machine learning analysis for predicting used car prices using feature engineering, regression models, Random Forest, and XGBoost.<br>
🚗 Car Price Prediction — Advanced Machine Learning Analysis<br>
**📌 Project Overview <br>**

This project develops and evaluates advanced machine learning models to predict used car prices using the Kaggle Car Price Prediction Challenge dataset. <br>

The dataset contains 19,237 vehicles with 17 attributes describing vehicle characteristics such as manufacturer, model, production year, mileage, engine volume, fuel type, gearbox type, drive wheels, color, airbags, and interior features. <br>

The objective is to estimate a car’s market value based on its attributes, enabling data-driven decision-making for buyers and sellers in the used car market. <br>

**📊 Dataset <br>**
- Source: https://www.kaggle.com/datasets/deepcontractor/car-price-prediction-challenge <br>
- Observations: 19,237 <br>
- Features: 17 vehicle attributes <br>
- Target Variable: LogPrice <br>
Key variables include: <br>
- Manufacturer & Model <br>
- Production Year <br>
- Mileage <br>
- Engine Volume <br>
- Fuel Type <br>
- Gear Box Type <br>
- Drive Wheels <br>
- Color <br>
- Airbags <br>
- Interior Features <br>

**🔎 Workflow <br>**
1️⃣ **Exploratory Data Analysis (EDA) <br>**
- Distribution analysis<br>
- Missing value inspection<br>
- Correlation analysis<br>
- Outlier detection<br>
2️⃣ **Data Preprocessing & Feature Engineering <br>**
- Isolation Forest for consistent outlier removal <br>
- Rare category grouping <br>
- Creation of new features: <br>
       - Car Age <br>
       - Mileage per Year <br>
- Feature binning: <br>
       - Cylinders → performance categories <br>
       - Airbags → safety levels <br>
- Standardization of numerical variables <br>
- One-Hot Encoding for categorical variables <br>

**🤖 Machine Learning Models <br>**
The following models were implemented and compared: <br>
- Multiple Linear Regression <br>
- Ridge Regression <br>
- Lasso Regression <br>
- Elastic Net <br>
- Random Forest <br>
- XGBoost <br>

**📈 Model Evaluation <br>**
Performance metrics: <br>
- Mean Squared Error (MSE) <br>
- R² Score <br>
| Model | Train R² | Test R² | Test MSE |<br>
|:------|:--------:|:-------:|---------:| <br>
| Random Forest | 0.803 | 0.732 | 902.3 |<br>
| XGBoost | **0.842** | **0.762** | **799.8** |<br>

**⭐ Key Findings <br>**
- Linear models struggle to capture nonlinear relationships. <br>
- Tree-based ensemble methods significantly improve prediction accuracy. <br>
- XGBoost achieved the best performance, while Random Forest showed strong stability. <br>
- Gradient boosting is highly effective for structured tabular pricing problems. <br>

**🛠️ Technologies Used <br>**
- R <br>
- tidyverse <br>
- caret <br>
- randomForest <br>
- xgboost <br>
- ggplot2 <br>
- Isolation Forest <br>
