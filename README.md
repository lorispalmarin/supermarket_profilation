# 📊 Is It All About Creativity? - Data-Driven Marketing Strategy

## 🔍 Project Overview
This project was developed as part of the **Statistical Learning exam** at the **University of Milan - Data Science for Economics** program. The objective is to apply **unsupervised and supervised machine learning techniques** to analyze customer data from an international supermarket chain and provide actionable insights for marketing strategies.

### 🎯 Goals
1. **Customer Segmentation**: Use clustering techniques to segment the consumer market and improve targeting.
2. **Predictive Modeling**: Implement machine learning models to predict customer responses to future marketing campaigns.
3. **Business Insights**: Provide data-driven recommendations to optimize marketing efforts and engagement.

---

## 📂 Dataset
The dataset is sourced from **Maven Analytics** and contains **2,240 customers** with **28 features**, including:
- **Demographics** (Age, Education, Marital Status, Income)
- **Purchase Behavior** (Product category spending, Recency of last purchase)
- **Marketing Interactions** (Previous campaign responses, Website visits)
- **Engagement Metrics** (Total purchases, Discount usage)

---

## 🔬 Methodology

### **1️⃣ Data Preparation & Preprocessing**
- Removed missing values and outliers.
- Engineered new features (e.g., total purchases, years of customer loyalty).
- Standardized numerical variables and encoded categorical features.

### **2️⃣ Exploratory Data Analysis (EDA)**
- Distribution analysis of key features.
- Correlation heatmaps to identify significant relationships.
- Examined purchasing patterns and customer segmentation potential.

### **3️⃣ Customer Segmentation (Clustering)**
- Applied **K-Means Clustering** to group customers based on numerical features.
- Enhanced results by incorporating **categorical variables using t-SNE & Gower Distance**.
- Identified 3 key customer segments:
  - **Family Explorers**: Budget-conscious customers with children, prefer in-store purchases.
  - **Gourmet Winers**: High-income individuals with a strong preference for fine wines.
  - **Wine Teen Bonding**: Older customers with teenagers, balanced online & in-store spending.

### **4️⃣ Predictive Modeling (Supervised Learning)**
- Compared **Logistic Regression, Stepwise Selection, and Random Forest**.
- **Random Forest** was the best-performing model with:
  - **97% Recall** (best at identifying potential respondents to marketing campaigns).
  - **High Precision & Accuracy**.
- Identified **key predictors** of marketing success: **recency of purchase, income, and previous campaign responses**.

---

## 📈 Key Findings & Business Implications
✅ **Segmenting customers leads to better-targeted marketing campaigns.**
✅ **Customers with recent purchases are more likely to respond positively.**
✅ **High-income customers engage less with promotional offers.**
✅ **Wine is a strong driver of customer engagement.**
✅ **Random Forest provides the best prediction for marketing response.**

### 🔥 Marketing Recommendations
- **Personalized Newsletters:**
  - **Family Explorers** → Coupons & interactive promotions.
  - **Gourmet Winers** → Exclusive wine-related offers.
  - **Wine Teen Bonding** → Content on wine pairings & family experiences.
- **Optimize Campaign Timing:** Prioritize customers based on **recency of last purchase**.
- **Increase Engagement:** Leverage **historical campaign responses** to fine-tune targeting.

---

## ⚙️ Technologies Used
- **Programming:** Python & R
- **Machine Learning Libraries:** scikit-learn, pandas, NumPy
- **Visualization:** Matplotlib, Seaborn
- **Clustering & Dimensionality Reduction:** K-Means, t-SNE, Gower Distance
- **Cloud & Version Control:** GitHub

---

## 📌 Future Work
🔹 Expand dataset to include **online behaviors & social media interactions**.
🔹 Explore **deep learning models** for improved predictive accuracy.
🔹 Implement **real-time marketing optimization** using AI-based recommendations.
