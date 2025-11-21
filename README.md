# RODEO ROC Curve Analyzer

A Shiny application for comprehensive ROC (Receiver Operating Characteristic) curve analysis with flexible filtering and comparison options.

**Developed by:** Hanash Lab Biostat Team  
**Authors:** Hamid Khoshfekr Rudsari & Ehsan Irajizad  
**Institution:** MD Anderson Cancer Center  
**Last Update:** November 2025

---

## Features

### Core Functionality
- **Interactive ROC Curve Analysis**: Generate and visualize ROC curves for multiple biomarkers simultaneously
- **Comprehensive Performance Metrics**: Calculate AUC with 95% confidence intervals
- **Sensitivity at Fixed Specificities**: Automatically compute sensitivity at 60%, 70%, 80%, 86%, 90%, 95%, and 98.5% specificity
- **Sample Size Reporting**: Display number of cases and controls for each marker

### Data Input
- Support for multiple file formats: CSV, TSV, Excel (.xlsx, .xls)
- Automatic file type detection and parsing

### Advanced Filtering
- **Three Independent Filters**: Apply up to 3 simultaneous filters on different columns
- **Numeric Filtering**: Set minimum and maximum value ranges
- **Categorical Filtering**: Select specific categories with "Select All" / "Deselect All" options
- **Missing Value Handling**: Option to include or exclude NA/blank values

### Flexible Group Comparison
- **Multiple Case Groups**: Select one or more groups as cases
- **Multiple Control Groups**: Select one or more groups as controls
- **Missing Value Groups**: Option to use NA/blank values as a group
- **Multi-Marker Selection**: Compare multiple biomarkers in a single analysis

### Visualization
- **Square ROC Plot**: Standard 1:1 aspect ratio for accurate curve visualization
- **Color-Coded Curves**: Rainbow color scheme for easy marker differentiation
- **Reference Line**: Diagonal line for random classifier comparison
- **Grid Lines**: Enhanced readability with background grid
- **Legend**: Clear marker identification

### Results Table
- Marker name
- Sample sizes (N Cases, N Controls)
- AUC with 95% confidence interval
- Sensitivity values at 7 different specificity thresholds

---

## Installation

### Required R Packages
```r
install.packages(c("shiny", "pROC", "ggplot2", "dplyr", "readxl"))
```

### Running the Application
To run the app, open the app in RStudio and:
- **Mac**: Press `Shift+Command+S`
- **Windows**: Press `Ctrl+Shift+S`

Alternatively, you can click the "Run App" button that appears at the top of the script editor when you have a Shiny app file open.

---

## Usage Guide

### 1. Upload Data
- Click "Browse..." and select your data file (CSV, TSV, or Excel)
- Ensure your data has:
  - A column identifying case/control groups
  - Numeric columns for biomarker values

### 2. Apply Filters (Optional)
For each of the 3 available filters:
- **Select Column**: Choose which column to filter
- **For Numeric Columns**: Set minimum and maximum values
- **For Categorical Columns**: 
  - Use "Select All" or "Deselect All" buttons
  - Check/uncheck specific categories
  - Include NA/blank values if needed

### 3. Define Comparison Groups
- **Select Comparison Column**: Choose the column that defines your groups
- **Select Case Group(s)**: Check one or more groups to be treated as cases (positive class)
- **Select Control Group(s)**: Check one or more groups to be treated as controls (negative class)

**Note:** You can select NA/blank values as either cases or controls, but not both

### 4. Select Biomarkers
- Check one or more numeric columns to analyze as biomarkers
- Multiple markers will be displayed on the same ROC plot for comparison

### 5. Run Analysis
- Click "Run Analysis" button
- View the ROC curve plot and results table

---

## Data Format Requirements

### Example Data Structure
```
| Marker1 | Marker2 | Group      | Age | Gender |
|---------|---------|------------|-----|--------|
| 125.3   | 45.2    | Cancer     | 65  | Female |
| 98.4    | 52.1    | Control    | 44  | Female |
| 145.7   | 38.9    | Cancer     | 52  | Male   |
| 102.1   | 48.3    | NA         | 59  | Female |
```

### Requirements
- **Biomarker columns**: Must be numeric
- **Group column**: Can be text or categorical
- **Missing values**: Can be NA, blank, or empty strings
- **No row names required**: Data should be in standard tabular format

---

## Output Interpretation

### ROC Plot
- **X-axis**: 1 - Specificity (False Positive Rate)
- **Y-axis**: Sensitivity (True Positive Rate)
- **Diagonal line**: Represents random classification (AUC = 0.5)
- **Curves above diagonal**: Better than random performance
- **Higher curves**: Better classifier performance

### Results Table Columns
- **Marker**: Biomarker name
- **N Cases**: Number of case samples with non-missing marker values
- **N Controls**: Number of control samples with non-missing marker values
- **AUC**: Area Under the Curve (0.5 = random, 1.0 = perfect)
- **95% CI Lower/Upper**: Confidence interval bounds for AUC
- **Sens @ XX% Spec**: Sensitivity achieved at specified specificity levels

### AUC Interpretation Guide
- **0.90 - 1.00**: Excellent discrimination
- **0.80 - 0.90**: Good discrimination
- **0.70 - 0.80**: Fair discrimination
- **0.60 - 0.70**: Poor discrimination
- **0.50 - 0.60**: Fail (no better than chance)

---

## Tips and Best Practices

### 1. Sample Size Considerations
- Ensure adequate sample sizes in both case and control groups
- Small sample sizes may result in wide confidence intervals
- Check N Cases and N Controls in the results table

### 2. Missing Values
- The app automatically handles missing biomarker values
- Samples with missing values in selected markers are excluded from that specific analysis
- You can explicitly include/exclude groups with missing labels using the NA option

### 3. Multiple Comparisons
- When analyzing multiple biomarkers, consider the number of tests being performed
- Look for markers with non-overlapping confidence intervals for clearer differences

### 4. Filtering Strategy
- Apply filters before selecting comparison groups to ensure proper sample selection
- Use multiple filters to create specific subpopulations for analysis
- Example: Filter by age range, gender, and disease stage simultaneously

### 5. Group Selection
- You can combine multiple categories into case or control groups
- Example: Combine "Stage I" and "Stage II" as early-stage cases
- This allows flexible comparison strategies

---

## Troubleshooting

### Common Error Messages

**"No case observations found" / "No control observations found"**
- Check that your selected groups exist in the filtered data
- Verify that filters aren't excluding all samples from selected groups

**"Case and control groups cannot both include NA"**
- You've selected NA/blank values for both cases and controls
- Choose NA for only one group, or select specific non-NA categories

**"Please select at least one case/control group"**
- You must select at least one checkbox for both cases and controls
- If checkboxes are not visible, check that a comparison column is selected

**Empty or unexpected results**
- Verify that selected marker columns are numeric
- Check for sufficient non-missing values in your markers
- Ensure comparison groups are correctly defined in your data

---

## Technical Details

### Statistical Methods
- ROC curves computed using the `pROC` package
- AUC confidence intervals calculated using DeLong's method (default in pROC)
- Direction automatically set to "<" (lower marker values in controls)
- Sensitivity interpolated at exact specificity thresholds

### Performance Considerations
- Efficient filtering applied sequentially
- Reactive programming ensures updates only when inputs change
- Suitable for datasets with thousands of samples
- Multiple markers analyzed independently

---

## Contact and Support

For questions, bug reports, or feature requests:

**Hanash Lab Biostat Team**  
MD Anderson Cancer Center  
- Hamid Khoshfekr Rudsari
- Ehsan Irajizad

---

## License

This tool is developed for research purposes at MD Anderson Cancer Center.

---

## Version History

**v1.0 (November 2025)**
- Initial release
- Support for CSV, TSV, and Excel files
- Three independent filtering options
- Multiple case and control group selection
- ROC curve visualization with performance metrics
- Sensitivity at fixed specificity thresholds