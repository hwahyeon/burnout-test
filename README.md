# Copenhagen Burnout Inventory (CBI) Program in Common Lisp

This project is a Common Lisp program for the Copenhagen Burnout Inventory (CBI), providing versions for South Korea, Greece, Serbia, Malaysia, and the original CBI. Each version has slightly different questions to ensure reliability and validity specific to each country's cultural, linguistic, and social context.

## Table of Contents

- [Overview](#overview)
- [Usage](#usage)
- [Country-Specific Versions](#country-specific-versions)
- [References](#references)

## Overview
The Copenhagen Burnout Inventory (CBI) measures burnout across three dimensions: personal burnout, work-related burnout, and client-related burnout. Developed by Tage S. Kristensen et al. in 2005, the CBI is known for its strong reliability and validity.<sup>[1](#ref1)</sup>

The CBI has high internal reliability and effectively differentiates between occupational groups. It correlates with other measures of fatigue and well-being and predicts outcomes such as sickness absence and intention to quit. 

## Usage

1. Run the program.
2. Enter your name.
3. Enter the country code corresponding to your location.
4. Answer the questions on a scale from 1 to 5.
5. Receive your burnout assessment.

## Country-Specific Versions

1. **Original CBI**<sup>[1](#ref1)</sup>
   - The original CBI consists of 19 questions divided into three groups: personal burnout (6 items), work-related burnout (7 items), and client-related burnout (6 items).

2. **South Korea**<sup>[2](#ref2)</sup>
   - The Korean version of the CBI (K-CBI) initially included all 19 items from the original version. However, item 13 (work-related burnout item 7), which asks ***"Do you have enough energy for family and friends during leisure time?"***, showed a low item-total correlation (r = -.19). The internal consistency reliability of the overall scale was Cronbach’s α = .93, but increased to .95 with the removal of this item. Consequently, this item was excluded as it did not directly address work-related burnout.

3. **Greece**<sup>[3](#ref3)</sup>
   - The Greek version of the CBI (CBI-Gr) followed WHO's forward-backward translation guidelines, involving language and content experts in both Greek and English. A pilot study was conducted to identify language barriers, and reconciliation meetings were held to refine the translation. In this program, questions are asked in English.
   - The face validity of CBI-Gr showed that all questions were understandable and clear, with index values surpassing 80%. The 3-structure model was found to be a good fit. Construct validity was satisfactory after the removal of one item and improved further with the removal of three items. Items removed included question 13 (***"Do you have enough energy for family and friends during leisure time?"***), question 12 (***"Do you feel that every working hour is tiring for you?"***), and question 17 (***"Do you feel that you give more than you get back when you work with clients?"***). The final model with 16 items showed satisfactory construct validity, and the usage of an 18-item model is also acceptable. In this program, the final model with 16 items is used.

4. **Serbia**<sup>[4](#ref4)</sup>
   - The Serbian version of the CBI is identical to the original CBI, consisting of all 19 questions without any omitted items. The study showed that the Serbian version can be used for the assessment of burnout among preschool teachers, demonstrating good test-retest reliability, good internal validity, and acceptable construct validity (However, further analyses on larger samples and teachers at various educational levels are recommended). 

5. **Malaysia**<sup>[5](#ref5)</sup>
   - The Malay version of the CBI (CBI-M) showed good factor structures and model fitness, supporting its construct validity (convergent and discriminant validity). However, one item (***"Do you have enough energy for family and friends during leisure time?"***) was removed due to validity issues, including gender bias.


## References

1. <a id="ref1"></a>**Kristensen, T. S., Borritz, M., Villadsen, E., & Christensen, K. B. (2005). The Copenhagen Burnout Inventory: A new tool for the assessment of burnout. *Work & Stress, 19*(3), 192-207.** [Link](https://doi.org/10.1080/02678370500297720)
2. <a id="ref2"></a>**강종수. (2022). 한국어판 코펜하겐소진척도(K-CBI)타당화 연구. *사회복지정책과 실천 (Social Welfare Policy and Practice)*, 8(1), 211-239.** [Link](https://doi.org/10.37342/swpp.2022.8.1.211)
3. <a id="ref3"></a>**Papaefstathiou, E., Tsounis, A., Malliarou, M., & Sarafis, P. (2019). Translation and validation of the Copenhagen Burnout Inventory amongst Greek doctors. *Health Psychology Research, 7*(1).** [Link](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6763708/)
4. <a id="ref4"></a>**Piperac, P., Todorovic, J., Terzic-Supic, Z., Maksimovic, A., Karic, S., Pilipovic, F., & Soldatovic, I. (2021). The Validity and Reliability of the Copenhagen Burnout Inventory for Examination of Burnout among Preschool Teachers in Serbia. *International Journal of Environmental Research and Public Health, 18*(13).** [Link](https://doi.org/10.3390/ijerph18136805)
5. <a id="ref5"></a>**Chin, R. W. A., Chua, Y. Y., Chu, M. N., Mahadi, N. F., Wong, M. S., Yusoff, M. S. B., & Lee, Y. Y. (2018). Investigating validity evidence of the Malay translation of the Copenhagen Burnout Inventory. *Journal of Taibah University Medical Sciences, 13*(1), 1-9.** [Link](http://dx.doi.org/10.1016/j.jtumed.2017.06.003)


