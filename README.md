# 1. MIES_project

This repostitory contains code corresponding to the paper: "COVID-19 Quarantine Reveals Behavioral Modifications Effect on Myopia Progression", which is submitted to the journal of Ophthalmology.

# 2.Abstract
COVID-19 quarantine provides the largest intervention data of myopia progression in schoolchildren. We found grade is an important risk factor rather than age, and six-month behavioral modifications sufficiently change the progression of myopia.

# 3. Introduction
Myopia is the most-common cause of visual impairment worldwide1, and younger onset may progress to high myopia 2. Since the coronavirus disease (COVID-19) has caused an unprecedented global pandemic, most nations have imposed various strict containment measures on citizens including limited outdoor activities and school closures to prevent COVID-19 virus spread. COVID-19 quarantine led to billions of students simultaneously experiencing life-altering challenges, which may influence the incidence and/or the worsening of myopia among schoolchildren 3. It is highly valuable to establish COVID-19 intervention model in a real world study to uncover the crucial risk factors of myopia development.

# 4. Materials and methods
# 4.1 Study design and participants
As part of our ongoing efforts within the Myopic Epidemiology and Intervention Study (MEIS) project, the current survey of refraction-confirmed myopia was conducted among more than one-million schoolchildren in Wenzhou City, Zhejiang Province, China in June 2019, and followed by two examinations in December 2019 and June 2020. In June of 2019, a census of 1,305 elementary and high schools encompassing different educational systems (key school and non-key school [i.e., public, sport, and martial arts]) in 11 districts of Wenzhou was conducted. There were 1,060,925 students aged 7 to 18 years recruited at the baseline, including 612,648 elementary school students (grades 1-6 in Chinese education system), 264,661 junior high school students (grades 7-9), and 183,616 senior high school students (grades 10-12).

All students identified in the census were invited to complete a self-administered questionnaire regarding demographic information and their knowledge of myopia. Certified technicians were trained at the Wenzhou Medical University Affiliated Eye Hospital with respect to standard procedures for determining visual acuity (VA) and auto refraction testing. Each school in the district was equipped with an auto refractometer (GoldEye RM-9000, Senzhen Aist Industrial Co., Ltd., China) and electronic logarithmic visual chart (GoldEye CM-1900C) by the Wenzhou Municipal Government for assessing the degree of myopia. These resources were utilized by trained technicians to examine all participants.

Among the 1,060,925 students, 18,627 students did not attend the VA and auto refraction testing. Participants with refraction values deemed nonstandard by the optometry staff were also excluded (N = 26,350), as were participants with atypical personal identity information (N = 14,119), resulting in 1,001,749 eligible participants (94.4% of the initial study population) remaining in the study. Subsequently, two follow-up examinations were conducted in December 2019 involving 813,755 students (81.2%) and June 2020 involving 768,492 students (76.7%). It was not possible to follow the remaining participants mainly because they had graduated from high school or refused to participate.

# 4.2 The screening process of MEIS samples
To facilitate this comprehensive myopia survey, we established the Wenzhou vision screening pattern, which is an integration of modernized vision screening and an online eyesight status information management system. The pattern possessed the characteristics of practicability, high feasibility, low cost, and modernization. We designed an efficiency recruitment strategies, supported by the Wenzhou education bureau and Eye Hospital of Wenzhou medical university. Moreover, we have set up the professional screening personnel. Vision screening personnel were composed of school health nurses, health care physicians (school doctors), and volunteers from graduate students in optometry. In order to guarantee that the vision screening project could be carried out smoothly and professionally, the local government issued the policy ensuring to train one capable screener per 250 students. The training mission was delegated to the Eye Hospital of Wenzhou Medical University by the Wenzhou government. Subsequently, the eye hospital organized senior optometrists to train the targeted training groups, including school nurses and health care physicians. The training, which consisted of four hours’ theoretical training and four hours’ practical training, was performed twice a year. The locations for centralized training were local hospitals of each district and county of Wenzhou. The exams were carried out immediately after training, and only by passing the exam could a screener be regarded as an eligible screener. 

# 4.3 Ethics and informed consent
The present study was approved by the Ethics Committee of the Wenzhou Medical University Affiliated Eye Hospital (approval numbers Wmu191204 and Wmu191205). All procedures were conducted in accordance with institutional/national research committee ethical standards as well as the 1964 Helsinki declaration. Participant completion of the self-administered questionnaire was considered informed consent.


# 4.4 Measurements and Outcomes
Due to the large sample size, performing the criterion standard of cycloplegic refraction to diagnose myopia was not applicable. Thus, the current survey adopted an alternative method of VA and autorefraction testing to identify myopia. VA was evaluated using an ‘‘E-type’’ standard logarithmic visual chart at a distance of 5 m. All students underwent noncycloplegic refraction testing using an automated refractometer, followed by complement subjective refraction testing for validation using an ‘‘E-type’’ standard logarithmic visual chart. Students who required corrective lenses were examined with and without eyeglasses, and naked eye refraction data were used to calculate the spherical equivalent. Myopia was identified as an uncorrected VA of less than 20/25, and spherical equivalent refraction (SER) was equaled to (sphere + [cylinder/2]) of -0.5 diopters (D) or less [1]. Since analysis for right and left eyes yielded a satisfactory correlation (Pearson correlation coefficient = 0.954, Supplemental Figure S1), SER data from the right eye were arbitrarily used for assessing myopia development, as reported in previous studies [2, 3]. An SER of -6.0 D or less indicated as high myopia, and an SER between -0.5 D and -6.0 D defined as non-high myopia [4]. 

From June 2019 to December 2019, the first 6-month interval indicated as “Normal” period without the influence of COVID-19 quarantine, and the second 6-month interval from January 2020 to June 2020 indicated as “COVID-19” period with the influence of COVID-19 quarantine. Since COVID-19 quarantine had a great influence on alterations of schoolchildren lifestyle and study behaviors, it provided a practical intervention model for comparing the differences of risk factors related to myopia between normal period and COVID-19 period. Earlier studies [5-7] have used the 6-month interval to measure myopia progression for exploring the intervention strategies. In addition, during August 2020, we further used a new-designed questionnaire to survey 12,013 students, which were randomly selected from all grades. The questionnaire consisted of 17 questions, including the basic characteristics of schoolchildren, myopia information of their parents and siblings, and students’ lifestyle or learning approaches such as outdoor activities times and online course times during normal period and COVID-19 period.

# 4.5 Statistical Analysis
The prevalence of myopia and high myopia were calculated by all eligible participants in three examinations. For prevalence, 95% confidence intervals (CIs) were calculated according to the procedure reported by Newcombe [8]. 

To examine factors associated with the incidence of myopia, the Cox proportional hazard regression analysis was performed on students who attended all three examinations and did not have myopia at the baseline. Similarly, Cox proportional hazard regression analysis was conducted to reveal risk factors associated with the incidence of high myopia on students who attended all three examinations and did not have high myopia at the baseline. These multivariate Cox regression analyses were adjusted for grade, age, gender, birth month, educational system (key/non-key school), and habitation (urban/rural). The survival package in R was applied to perform multivariate Cox regression analyses.

We performed comparison analysis to assess the influence of COVID-19 on myopia progression of six months between pre-COVID-19 and post-COVIV-19. To reduce the effects of age increasing during COVID-19 periods, we divided the students of each grade into two independent sets (pre-COVID-19 vs post-COVID-19) according to student birth months: pre-COVID-19 set includes students from September 1 to February 28 of next year, and post-COVID-19 set includes students from March 1 to August 31. 

The significance of differences between categorical variables was assessed using the Fisher’s test, and that of differences between continuous variables was assessed using Student’s t test. Two-sided P values were used in all statistical analyses. The Pearson correlation analysis was used to calculate the correlation of SER between right- and left-eye. The R software (ver. 3.6.1) was used for all statistical analyses (http://www.r-project.org).

To explore the effects of both students' online time and outdoor activity time during COVID-19 quarantine on myopia incidence and progression, we performed a multivariate Cox proportional hazard regression analysis for the association of two widely-reported behavioral changes (i.e., student's online course time and outdoor activity time) with myopia incidence, and a multivariate linear regression analysis for the association of the two behavioral changes with myopia progression among 12,013 students collected from August 2020. These multivariate regression analyses were adjusted for other potential risk factors, including grade, age, gender, birth month, educational system (key/non-key school), habitation (urban/rural), father's eye health, father's educational level, mother's eye health, mother's educational level, and electronic devices for online class (TV/computer/tablet/cellphone).


# 5 Myopic Epidemiology and Intervention Study (MEIS) 
# 5.1 Group members:
Liangde Xu, Ph.D.1,2,3, Yunlong Ma, Ph.D. 1,3, Jian Yuan, Ph.D.1,3, Yaru Zhang, M.S.1,3, Hong Wang, Ph.D.1,2, Guosi Zhang, M.S.1,3, Changsheng Tu, M.S.1,4, Xiaoyan Lu, M.S.1,2, Jing Li, M.S.1,2, Yichun Xiong, M.S.1,2, Fukun Chen, M.S.1,2, Xinting Liu, M.D. 1,2, Zhengbo Xue, M.S.1,2, Meng Zhou, Ph.D.1,3, Wen-Qing Li, Ph.D.5, Nan Wu, M.D.6, Hao Chen, M.D.1,2,4, Fan Lu, M.D.1,2,4, Jianzhong Su, Ph.D.1,2,3, Jia Qu, M.D.1,2,4 , Jie Sun, Ph.D. 1,2,3, Jinhua Bao, M.D. 1,2,4, Liang Ye, M.D. 1,2,4, Jun Jiang, M.D. 1,2,4, Xinjie Mao, M.D. 1,2,4, Xinping Yu, M.D. 1,2,4, Xiaoming Huang, M.D. 1,2,4, Jingjing Xu, M.D. 1,2,4, Miaomiao Li, M.D. 1,2,4, Xuemei Zhang, M.D. 1,2,4, Liang Hu, M.D. 1,2,4, Zhuopao Zuo, M.D. 1,2,4, Wanqing Jin, M.D. 1,2,4, Jiawei Zhou, M.D. 1,2,4, Yuwen Wang, M.D. 1,2,4, Xue Li, M.D. 1,2,4, Fang Hou, M.D. 1,2,4, Yinghao Yao, Ph.D.3, Yukuan Huang, M.S. 1,3, Dandan Fan, M.S. 1,3, Zhenhui Chen, Ph.D. 1,3, Fei Qiu, M.S. 1,3, Yijun Zhou, M.S. 1,3, Na Gao, M.S. 1,3, Xinyu Wang, M.S. 1,3, Kai Li, M.S. 1,3, Liansheng Li, M.S. 1,4, Xinrui Shi, Ph.D. 1,2, 3, Yuchun Deng, Ph.D. 1,3, Ping Hou, M.S. 1,3, Yu Bai, M.S. 1,3, Chenghao Li, M.S. 1,3, Siyu Wang, M.S. 1,3, Lu Chen, M.S. 1,3, Ke Li, M.S. 1,3, Lijun Dai, M.S. 1,3, Xiangyi Yu, M.S. 1,3, Peng Lin, M.S. 1,3, Jingting Zhao, M.S. 1,3, Qi Jiang, M.S. 1,3, Congcong Yan, M.S. 1,3, Siqi Bao, M.S. 1,3, Zicheng Zhang, M.S. 1,3, Ji Zhang, M.S. 1,3, Fangjie Guo, M.S. 1,3.

# 5.2 Institutions:
1. School of Ophthalmology & Optometry and Eye Hospital, Wenzhou Medical University, Wenzhou, 325027, China.
2. State Key Laboratory of Ophthalmology, Optometry and Visual Science, Wenzhou, 325027, China.
3. Institute of Biomedical Big Data, Wenzhou Medical University, Wenzhou, 325027, China.
4. National Clinical Research Center for Ocular Disease, Wenzhou, 325027, China.
5. Department of Cancer Epidemiology, Peking University Cancer Hospital & Institute, Beijing 100142, China.
6. Medical Research Center, Peking Union Medical College Hospital, Peking Union Medical College and Chinese Academy of Medical Sciences, Beijing, 100730, China.

# 6 References:
1. Flitcroft DI, He M, Jonas JB, et al. IMI–Defining and classifying myopia: a proposed set of standards for clinical and epidemiologic studies. Investigative ophthalmology & visual science 2019;60(3):M20-M30.
2. He M, Kong X, Chen Q, et al. Two-year changes in refractive error and related biometric factors in an adult Chinese population. JAMA Ophthalmol 2014;132(8):978-84.
3. He M, Xiang F, Zeng Y, et al. Effect of Time Spent Outdoors at School on the Development of Myopia Among Children in China: A Randomized Clinical Trial. JAMA 2015;314(11):1142-8.
4. Knutsen LJ, Weiss SM. KW-6002 (Kyowa Hakko Kogyo). Curr Opin Investig Drugs 2001;2(5):668-73.
5. Jin JX, Hua WJ, Jiang X, et al. Effect of outdoor activity on myopia onset and progression in school-aged children in northeast China: the Sujiatun Eye Care Study. BMC Ophthalmol 2015;15:73.
6. Huang PC, Hsiao YC, Tsai CY, et al. Protective behaviours of near work and time outdoors in myopia prevalence and progression in myopic children: a 2-year prospective population study. Br J Ophthalmol 2020;104(7):956-61.
7. Nakamura T, Isogai N, Kojima T, et al. Posterior Chamber Phakic Intraocular Lens Implantation for the Correction of Myopia and Myopic Astigmatism: A Retrospective 10-Year Follow-up Study. Am J Ophthalmol 2019;206:1-10.
8. Newcombe RG. Two‐sided confidence intervals for the single proportion: comparison of seven methods. Statistics in medicine 1998;17(8):857-72.
 
