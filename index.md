---
title       : Data Prediction - Capstone Project
subtitle    : Data Prediction Model 
author      : Karthik Kalimuthu
job         : 
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## INTRODUCTION


This capstone project involved applying data science in the area of natural language processing to build an app to highlight a prediction algorithm generated from a corpus of text files provided by SwiftKey.

The dataset is provided by the Coursera course website. It is derived from a corpus called HC Corpora
www.corpora.heliohost.org.

The dataset provided included three corpus files (blogs, news, and twitter) for each of four locales (US English, German, Russian, and Finnish). This app focuses exclusively on the US English files.

---

## DATA PREDICTION ALGORITHM

* Download and Unzip The Data
* Select US Data
* Clean the Data & Remove Bad Words
* Tokenize the Data
* Explore the Data & Sample the Data
* Build N-Gram Model
* Build Predictive Model
* Data sampling was performed so as to reduce the size of the data. Final data Size was 1.2 MB compared with 560 MB for the original data.

---

## BUILDING PREDICTIVE MODEL

* The user inputs two words (a 2-gram) to the predict function - with the goal of generating most likely third words
* The 2-gram is searched for in the trigram model look-up table
* If that 2-gram does not exist, a simple message appears that The word did not exist in the look-up table (in prototype only) 
* If the bigram is found, a subset of all 3-grams starting ith that 2-gram are generated 
* For more accuracy, the same process can be repeated for 4gram & 3gram of data

<center><img src=./images/high-level-architecture1.png height='40%' width='40%' /></center>

---

## PERFORMANCE TUNING (MARKOV CHAIN)

Markov Chain Model is the main driver of this application. It represents each word as state with trasitional probabilities. As an example below, word "B" has 30% chance to transit to word "A".

   | A | B |
---|---|---|
 A | 0.6 | 0.4 |
 B | 0.3 | 0.7 |

`Add-1 Smoothing` is applied to distribute some of the high probability mass to words that have zero probability mass. The objective is to have a fairer prediction model.

`Shiny Application` embeds the `model` described above; along with the `Prediction Executor` and `User Interface`.

---

## HOW TO USE

* Type in some text and hit the `submit` button! 
* For more information, Hit the `Overview` tab!

<center><img src=./images/Application.png height='75%' width='75%' /></center>

---

### FUTURE PLANS

To improve the performance of this prediction model,

1. Increase Corpus size and/or type.
2. Cascade prediction model; this may improve prediction results.
3. Increase machine resource; be able to create more complex model.
4. Increase Shiny io resource; be able to host more complex text predictiona application.

Data Prediction Application Link - [here](https://spdkarthik.shinyapps.io/SCapstone/).

For more detailed information about the implementation, you may see [here](https://github.com/pkkarthikraaj/Capstone_Prediction)


