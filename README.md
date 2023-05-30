# Dashboards-GUIs
A collection of dashboards/GUIs created in RShiny and Python


# Table of contents
1. [Comet Score](#cometScore)
2. [Land Survey GUI](#ellisGUI)
4. [MATTI](#matti)



## **Comet Score:**<a name="cometScore"></a>

**Files included:** CometScoreGUI.R,CometScore_v0_4REDACTED.R

**Purpose:** SEO teams have only the Google Lighthouse score to use as a baseline for their website's performance on a global ecommerce level. This is limited to using only controlled synthetic data. This is useful as a baseline, but many SEO teams would like to know what their RUM (Real User Data) shows as far as their performance on the global scale. Using Big Query and http Archive's database, I have developed a similar scoring method as Google's score, but utilizeing the very noisey and outlier-prone RUM data. This particular version is still in Beta and not release-ready.

It is based on Google's Lighthouse Algorithm which uses the Abramowitz and Stegun formula to derive a log-normal cumulative distribution function that can ultimately be used to derive a score of 0-1 from a given timing metric. Google uses a weighted average of several of these metrics to derive a site's ultimate Lighthouse Performance Score.

personal note: this is my pride and joy, and by far my biggest undertaking as a solo project. Of all my GUIs, this one has the cumalitive of my knowledge base. This field is constantly changing, and with it the way Comet Score is calculated. For more SEO information, visit www.thisisoptimal.com


**Libraries used:** dplyr, json, plotly, stringr, shiny

**Output:**

![image](https://github.com/PlaidDragon/Dashboards-GUIs/assets/135033377/0bf2a43f-0d05-455d-834a-1b300597116b)
![image](https://github.com/PlaidDragon/Dashboards-GUIs/assets/135033377/1fa0e109-9b84-48fc-8b58-fefe29eab06a)


## **Land Surveying Database:**<a name="ellisGUI"></a>

**Files included:** EllisGUI_v_0_7.py

**Purpose:** A specific Land Surveying company was family owned and too small to have any sort of database implimented. Their workflow was becoming very inefficient with jobs being duplicated and crews being sent out to the same area of the town. I designed a GUI using PyQT5 package designed to be hooked into a SQL server; possibly hosted on an AWS EC2 server. The main goal was for this to be as cost-effective as possible, and such it uses the folium mapping API as well as a free geo-coding API. Future expansion would most likely include expanding to Google Maps API (which has more functionalilty) and AWS RDS for the database itself. The version included in this repository is hard-coded with no connection to a server or geo-coding API as it is only the demo version.


personal note: I was not exaclty asked to make this, but hearing my boyfriend rant about his issues at work I took the opportunity to make this during my free-time. It is not my first GUI in Python, but It is definitely the most robust and clean-coded GUI I have made in Python. The fact it essentially runs at no-cost is a huge bonus for small companies. Again, I ask this not be implimented without my permission.


**Libraries used:** sys, numpy, pandas, PyQT5, folium

**Output:**
![image](https://github.com/PlaidDragon/Dashboards-GUIs/assets/135033377/ffc29772-0ae4-4aad-be0f-d1870514fb87)
![image](https://github.com/PlaidDragon/Dashboards-GUIs/assets/135033377/835195b8-3b7b-42cf-864d-77838608e5c8)
![image](https://github.com/PlaidDragon/Dashboards-GUIs/assets/135033377/11c41bd9-fd69-48d8-8a1b-fe6c51d218cf)





## **Mas to TrackTrace Interface (MATTI):**<a name="matti"></a>

**Files included:** MATTIProductionV3.R, MATTISOAPFunctionsREDACTED.R

**Purpose:** Due to compliance laws, any transfer of pharmaceuticals needs to have a traceable paper trail (called a T3). This particualr company had no easy way to input their T3 data into their third-party tracking vendor. The purpose of MATTI was to create functions that connected into TrackTrace's back end (using SOAP) and allows the compliance officer to quickly move T3 information from the company's internal database to the Third Party tracking vendor's database, with no to minimal manual entry of the data. The results was MATTI, which utilizes RShiny and R odbc connections to pull data from the internal company's systems, present the data to the compliance officer in a readable form, allow any necessary fixes (if any), and push the resulting information en mass to the third part vendor. MATTI sucessfully increasesd the compliance officer's output 10 fold.

personal note: this was my first attempt at a Shiny application and has a close place in my heart, despite my parting with the company it was implimented on and its admittedly naive coding style


**Libraries used:** RMySQL, tidyverse, rio, readxl, RCurl, xml2, RODBC

**Output:**

![image](https://github.com/PlaidDragon/Dashboards-GUIs/assets/135033377/4be69b6f-9a71-4dd6-8a8b-c8666e919535)
