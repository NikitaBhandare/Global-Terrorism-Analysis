# Global Terrorism Database

The Global Terrorism Database (GTD) is an open-source database including information on terrorist events around the world from 1970 
through 2016 (with additional annual updates planned for the future). Unlike many other event databases, the GTD includes systematic 
data on domestic as well as transnational and international terrorist incidents that have occurred during this time period and now
includes more than 170,000 cases. In addition, data for the incidents of terrorism for the year 1993 are not present in the GTD because 
they were lost prior to START’s compilation of the GTD from multiple data collection efforts.

The National Consortium for the Study of Terrorism and Responses to Terrorism (START) makes the GTD available via this online interface
in an effort to increase understanding of terrorist violence so that it can be more readily studied and defeated.

## Characteristics of the GTD
Duration of Attack     - 1970 to 2016 (except 1993)
Number of Data Points  - 135
Number of Records      - 170,351
Geography              - Worldwide
Variables (Data Point) - Data is collected for variables like location, tactics, success of attack, target group name, number of victims 
                         killed or wounded, number of perpetrators killed or wounded, targets and outcomes.
Size                   - 73.9 MB

## Data Collection Points
The GTD was designed to gather a wide variety of etiological and situational variables pertaining to each terrorist incident. 
Depending on availability of information, the database records up to 135 separate attributes of each incident, including approximately 
75 coded variables that can be used for statistical analysis. These are collected under eight broad categories, as identified in the GTD
Codebook, and include, whenever possible:

1) GTD ID and Date:
- Event Id: Incidents from the GTD follow a 12‐digit Event ID system.
- Year, Month, Day, Approximate Date
- Extended Incident: whether the duration of an incident extended more than 24 hours or not.

2) Incident Information:
- Incident Summary: A brief narrative summary of the incident, noting the “when, where, who, what, how, and why.”
- Inclusion Criteria

3) Incident Location:
- Country, region, state/province, city, vicinity, Location Description.
- Latitude and longitude
- Geocoding specificity

4) Attack Information:
- Attack Type: 8 categories + unknown.
- Assassination, Hijacking, Kidnapping, Barricade Incident, Bombing/Explosion, Armed Assault, Unarmed Assault, Facility/Infrastructure Attack, and Unknown.
  Suicide Attack

5) Weapon Information:
- Weapon Type: 12 categories + unknown.
- Several sub weapon types.

6) Target Information:
- Target Type: 22 categories
- Several specific target/victim information, including names, nationalities, etc.

7) Perpetrator Information:
- Perpetrator Group Name: the name of the group that carried out the attack
- Several sub-group information, including number, claim, motive, etc.

8) Casualties and Consequences:
- Total Number of Fatalities, including Number of US Fatalities and Number of Perpetrator Fatalities
- Total Number of Injured, including Number of U.S. Injured and Number of Perpetrators Injured
- Property Damage, including damage extend, values and comments
- Total Number of Hostages/Kidnapping Victims, including US Hostages or Kidnapping Victims, kidnapping hours, countries, total ransom amount demanded, and number released/escaped/rescued

9) Additional Information and Sources:
- Additional relevant details about the attack, including International‐ Logistical, International‐ Miscellaneous, and sources.

# Executive Summary

Since 2004, the global terrorist attacks have increased exponentially. 
We cannot deny that they are obviously dangerous threats to civilians, governments, and societies. Therefore, we study the historical data from The Global Terrorism Database (GTD) in order to understand the patterns of the terrorist attacks. 
Our study could potentially identify the terrorist groups based on their attack characteristics and determine success rate of their attacks in order to avoid or reduce the degree of destruction in the future.

In the event of an attack, determining if an attack is successful could help governments take calculated measures against the attackers.
Also, this could help them manage the Law and Order situation better. With our final model using random forest technique, we are able to 
predict whether an attack would be successful or not with 80.7% accuracy. 
According to our model, the top-five variables that are important to classify successful attacks are nwound (the number of people got injured by the attack), nkill (the number of people got killed by the attack), attack type, day of a month, and target type. 

We believe that if we did further studies on this topic and found the effective method to protect people from an attack, the rate of successful attacks would be decreased substantially. Finally, our pilot model is a starting point of the study to help governments and related organizations identify the important factors to protect civilians from terror attacks.

Another study is that how we could identify the terrorist group behind the attacks in a specific region. Our study was scoped down to South Asia
region as a pilot study. 

In the South Asia region, 49% of the terrorist attacks are unclaimed. So, implementing our model could reduce the rate of unclaimed attacks by 6.89%. It could also provide an insight to Law Enforcement as to who can be the culprit of a terror attack. 

Likewise, this model could be used to study the attack patterns of terror activities in other specific regions or specific terror groups.

Website Link : https://gtaanalysis.000webhostapp.com/
