TITLE 'Validation, all tiers';
data v1;
set series.confusion_corr;
if SameClosestZone ne " " AND SameClosestPen ne " " then antenna = "true"; else antenna = "false";
PROC FREQ;
     TABLE ANTENNA*HEN /EXACT;

data v2;
set series.confusion_corr;
if DetectedInTier = "Detected in zone within 1 minute" then antenna1min = "1min"; else antenna1min = "not1min";
PROC FREQ;
     TABLE ANTENNA1MIN*HEN /EXACT;

data v3;
set series.confusion_corr;
if SameClosestTier = " " or SameClosestPen = " " then s = 'f'; 
if SameClosestPerspective = " " then s = 'f';
if s = 'f' then closestTierSidePen = "false"; else closestTierSidePen = "correct";
PROC FREQ;
     TABLE CLOSESTTIERSIDEPEN*HEN /EXACT;

data v3a;
set series.confusion_corr;
if SameClosestTier = " " or SameClosestPerspective = " " then closestTierSide = 'false'; 
else closestTierSide = "true";
PROC FREQ;
     TABLE CLOSESTTIERSIDE*HEN /EXACT;

data v3b;
set series.confusion_corr;
if SameClosestTier = " " then closestTier = 'false'; 
else closestTier = "true";
PROC FREQ;
     TABLE CLOSESTTIER*HEN /EXACT;

/*data v3c;
set series.confusion_corr;
if SameClosestTier = " " or SameClosestPerspective = " " then closestTierSide = 'false'; 
else closestTierSide = "true";
if SameClosestZone ne " " AND SameClosestPen ne " " then antenna = "true"; else antenna = "false";
if DetectedInTier = "Detected in zone within 1 minute" then antenna1min = "1min"; else antenna1min = "not1min";
if antenna = 'true' or antenna1min = "1min" then cTier1min = 't';
if SameClosestTier = " " or SameClosestPen = " " then s = 'f'; 
if SameClosestPerspective = " " then s = 'f';
if s = 'f' then closestTierSidePen = "false"; else closestTierSidePen = "correct";
if closestTierSidePen = "correct" or closestTierSide = "true" then cTier1min = 't';
if closestTier = "true" then cTier1min = 't';
if cTier1min = 't' then clostestTier1min = 'true'; else clostestTier1min = 'false';

PROC FREQ;
     TABLE clostestTier1min*HEN /EXACT;
*/
data v4;
set series.confusion_corr;
if DetectedInTier = "Detected in tier+side within 1 minute" or DetectedInTier = 
"Detected in zone within 1 minute" then CLOSESTTIERSIDEPEN1min = "true"; else CLOSESTTIERSIDEPEN1min = "false";

PROC FREQ;
     TABLE CLOSESTTIERSIDEPEN1min*HEN /EXACT;

data v5;
set series.confusion_corr;
if SameClosestPen = " " then SameClosestPen = "false"; else SameClosestPen = "true";

PROC FREQ;
     TABLE SAMECLOSESTPEN*HEN /EXACT;

data v6;
set series.confusion_corr;
if SameClosestPerspective = " " then SameClosestPerspective = "false"; else SameClosestPerspective = "true";
PROC FREQ;
     TABLE SAMECLOSESTPERSPECTIVE*HEN /EXACT;
run;
