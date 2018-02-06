--Categories
--policyID,statecode,county,eq_site_limit,hu_site_limit
--119736,FL,CLAYCOUNTY,498960,498960
--fl_site_limit ,fr_site_limit,tiv_2011,tiv_2012
--498960,498960,498960,792148.9
--eq_site_deductible,hu_site_deductible,fl_site_deductible
--0,9979.2,0
--fr_site_deductible,point_latitude,point_long itude,line
--0,30.102261,-81.711777,Residential
--construction,point_granularity
--Masonry,1
.print ' '
.print 'Importing Data'

--Build the table framework for the data
CREATE TABLE "FL_insurance_sample" (
	policyID INTEGER,
	statecode CHAR,
	county CHAR,
	eq_site_limit REAL,
	hu_site_limit REAL,
	fl_site_limit REAL,
	fr_site_limit REAL,
	tiv_2011 REAL,
	tiv_2012 REAL,
	eq_site_deductible REAL,
	hu_site_deductible REAL,
	fl_site_deductible REAL,
	fr_site_deductible REAL,
	point_latitude REAL,
	point_long itude REAL,
	line CHAR,
	construction CHAR,
	point_granularity INTEGER
);

--Place SQL into CSV mode
.mode csv

--Import data into table
.separator ,
.import FL_insurance_sample.csv FL_insurance_sample

DELETE FROM FL_insurance_sample WHERE policyID = 'policyID';

--Print first ten rows
.print ' '
.print 'First 10 Rows of data'

SELECT * FROM FL_insurance_sample LIMIT 10;

--List all counties
.print ' '
.print 'Counties in survey'

SELECT DISTINCT county FROM FL_insurance_sample;

--Find average Property Apprecation
.print ' '
.print 'Average Property Apprecation'

SELECT AVG(tiv_2012) FROM FL_insurance_sample;
SELECT AVG(tiv_2011) FROM FL_insurance_sample;
SELECT AVG(tiv_2012-tiv_2011) FROM FL_insurance_sample;

--Create a Frequency table of Construction Materials

.print ' '
.print 'Construction Frequencies'

SELECT construction, COUNT(*) FROM FL_insurance_sample GROUP BY construction;


--Create Text File
.output FL_insurance.sqlite3
.dump
