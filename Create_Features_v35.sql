create table Features_temp(
	neighborhoodid varchar(200)
	, neighborhoodname varchar(200)
	, county varchar(200)
	, state varchar(200)
	, source varchar(200)
	, sorta varchar(200)
	, medianavmstate varchar(200)
	, medianavmcounty varchar(200)
	, medianavm varchar(200)
	, medianrent varchar(200)
	, medianincomenbhd varchar(200)
	, relativeincome varchar(200)
	, collegegrads varchar(200)
	, whitecollar varchar(200)
	, schools varchar(200)
	, unemployment varchar(200)
	, employmentdiversity varchar(200)
	, occupancy varchar(200)
	, safety varchar(200)
	, RN varchar(200)
);

copy Features_temp from '/home/DataScience/NQS/Features_20150721.csv'
 DELIMITER '|'  ENCLOSED BY '"' SKIP 1 REJECTMAX 0 ;


--Foreclosures NBHD
create table fc_nbhd as
select distinct nid, count(distinct(d.property_identifier)) as total_fc, 'neighborhood' as Source
from deed_history d
join property_maponics_mapping p on d.property_identifier = p.property_identifier
where extract(year from assessor_recording_date) >= 2013 
and (assessor_forclosure_code in ('P', 'Y') and assessor_pri_cat_code in ('A', 'B'))
or assessor_pri_cat_code = 'F'
group by p.nid;

create table total_nbhd as
select nid, count(distinct(property_identifier)) as total, 'neighborhood' as Source
from property_maponics_mapping
group by nid;

create table Foreclosures_nbhd as
select t.nid, total
, case when d.nid is null then 0
			 else total_fc/total
			 end as prop_fc
, t.Source
from fc_nbhd d
right join total_nbhd t on d.nid = t.nid;

--Foreclosures PLACE
create table fc_place as
select distinct plcidfp, count(distinct(d.property_identifier)) as total_fc, 'place' as Source
from deed_history d
join property_maponics_mapping p on d.property_identifier = p.property_identifier
where extract(year from assessor_recording_date) >= 2013 
and (assessor_forclosure_code in ('P', 'Y') and assessor_pri_cat_code in ('A', 'B'))
or assessor_pri_cat_code = 'F'
group by p.plcidfp;

create table total_place as
select plcidfp, count(distinct(property_identifier)) as total, 'place' as Source
from property_maponics_mapping
group by plcidfp;

create table Foreclosures_place as
select t.plcidfp, total
, case when d.plcidfp is null then 0
			 else total_fc/total
			 end as prop_fc
, t.Source
from fc_place d
right join total_place t on d.plcidfp = t.plcidfp;

--Foreclosures MCD
create table fc_mcd as
select distinct cosbidfp, count(distinct(d.property_identifier)) as total_fc, 'MCD' as Source
from deed_history d
join property_maponics_mapping p on d.property_identifier = p.property_identifier
where extract(year from assessor_recording_date) >= 2013 
and (assessor_forclosure_code in ('P', 'Y') and assessor_pri_cat_code in ('A', 'B'))
or assessor_pri_cat_code = 'F'
group by p.cosbidfp;

create table total_mcd as
select cosbidfp, count(distinct(property_identifier)) as total, 'mcd' as Source
from property_maponics_mapping
group by cosbidfp;

create table Foreclosures_mcd as
select t.cosbidfp, total
, case when d.cosbidfp is null then 0
			 else total_fc/total
			 end as prop_fc
, t.Source
from fc_mcd d
right join total_mcd t on d.cosbidfp = t.cosbidfp;

--- UNION FORECLOSURES
create table Foreclosures_prop as
select x.*
from(
select * from foreclosures_nbhd
union select * from foreclosures_place
union select * from foreclosures_mcd) x;

create table Features_20150803 as
select concat('id: ', fc.nid) as nid
, f.*
, fc.prop_fc as PropForeclosures
, fc.total as Nbhd_Size 
from Features_temp f
left join Foreclosures_prop fc on fc.nid = f.neighborhoodid and upper(f.source) = upper(fc.source)

create table Features_NQS_temp(
	neighborhoodid varchar(200)
	, neighborhoodname varchar(200)
	, county varchar(200)
	, state varchar(200)
	, source varchar(200)
	, medianavmstate varchar(200)
	, medianavmcounty varchar(200)
	, medianavm varchar(200)
	, medianrent varchar(200)
	, medianincomenbhd varchar(200)
	, relativeincome varchar(200)
	, collegegrads varchar(200)
	, whitecollar varchar(200)
	, schools varchar(200)
	, unemployment varchar(200)
	, employmentdiversity varchar(200)
	, occupancy varchar(200)
	, safety varchar(200)
	, RN varchar(200)
	, PropForeclosures varchar(200)
	, sqrtMedianAVM varchar(200)
	, NDS varchar(200)
);

copy Features_NQS_temp from '/home/DataScience/NQS/NeighborhoodsCombinedScore_v35_Intermediate.txt'
 DELIMITER '|' enclosed by '"' SKIP 1 REJECTMAX 0 ;
 
