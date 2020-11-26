

-----**************************************************************************** -------
---- This file contains a couple of SQL commands usefull in order to convert the data from the IM2 format (where all fields are of type String)
---- into the Medslt required format 
-----**************************************************************************** -------

------- EXTRACT MEETINGS ---------------------------------------------------------------------

-- cast starttime format from 'text' into timestamp; cast duration format from text into number :
select meeting_id, starttime, to_timestamp(starttime, 'HH:MI') as starttime2, duration, (to_number(duration, '999999999.999999999') ) as duration_sec, description 
	from meetings ;

-- cast starttime format from 'text' into timestamp; cast duration format from text into number:
select meeting_id, type, date, institution, country, continent, 
       starttime, to_timestamp(starttime, 'HH:MI') as starttime_temp1, 
       duration, (to_number(duration, '999999999.999999999') ) as duration_sec, 
       description 
    into meetings_temp1 from meetings;

-- cast starttime format from timestamp into number (i.e. number of seconds): 
select meeting_id, type, date, institution, country, continent, 
	starttime, (3600*EXTRACT(HOUR from starttime_temp1) + 60* EXTRACT(MINUTES from starttime_temp1)) as starttime_sec, 
       duration, duration_sec, 
       description into meetings_temp2 from meetings_temp1;

drop table meetings_temp1;

-- compute the endtime (i.e. number of seconds) from starttime plus duration:
select meeting_id, type, date, institution, country, continent, 
	starttime, starttime_sec, 
       duration, (starttime_sec + duration_sec) as endtime_sec, 
	description into meetings_temp3 from meetings_temp2;

drop table meetings_temp2;

-- cast, i.e. Input  : both starttime and endtime fields having number format (number of seconds)
--            Output : separate fields for hour and minutes ( taking as input starttime and endtime )
---- Note: to_number(to_char(..) is necessary since otherwise we have float format and the mod operator % doesn't work
select meeting_id, type, date, institution, country, continent, 
       starttime,
       trunc( to_number(to_char( starttime_sec, '99999999'), '99999999') / 3600  ) as starttime_hour,
	trunc( (to_number(to_char(starttime_sec, '99999999'), '99999999') % 3600) / 60 ) as starttime_min,	
	duration,
	trunc( to_number(to_char( endtime_sec, '99999999'), '99999999') / 3600  ) as endtime_hour,
       trunc( (to_number(to_char(endtime_sec, '99999999'), '99999999') % 3600) / 60 ) as endtime_min,
	description 
	into meetings_temp4
	from meetings_temp3;

drop table meetings_temp3;

-- NEXT TWO SELECT COMMANDS are required IN ORDER TO print out the time in format HH:MM
-- Note: otherwise problems since printing for instance "10h02" as "10: 2" 
select meeting_id, type, date, institution, country, continent, 
       starttime, 
       starttime_hour,
       trunc(starttime_hour / 10) as starttime_hour1,
       (starttime_hour % 10) as starttime_hour2,
	starttime_min,
       trunc(starttime_min / 10) as starttime_min1,	
	(starttime_min % 10) as starttime_min2,	
	duration,
	endtime_hour, trunc(endtime_hour / 10) as endtime_hour1, (endtime_hour % 10) as endtime_hour2,
        endtime_min,trunc(endtime_min / 10) as endtime_min1, (endtime_min % 10) as endtime_min2,
	description 
	into meetings_temp5 from meetings_temp4;

drop table meetings_temp4;

select meeting_id, type, date, institution, country, continent, 
       starttime,
      ( trim(to_char( starttime_hour1, '9')) || trim(to_char( starttime_hour2, '9')) || ':' || trim(to_char(starttime_min1, '9')) || trim(to_char(starttime_min2, '9')) ) as starttime4medslt,
       duration,
      ( trim(to_char( endtime_hour1, '9')) || trim(to_char( endtime_hour2, '9')) || ':' || trim(to_char(endtime_min1, '9'))|| trim(to_char(endtime_min2, '9'))) as endtime4medslt,
       	description 
	into meetings_temp6
	from meetings_temp5;

drop table meetings_temp5;

-- EXTRACT DATA INTO A DATABASE HAVING THE PROLOG FORMAT REQUIRED BY THE MEDSLT APPLICATION
select 'meeting(' as tableName, meeting_id as ID, ', ' as virg1, EXTRACT(DAY FROM to_date(date, 'DD/MM/YYYY')) as Day, 
      ', ' as virg2, EXTRACT(MONTH FROM to_date(date, 'DD/MM/YYYY')) as Month, ', ' as virg3, 
      EXTRACT(YEAR FROM to_date(date, 'DD/MM/YYYY')) as Year, ', ' as virg4, 
      -- to_char(to_timestamp(starttime, 'HH:MI'), 'HH24:MI') as StartTime, 
      -- ', ' as virg5, 
      starttime4medslt, ', ' as virg6, 
      endtime4medslt, ', ' as virg7, 
      institution as LocID , '). ' as par2 
      into meetings_medslt
      from meetings_temp6 ;

drop table meetings_temp6;

-- update the field LocID 
UPDATE meetings_medslt
    SET LocID = 'Univ_Edinburgh_1000' 
    WHERE trim(LocID) = 'University of Edinburgh';

UPDATE meetings_medslt
    SET LocID = 'IDIAP_ISSCO_2000' 
    WHERE trim(LocID) = 'IDIAP/ISSCO';

UPDATE meetings_medslt
    SET LocID = 'TNO_3000' 
    WHERE trim(LocID) = 'TNO';

-- copy the table into a file
copy meetings_medslt to 'D:/work/MEDSLTrelated/medslt_db/meeting_medslt.txt';

-- drop the table
drop table meetings_medslt;

------------ EXTRACT LOCATION --------------------------------------------------------------

-- drop table locations;

select distinct 'location(' as tableName, institution as LocID, ', ' as virg1,
	        ('\'' || institution || '\'') as Name, ', ' as virg2,
		('\'' || country || '\'') as Country, ', ' as virg3, 
		'null' as City, ', ' as virg4,
		('\'' || institution || '\'' ) as Organization, ').' as par
		into locations
		from meetings;

UPDATE locations
    SET LocID = 'Univ_Edinburgh_1000' 
    WHERE trim(LocID) = 'University of Edinburgh';

UPDATE locations
    SET LocID = 'IDIAP_ISSCO_2000' 
    WHERE trim(LocID) = 'IDIAP/ISSCO';

UPDATE locations
    SET LocID = 'TNO_3000' 
    WHERE trim(LocID) = 'TNO';

-- select * from locations;
copy locations to 'D:/work/MEDSLTrelated/medslt_db/locations_medslt.txt';

drop table locations;

------------EXTRACT PERSON ----------------------------------------------------------------


-- drop table persons;

select distinct 'person(' as tableName, participant_id as PersID, ', ' as virg1,
		'null' as FirstName, ', ' as virg2,
		'null' as LastName, ', ' as virg3, 
		'null' as Affiliation, ', ' as virg4,
		'null' as Phone, ', ' as virg5,
		'null' as Email, ').' as par
		into persons
		from speakers;

-- copy persons to 'D:/work/MEDSLTrelated/medslt_db/persons.pl';

-- drop table persons;

-- select * from persons;

------------ EXTRACT ATTENDS -----------------------------------------------------------------
drop table attends;

select distinct 'attends(' as tableName, participant_id as PersID, ', ' as virg1,
		meeting_id as MeetingID, ').' as par
		into attends
		from participation 
		order by meeting_id;

-- copy attends to 'D:/work/MEDSLTrelated/medslt_db/attends_medslt.txt';

-- select * from attends;

---------------------------- CHANGE PERSONS IDs INTO "ATTENDS" TABLE AND INTO "PERSON" TABLE: ----------------------------
-- drop table person_attends;

select (lower( replace(b.firstname, '\'', '') ) || '_' || lower( replace(b.lastname, '\'', '') ) || '_' || lower( replace(a.persid, '\'', '') )) as persID, 
	a.meetingid as meetingID, lower(replace(b.firstname, '\'', '')) as firstname, lower(replace(b.lastname, '\'', '')) as lastname, b.affiliation, b.phone, b.email 
	into person_attends
	from attends a, persons2 b
	where a.persid = b.persid;

-------------------------------
drop table attends_changed_ids;

select distinct 'attends(' as tableName, persid, ',' as virg2 , meetingID , ').' as par
	into attends_changed_ids
	from person_attends;
-------------------------------
drop table persons_changed_ids;

SELECT DISTINCT 'person(' as tableName, persID, ',' as virg2,   
	firstname, ',' as virg4, lastname, ',' as virg5, affiliation, ',' as virg6, phone, ',' as virg7 , email , ').' as par
	into persons_changed_ids
	from person_attends;
-------------------------------

copy persons_changed_ids to 'D:/work/MEDSLTrelated/medslt_db/data/persons.pl';

-- select * from attends_changed_ids;
-- DROP TABLE attends_changed_ids;

copy attends_changed_ids to 'D:/work/MEDSLTrelated/medslt_db/data/attends.pl';

-- select * from persons_changed_ids;
-- DROP TABLE persons_changed_ids;

--------------------------------------------------------------------------


