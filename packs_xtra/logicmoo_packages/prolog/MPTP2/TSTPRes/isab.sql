-- sample SQL table creation for F_LT__E___0_999
-- CREATE TABLE `F_LT__E___0_999` (
-- `problem` VARCHAR( 255 ) NOT NULL ,
-- `result` CHAR( 3 ) NOT NULL ,
-- `output_status` CHAR( 3 ) NOT NULL ,
-- `time` FLOAT NOT NULL DEFAULT 700,
-- PRIMARY KEY ( `problem` ) ,
-- INDEX ( `result`) , 
-- INDEX (`output_status`) ,
-- INDEX (`time` )
-- ) TYPE = MYISAM ;

-- SQL table of all provers
CREATE TABLE `all` (
`problem` VARCHAR( 255 ) NOT NULL ,
`prover` VARCHAR( 255 ) NOT NULL ,
`result` CHAR( 3 ) NOT NULL ,
`output_status` CHAR( 3 ) NOT NULL ,
`time` FLOAT NOT NULL DEFAULT 700,
INDEX ( `problem` ) ,
INDEX ( `prover` ) ,
INDEX ( `result`) , 
INDEX (`output_status`) ,
INDEX (`time` )
) TYPE = MYISAM ;

-- create load file for all: 
-- for i in `cat 00Systems_isab`; do sed -e "s/^\([^\\t]\+\)\\t/\\1\\t$i\\t/" loadisab/"$i"_r1; done > loadisab/all_r1

LOAD DATA LOCAL INFILE 'all_r1' INTO TABLE `all` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';


-- sample loading of the table form a file
-- LOAD DATA LOCAL INFILE 'Bliksem___1_12_r1' INTO TABLE `Bliksem___1_12` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';

-- the rest of tables (generate this from the 00Systems_isab file):
-- sed -e 's/\(.*\)/CREATE TABLE `\1` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT\ 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;/' 00Systems_isab

CREATE TABLE `F_LT__E___0_999` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `F_LT__Fampire___8_1` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `F_LT__Fampire___9_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `F_LT__SPASS___2_2` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `F_LT__Vampire___8_1` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `F_LT__Vampire___9_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `F_MT__E___0_999` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `F_MT__Fampire___8_1` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `F_MT__Fampire___9_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `F_MT__SPASS___2_2` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `F_MT__Vampire___8_1` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `F_MT__Vampire___9_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `U_MT__E___0_999` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `U_MT__Fampire___8_1` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `U_MT__Fampire___9_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `U_MT__SPASS___2_2` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `U_MT__Vampire___8_1` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `U_MT__Vampire___9_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;


-- the rest of loading (generate this from the 00Systems_isab file):
-- sed -e "s/\(.*\)/LOAD DATA LOCAL INFILE \'\1_r1\' INTO TABLE \`\1\` FIELDS TERMINATED BY \'\\\t\' ENCLOSED BY \'\"\' ESCAPED BY \'\\\\\\\' LINES TERMINATED BY \'\\\n\';/" 00Systems_isab

LOAD DATA LOCAL INFILE 'F_LT__E___0_999_r1' INTO TABLE `F_LT__E___0_999` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'F_LT__Fampire___8_1_r1' INTO TABLE `F_LT__Fampire___8_1` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'F_LT__Fampire___9_0_r1' INTO TABLE `F_LT__Fampire___9_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'F_LT__SPASS___2_2_r1' INTO TABLE `F_LT__SPASS___2_2` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'F_LT__Vampire___8_1_r1' INTO TABLE `F_LT__Vampire___8_1` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'F_LT__Vampire___9_0_r1' INTO TABLE `F_LT__Vampire___9_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'F_MT__E___0_999_r1' INTO TABLE `F_MT__E___0_999` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'F_MT__Fampire___8_1_r1' INTO TABLE `F_MT__Fampire___8_1` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'F_MT__Fampire___9_0_r1' INTO TABLE `F_MT__Fampire___9_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'F_MT__SPASS___2_2_r1' INTO TABLE `F_MT__SPASS___2_2` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'F_MT__Vampire___8_1_r1' INTO TABLE `F_MT__Vampire___8_1` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'F_MT__Vampire___9_0_r1' INTO TABLE `F_MT__Vampire___9_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'U_MT__E___0_999_r1' INTO TABLE `U_MT__E___0_999` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'U_MT__Fampire___8_1_r1' INTO TABLE `U_MT__Fampire___8_1` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'U_MT__Fampire___9_0_r1' INTO TABLE `U_MT__Fampire___9_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'U_MT__SPASS___2_2_r1' INTO TABLE `U_MT__SPASS___2_2` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'U_MT__Vampire___8_1_r1' INTO TABLE `U_MT__Vampire___8_1` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'U_MT__Vampire___9_0_r1' INTO TABLE `U_MT__Vampire___9_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
