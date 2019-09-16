# CREATE TABLE Attendance
 
# --- !Ups

CREATE SEQUENCE seq_attendance_id;
CREATE TABLE attendance (
    id integer NOT NULL DEFAULT nextval('seq_attendance_id'),
		att_cat varchar(1) NOT NULL,
		disp_cat varchar(1) NOT NULL,
		emp_name varchar(100) NOT NULL,
		emp_no varchar(5) NOT NULL,
		grp_name varchar(100) NOT NULL,
		handout_name varchar(100) NOT NULL,
		handout_sit_cat varchar(1) NOT NULL,
		pj_name varchar(100) NOT NULL,
		work_location varchar(100) NOT NULL,
		post_name varchar(100) NOT NULL,
		rcpt_name varchar(100) NOT NULL,
		rcpt_sit_cat varchar(1) NOT NULL,
		rem_col varchar(1000) NOT NULL,
		seq_no integer NOT NULL,
		time_stamp timestamp(3) NOT NULL,
		CONSTRAINT pk_attendance PRIMARY KEY (id)
);
 
# --- !Downs
 
DROP TABLE attendance;
DROP SEQUENCE seq_attendance_id;