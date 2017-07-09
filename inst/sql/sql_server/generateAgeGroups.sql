IF OBJECT_ID('@scratchDatabaseSchema.@tablePrefix_cohort_agegroups', 'U') IS NOT NULL
	DROP TABLE @scratchDatabaseSchema.@tablePrefix_cohort_agegroups;

select 
  A.cohort_definition_id,
	A.subject_id,
	year(A.cohort_start_date) - B.year_of_birth as age_at_index,
	case
	  when year(A.cohort_start_date) - B.year_of_birth >= 0 
			and year(A.cohort_start_date) - B.year_of_birth <= 4 then 'Age Group 0-4'
	  when year(A.cohort_start_date) - B.year_of_birth >= 5 
			and year(A.cohort_start_date) - B.year_of_birth <= 9 then 'Age Group 5-9'
	  when year(A.cohort_start_date) - B.year_of_birth >= 10 
			and year(A.cohort_start_date) - B.year_of_birth <= 14 then 'Age Group 10-14'
		when year(A.cohort_start_date) - B.year_of_birth >= 15 
			and year(A.cohort_start_date) - B.year_of_birth <= 19 then 'Age Group 15-19'
		when year(A.cohort_start_date) - B.year_of_birth >= 20 
			and year(A.cohort_start_date) - B.year_of_birth <= 24 then 'Age Group 20-24'
		when year(A.cohort_start_date) - B.year_of_birth >= 25 
			and year(A.cohort_start_date) - B.year_of_birth <= 29 then 'Age Group 25-29'
		when year(A.cohort_start_date) - B.year_of_birth >= 30 
			and year(A.cohort_start_date) - B.year_of_birth <= 34 then 'Age Group 30-34'
		when year(A.cohort_start_date) - B.year_of_birth >= 35 
			and year(A.cohort_start_date) - B.year_of_birth <= 39 then 'Age Group 35-39'
		when year(A.cohort_start_date) - B.year_of_birth >= 40 
			and year(A.cohort_start_date) - B.year_of_birth <= 44 then 'Age Group 40-44'
		when year(A.cohort_start_date) - B.year_of_birth >= 45 
			and year(A.cohort_start_date) - B.year_of_birth <= 49 then 'Age Group 45-49'
		when year(A.cohort_start_date) - B.year_of_birth >= 50 
			and year(A.cohort_start_date) - B.year_of_birth <= 54 then 'Age Group 50-54'
		when year(A.cohort_start_date) - B.year_of_birth >= 55 
			and year(A.cohort_start_date) - B.year_of_birth <= 59 then 'Age Group 55-59'
		when year(A.cohort_start_date) - B.year_of_birth >= 60 
			and year(A.cohort_start_date) - B.year_of_birth <= 64 then 'Age Group 60-64'
		when year(A.cohort_start_date) - B.year_of_birth >= 65 
			and year(A.cohort_start_date) - B.year_of_birth <= 69 then 'Age Group 65-69'
		when year(A.cohort_start_date) - B.year_of_birth >= 70 
			and year(A.cohort_start_date) - B.year_of_birth <= 74 then 'Age Group 70-74'
		when year(A.cohort_start_date) - B.year_of_birth >= 75 
			and year(A.cohort_start_date) - B.year_of_birth <= 79 then 'Age Group 75-79'
		when year(A.cohort_start_date) - B.year_of_birth >= 80 
			and year(A.cohort_start_date) - B.year_of_birth <= 84 then 'Age Group 80-84'
		when year(A.cohort_start_date) - B.year_of_birth >= 85 
			and year(A.cohort_start_date) - B.year_of_birth <= 89 then 'Age Group 85-89'
		when year(A.cohort_start_date) - B.year_of_birth >= 90 
			and year(A.cohort_start_date) - B.year_of_birth <= 94 then 'Age Group 90-94'
		when year(A.cohort_start_date) - B.year_of_birth >= 95 
			and year(A.cohort_start_date) - B.year_of_birth <= 99 then 'Age Group 95-99'
		else 'Age Group 100+'
	end as age_group
into @scratchDatabaseSchema.@tablePrefix_cohort_agegroups
from @cdmDatabaseSchema.cohort A
join @cdmDatabaseSchema.person B on A.subject_id = B.person_id
where A.cohort_definition_id in (@cohortDefinitionIds)
group by A.cohort_definition_id, A.subject_id, A.cohort_start_date, B.year_of_birth
;