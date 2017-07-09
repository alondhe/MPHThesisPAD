IF OBJECT_ID('@scratchDatabaseSchema.@tablePrefix_pad_days', 'U') IS NOT NULL
	DROP TABLE @scratchDatabaseSchema.@tablePrefix_pad_days;

with cte_pad
as
(
  select 
    A.cohort_definition_id,
    A.subject_id,
    datediff(d, B.condition_start_date, A.cohort_start_date) as pad_days,
    row_number() over (PARTITION BY cohort_definition_id, subject_id order by subject_id, condition_start_date) as row_num
  from @cdmDatabaseSchema.cohort A
  join @cdmDatabaseSchema.condition_occurrence B on A.subject_id = B.person_id
    and B.condition_start_date < A.cohort_start_date
    and B.condition_concept_id in (@conceptIds)
  where A.cohort_definition_id in (@treatmentIds)
)
select distinct cohort_definition_id, subject_id, pad_days as score
into @scratchDatabaseSchema.@tablePrefix_pad_days
from cte_pad
where row_num = 1
;
