IF OBJECT_ID('@scratchDatabaseSchema.@tablePrefix_negcontrols', 'U') IS NOT NULL
 DROP TABLE @scratchDatabaseSchema.@tablePrefix_negcontrols;

select C.ancestor_concept_id as cohort_definition_id,
       A.subject_id,
       B.condition_start_date as cohort_start_date,
       coalesce(B.condition_end_date, dateadd(d, 1, B.condition_start_date)) as cohort_end_date
into @scratchDatabaseSchema.@tablePrefix_negcontrols
from @cdmDatabaseSchema.cohort A
  join @cdmDatabaseSchema.condition_occurrence B on A.subject_id = B.person_id
  join @cdmDatabaseSchema.concept_ancestor C on B.condition_concept_id = C.descendant_concept_id
where 
  A.cohort_definition_id in (@treatmentIds)
  and B.condition_start_date >= A.cohort_start_date
  and C.ancestor_concept_id in (@negConceptIds)
;
