select distinct
  case
    when A.cohort_definition_id = @targetId then 1
    else 0
  end as treatment,
  A.subject_id,
  case
    when B.person_id is null then 0
    else 1
  end as score,
  case
    when C.subject_id is null then 0
    else 1
  end as after_match
from @cdmDatabaseSchema.cohort A
left join @cdmDatabaseSchema.@domainTable B on A.subject_id = B.person_id
  and B.@domainStartDate < A.cohort_start_date
  and B.@domainId_concept_id in (@conceptIds)
left join @scratchDatabaseSchema.@afterMatchingTable C on A.cohort_definition_id = C.cohort_definition_id
  and A.subject_id = C.subject_id
where A.cohort_definition_id in (@targetId, @comparatorId)
;
