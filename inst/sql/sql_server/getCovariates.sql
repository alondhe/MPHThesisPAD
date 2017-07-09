{DEFAULT @categoryColumn = ''}
{DEFAULT @scoreColumn = ''}

select 
  case
    when A.cohort_definition_id = @targetId then 1
    else 0
  end as treatment,
  A.subject_id,
  {@categoryColumn != ''}?{A.@categoryColumn as category,}
  {@scoreColumn != ''}?{A.@scoreColumn as score,}
  case 
    when B.subject_id is not null then 1 
    else 0 
  end as after_match
from @scratchDatabaseSchema.@tablePrefix_@covariateTable A
left join @scratchDatabaseSchema.@afterMatchingTable B on A.cohort_definition_id = B.cohort_definition_id
  and A.subject_id = B.subject_id
where A.cohort_definition_id in (@targetId, @comparatorId);