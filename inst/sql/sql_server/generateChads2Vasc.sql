IF OBJECT_ID('tempdb..#CHADS2VASc_concepts', 'U') IS NOT NULL
  DROP TABLE #CHADS2VASc_concepts;

CREATE TABLE #CHADS2VASc_concepts (
  diag_category_id INT,
	concept_id INT
	);

IF OBJECT_ID('tempdb..#CHADS2VASc_scoring', 'U') IS NOT NULL
	DROP TABLE #CHADS2VASc_scoring;

CREATE TABLE #CHADS2VASc_scoring (
	diag_category_id INT,
	diag_category_name VARCHAR(255),
	weight INT
	);

-- C: Congestive heart failure
INSERT INTO #CHADS2VASc_scoring (diag_category_id,diag_category_name,weight)
VALUES (1,'Congestive heart failure',1);

INSERT INTO #CHADS2VASc_concepts (diag_category_id,concept_id)
SELECT 1, c.concept_id 
FROM (
  select distinct I.concept_id FROM
  ( 
    select DISTINCT concept_id from @cdmDatabaseSchema.CONCEPT where concept_id in (316139,314378,318773,321319) and invalid_reason is null
    UNION 
    select c.concept_id
    from @cdmDatabaseSchema.CONCEPT c
    join @cdmDatabaseSchema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
    and ca.ancestor_concept_id in (316139,314378)
    and c.invalid_reason is null
  
  ) I
) C
;

-- H: Hypertension
INSERT INTO #CHADS2VASc_scoring (diag_category_id,diag_category_name,weight)
VALUES (2,'Hypertension',1);

INSERT INTO #CHADS2VASc_concepts (diag_category_id,concept_id)
SELECT 2, c.concept_id 
FROM 
(
  select distinct I.concept_id FROM
  ( 
    select DISTINCT concept_id from @cdmDatabaseSchema.CONCEPT where concept_id in (320128,442604,201313) and invalid_reason is null
      UNION 
  
    select c.concept_id
    from @cdmDatabaseSchema.CONCEPT c
    join @cdmDatabaseSchema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
    and ca.ancestor_concept_id in (320128,442604,201313)
    and c.invalid_reason is null
  
  ) I
  LEFT JOIN
  (
    select concept_id from @cdmDatabaseSchema.CONCEPT where concept_id in (197930)and invalid_reason is null
      UNION 
  
    select c.concept_id
    from @cdmDatabaseSchema.CONCEPT c
    join @cdmDatabaseSchema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
    and ca.ancestor_concept_id in (197930)
    and c.invalid_reason is null
  
  ) E ON I.concept_id = E.concept_id
  WHERE E.concept_id is null
) C
;

-- A2: Age > 75
INSERT INTO #CHADS2VASc_scoring (diag_category_id,diag_category_name,weight)
VALUES (3,'Age>75',2);

--no codes

-- D: Diabetes
INSERT INTO #CHADS2VASc_scoring (diag_category_id,diag_category_name,weight)
VALUES (4,'Diabetes',1);

INSERT INTO #CHADS2VASc_concepts (diag_category_id,concept_id)
SELECT 4, c.concept_id 
FROM 
(
  select distinct I.concept_id FROM
  ( 
    select DISTINCT concept_id from @cdmDatabaseSchema.CONCEPT where concept_id in (201820,442793) and invalid_reason is null
      UNION 
  
    select c.concept_id
    from @cdmDatabaseSchema.CONCEPT c
    join @cdmDatabaseSchema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
    and ca.ancestor_concept_id in (201820,442793)
    and c.invalid_reason is null
  
  ) I
  LEFT JOIN
  (
    select concept_id from @cdmDatabaseSchema.CONCEPT where concept_id in (195771,376112,4174977,4058243,193323,376979)and invalid_reason is null
    UNION 
  
    select c.concept_id
    from @cdmDatabaseSchema.CONCEPT c
    join @cdmDatabaseSchema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
    and ca.ancestor_concept_id in (195771,376112,4174977,4058243,193323,376979)
    and c.invalid_reason is null
  
  ) E ON I.concept_id = E.concept_id
  WHERE E.concept_id is null
) C
;

-- S2: Stroke
INSERT INTO #CHADS2VASc_scoring (diag_category_id,diag_category_name,weight)
VALUES (5,'Stroke',2);

INSERT INTO #CHADS2VASc_concepts (diag_category_id,concept_id)
SELECT 5, c.concept_id 
FROM 
(
  select distinct I.concept_id FROM
  ( 
    select DISTINCT concept_id from @cdmDatabaseSchema.CONCEPT where concept_id in (4043731,4110192,375557,4108356,373503,434656,433505,376714,312337) and invalid_reason is null
      UNION 
  
    select c.concept_id
    from @cdmDatabaseSchema.CONCEPT c
    join @cdmDatabaseSchema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
    and ca.ancestor_concept_id in (4043731,4110192,375557,4108356,373503,434656,433505,376714,312337)
    and c.invalid_reason is null
  
  ) I
) C
;

-- V: Vascular disease (e.g. peripheral artery disease, myocardial infarction, aortic plaque)
INSERT INTO #CHADS2VASc_scoring (diag_category_id,diag_category_name,weight)
VALUES (6,'Vascular Disease', 1);

INSERT INTO #CHADS2VASc_concepts (diag_category_id,concept_id)
SELECT 6, c.concept_id FROM 
(
  select distinct I.concept_id 
  FROM
  ( 
    select DISTINCT concept_id from @cdmDatabaseSchema.CONCEPT where concept_id in (312327,43020432,314962,312939,315288,317309,134380,196438,200138,194393,319047,40486130,317003,4313767,321596,317305,321886,314659,321887,437312,134057) and invalid_reason is null
    
    UNION 

    select c.concept_id
    from @cdmDatabaseSchema.CONCEPT c
    join @cdmDatabaseSchema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
    and ca.ancestor_concept_id in (312327,43020432,314962,312939,315288,317309,134380,196438,200138,194393,319047,40486130,317003,4313767,321596)
    and c.invalid_reason is null
  
  ) I
) C
;

-- A: Age 65–74 years
INSERT INTO #CHADS2VASc_scoring (diag_category_id,diag_category_name,weight)
VALUES (7,'Age 65-74 Years', 1);

-- Sc: Sex category (i.e. female sex)
INSERT INTO #CHADS2VASc_scoring (diag_category_id,diag_category_name,weight)
VALUES (8,'Sex Category', 1);


IF OBJECT_ID('@scratchDatabaseSchema.@tablePrefix_@chads2Table', 'U') IS NOT NULL
	DROP TABLE @scratchDatabaseSchema.@tablePrefix_@chads2Table;

SELECT subject_id,
	cohort_definition_id,
	coalesce(SUM(weight), 0) AS score
  INTO @scratchDatabaseSchema.@tablePrefix_@chads2Table
FROM (
	SELECT DISTINCT cp1.subject_id,
	  cp1.cohort_definition_id,
		cs1.diag_category_id,
		cs1.weight
	FROM @cdmDatabaseSchema.cohort cp1
	INNER JOIN @cdmDatabaseSchema.condition_era ce1
		ON cp1.subject_id = ce1.person_id
	INNER JOIN #CHADS2VASc_concepts c1
		ON ce1.condition_concept_id = c1.concept_id
	INNER JOIN #CHADS2VASc_scoring cs1
		ON c1.diag_category_id = cs1.diag_category_id
	WHERE ce1.condition_era_start_date <= cp1.cohort_start_date
	and cp1.cohort_definition_id in (@cohortDefinitionIds)

  UNION

  SELECT DISTINCT cp1.subject_id,
    cp1.cohort_definition_id,
		3 as diag_category_id,
		2 as weight
	FROM @cdmDatabaseSchema.cohort cp1
  INNER JOIN @cdmDatabaseSchema.person p1 
    ON cp1.subject_id = p1.person_id
  WHERE year(cp1.cohort_start_date) - p1.year_of_birth >= 75
  and cp1.cohort_definition_id in (@cohortDefinitionIds)

  UNION

  SELECT DISTINCT cp1.subject_id,
    cp1.cohort_definition_id,
		7 as diag_category_id,
		1 as weight
	FROM @cdmDatabaseSchema.cohort cp1
  INNER JOIN @cdmDatabaseSchema.person p1 
    ON cp1.subject_id = p1.person_id
  WHERE year(cp1.cohort_start_date) - p1.year_of_birth between 65 and 74
  and cp1.cohort_definition_id in (@cohortDefinitionIds)

  UNION 

  SELECT DISTINCT cp1.subject_id,
    cp1.cohort_definition_id,
		8 as diag_category_id,
		1 as weight
  FROM @cdmDatabaseSchema.cohort cp1
  INNER JOIN @cdmDatabaseSchema.person p1 
    ON cp1.subject_id = p1.person_id
  WHERE p1.gender_concept_id = 8532 
  and cp1.cohort_definition_id in (@cohortDefinitionIds)
) t1
GROUP BY subject_id, cohort_definition_id;

TRUNCATE TABLE #CHADS2VASc_concepts;

DROP TABLE #CHADS2VASc_concepts;

TRUNCATE TABLE #CHADS2VASc_scoring;

DROP TABLE #CHADS2VASc_scoring;
