IF OBJECT_ID('tempdb..#CHADS2_concepts', 'U') IS NOT NULL
  DROP TABLE #CHADS2_concepts;

CREATE TABLE #CHADS2_concepts (
	diag_category_id INT,
	concept_id INT
	);

IF OBJECT_ID('tempdb..#CHADS2_scoring', 'U') IS NOT NULL
	DROP TABLE #CHADS2_scoring;

CREATE TABLE #CHADS2_scoring (
	diag_category_id INT,
	diag_category_name VARCHAR(255),
	weight INT
	);

--Congestive heart failure
INSERT INTO #CHADS2_scoring (diag_category_id,diag_category_name,weight)
VALUES (1,'Congestive heart failure',1);

INSERT INTO #CHADS2_concepts (diag_category_id,concept_id)
SELECT 1, descendant_concept_id
FROM @cdmDatabaseSchema.concept_ancestor
WHERE ancestor_concept_id in (316139)
;

--Hypertension
INSERT INTO #CHADS2_scoring (diag_category_id,diag_category_name,weight)
VALUES (2,'Hypertension',1);

INSERT INTO #CHADS2_concepts (diag_category_id,concept_id)
SELECT 2, descendant_concept_id
FROM @cdmDatabaseSchema.concept_ancestor
WHERE ancestor_concept_id in (316866)
;

--Age > 75
INSERT INTO #CHADS2_scoring (diag_category_id,diag_category_name,weight)
VALUES (3,'Age>75',1);

--no codes

--Diabetes
INSERT INTO #CHADS2_scoring (diag_category_id,diag_category_name,weight)
VALUES (4,'Diabetes',1);

INSERT INTO #CHADS2_concepts (diag_category_id,concept_id)
SELECT 4, descendant_concept_id
FROM @cdmDatabaseSchema.concept_ancestor
WHERE ancestor_concept_id in (201820)
;

--Stroke
INSERT INTO #CHADS2_scoring (diag_category_id,diag_category_name,weight)
VALUES (5,'Stroke',2);

INSERT INTO #CHADS2_concepts (diag_category_id,concept_id)
SELECT 5, descendant_concept_id
FROM @cdmDatabaseSchema.concept_ancestor
WHERE ancestor_concept_id in (381591, 434056)
;

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
	left JOIN @cdmDatabaseSchema.condition_era ce1
		ON cp1.subject_id = ce1.person_id
	left JOIN #CHADS2_concepts c1
		ON ce1.condition_concept_id = c1.concept_id
	left JOIN #CHADS2_scoring cs1
		ON c1.diag_category_id = cs1.diag_category_id
	WHERE ce1.condition_era_start_date <= cp1.cohort_start_date
	and cp1.cohort_definition_id in (@cohortDefinitionIds)

  UNION

  SELECT DISTINCT cp1.subject_id,
    cp1.cohort_definition_id,
		3 as diag_category_id,
		1 as weight
	FROM @cdmDatabaseSchema.cohort cp1
  left JOIN @cdmDatabaseSchema.person p1
  ON cp1.subject_id = p1.person_id
  WHERE year(cp1.cohort_start_date) - p1.year_of_birth >= 75
  and cp1.cohort_definition_id in (@cohortDefinitionIds)
	) t1
GROUP BY subject_id, cohort_definition_id;

TRUNCATE TABLE #CHADS2_concepts;

DROP TABLE #CHADS2_concepts;

TRUNCATE TABLE #CHADS2_scoring;

DROP TABLE #CHADS2_scoring;

