SELECT distinct concept_id FROM @cdmDatabaseSchema.concept_ancestor 
JOIN @cdmDatabaseSchema.concept ON descendant_concept_id = concept_id 
WHERE ancestor_concept_id in (@exposureAncestorConceptIds) AND standard_concept='S'