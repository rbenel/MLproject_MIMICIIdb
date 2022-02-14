WITH first_admission_time AS
(
  SELECT DISTINCT mimiciii.patients.subject_id,
      mimiciii.patients.dob, mimiciii.patients.gender, mimiciii.admissions.language, mimiciii.admissions.marital_status, mimiciii.admissions.ethnicity,mimiciii.admissions.insurance, mimiciii.icustays.los, mimiciii.icustays.icustay_id
      , EXTRACT(EPOCH FROM outtime - intime)/60.0/60.0/24.0 as icu_length_of_stay
      , MIN (mimiciii.admissions.admittime) AS first_admittime
      , MIN( ROUND( (cast(mimiciii.admissions.admittime as date) - cast(mimiciii.patients.dob as date)) / 365.242,2) )
          AS first_admit_age
  FROM mimiciii.patients
  INNER JOIN mimiciii.admissions
  ON mimiciii.patients.subject_id = mimiciii.admissions.subject_id
  INNER JOIN mimiciii.icustays
  on mimiciii.icustays.subject_id = mimiciii.admissions.subject_id
   WHERE mimiciii.admissions.language IS NOT NULL
  GROUP BY mimiciii.patients.subject_id, mimiciii.patients.dob, mimiciii.patients.gender, mimiciii.admissions.language, mimiciii.admissions.marital_status, mimiciii.admissions.ethnicity,mimiciii.admissions.insurance, mimiciii.icustays.los, mimiciii.icustays.icustay_id, mimiciii.icustays.outtime, mimiciii.icustays.intime
  ORDER BY mimiciii.patients.subject_id
)
SELECT
    subject_id, icustay_id, dob,  gender, language, marital_status, ethnicity, insurance, los, icu_length_of_stay
    , first_admittime, first_admit_age
    , CASE
        -- all ages > 89 in the database were replaced with 300
        WHEN first_admit_age > 89
            then '>89'
        WHEN first_admit_age >= 14
            THEN 'adult'
        WHEN first_admit_age <= 1
            THEN 'neonate'
        ELSE 'middle'
        END AS age_group
FROM first_admission_time
ORDER BY subject_id
